# Testing decomposing INLA predictions

library(sf)
library(dplyr)
library(INLA)

# Load functions
files.sources = list.files("R/", full.names = TRUE)
sapply(files.sources, source)

data <- readRDS("data/processed/engineered_data.rds")
aadt2024 <- read.csv("data/raw/traffic-links-aadt-data-2024.csv")
nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")


# Create spatial index - this is simply the row number for each traffic link
data$spatial.idx <- 1:nrow(data)

adjacency_matrix <- readRDS("data/processed/adjacency_matrix_2024.rds")

formula <- aadt ~ functionalRoadClass:maxLanes + 
  functionalRoadClass:roadCategory +
  minLanes:roadCategory + functionalRoadClass +
  maxLanes + roadCategory + 
  #hasOnlyPublicTransportLanes + #isFerryRoute + isNorwegianScenicRoute +
  functionalRoadClass:isRamp +
  f(spatial.idx, model = "besagproper", 
    graph = adjacency_matrix, 
    adjust.for.con.comp = FALSE, constr = TRUE) +
  f(roadSystem, model = "iid")  



model <- inla(formula, 
              family = "poisson",
              data = data,
              control.predictor=list(link=1))

summary(model)

data$pred <- model$summary.fitted.values$`0.5quant`
data$pred <- model$summary.fitted.values$mean

data$sd <- model$summary.fitted.values$sd

inla_res <- calculate_approval_metrics(
  model = model,
  data = data, 
  data_manual = aadt2024,
  model_name = "inla")

inla_res$summary



directed_data <- inla_res$directed_data %>% add_geometry_to_traffic_links()
plot_directed_links(directed_data, county_to_plot = "Trøndelag")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# INLA Prediction Decomposition ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

decompose_inla_predictions <- function(model) {
  
  # Extract the data used in fitting
  data <- model$.args$data
  n_obs <- nrow(data)
  
  # Get summaries
  linpred <- model$summary.linear.predictor
  fitted <- model$summary.fitted.values
  
  # Initialize storage for components
  fixed_contrib_log <- rep(0, n_obs)
  spatial_contrib_log <- rep(0, n_obs)
  roadSystem_contrib_log <- rep(0, n_obs)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 1. Fixed effects contribution ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Get fixed effects estimates
  fixed_effects <- model$summary.fixed
  
  # Start with intercept
  fixed_contrib_log <- rep(fixed_effects["(Intercept)", "mean"], n_obs)
  
  # Add contribution from each covariate
  # We need to reconstruct which coefficient applies to each observation
  
  # Get the model matrix (this shows which coefficients apply to each obs)
  # INLA doesn't store this directly, so we need to recreate it
  formula_terms <- delete.response(terms(model$.args$formula))
  
  # Create model matrix from the original data
  # Remove the f() terms first as they're not in the fixed effects
  fixed_formula <- update(formula_terms, ~ . - f(spatial.idx, model = "besagproper", 
                                                 graph = adjacency_matrix, 
                                                 adjust.for.con.comp = FALSE, 
                                                 constr = TRUE) - 
                            f(roadSystem, model = "iid"))
  
  X <- model.matrix(fixed_formula, data = data)
  
  # Check which rows have complete data
  complete_rows <- as.numeric(rownames(X))
  
  
  # Multiply model matrix by coefficient estimates
  # (excluding intercept since we already added it)
  beta <- fixed_effects$mean[-1]  # All coefficients except intercept
  fixed_contrib_log <- fixed_contrib_log + X[, -1] %*% beta
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 2. Spatial random effect ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  spatial_effects <- model$summary.random$spatial.idx
  spatial_contrib_log <- spatial_effects$mean[data$spatial.idx]
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 3. Road system random effect ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  roadSystem_effects <- model$summary.random$roadSystem
  
  # Map each observation to its road system effect
  # Need to match the roadSystem value to the random effect index
  road_system_levels <- roadSystem_effects$ID
  roadSystem_contrib_log <- roadSystem_effects$mean[match(data$roadSystem, road_system_levels)]
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 4. Calculate predictions on natural scale ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Sequential contributions
  pred_fixed_only <- exp(fixed_contrib_log)
  pred_with_roadSystem <- exp(fixed_contrib_log + roadSystem_contrib_log)
  pred_full <- exp(fixed_contrib_log + roadSystem_contrib_log + spatial_contrib_log)
  
  # Additive contributions on natural scale
  contrib_fixed <- pred_fixed_only
  contrib_roadSystem <- pred_with_roadSystem - pred_fixed_only
  contrib_spatial <- pred_full - pred_with_roadSystem
  
  # Calculate percentages
  pct_fixed <- 100 * contrib_fixed / pred_full
  pct_roadSystem <- 100 * contrib_roadSystem / pred_full
  pct_spatial <- 100 * contrib_spatial / pred_full
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 5. Get uncertainty from INLA ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Use fitted values SD (on natural scale) for reporting
  pred_sd <- rep(NA_real_, n_obs)
  pred_sd[complete_rows] <- fitted$sd
  
  # Also save the log-scale uncertainty for reference if needed
  pred_sd_log <- rep(NA_real_, n_obs)
  pred_sd_log[complete_rows] <- linpred$sd

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 6. Return decomposition dataframe ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  decomp_df <- data.frame(
    id = data$id,
    # Predictions
    inla_pred = pred_full,
    inla_sd = pred_sd,
    inla_sd_log = pred_sd_log,
    
    # Log-scale contributions
    fixed_log = fixed_contrib_log,
    roadSystem_log = roadSystem_contrib_log,
    spatial_log = spatial_contrib_log,
    
    # Natural-scale contributions
    contrib_fixed = contrib_fixed,
    contrib_roadSystem = contrib_roadSystem,
    contrib_spatial = contrib_spatial,
    
    # Percentages
    pct_fixed = round(pct_fixed, 1),
    pct_roadSystem = round(pct_roadSystem, 1),
    pct_spatial = round(pct_spatial, 1)
  )
  
  data_with_decomp <- full_join(data, decomp_df)
  
  return(data_with_decomp)
}


add_decomposition_popup <- function(decomp_df){
  decomp_df <- decomp_df %>% 
    mutate(pred_text = format(round(inla_pred), scientific = FALSE),
           sd_text = format(round(inla_sd), scientific = FALSE),
           text = sprintf("
    <div style='font-family: Arial, sans-serif; font-size: 12px;'>
      <b style='font-size: 14px;'>INLA prediction: </b> %s (±%s)<br>
      <b> Measured value: </b> %s <br>
      <hr style='margin: 8px 0;'>
      
      <b>Contribution Breakdown:</b><br>
      <div style='margin-left: 10px;'>
        ├─ Fixed effects: <b>%.1f%%</b><br>
        ├─ Road system: <b>%.1f%%</b><br>
        └─ Spatial neighbors: <b>%.1f%%</b>
      </div>
      <hr style='margin: 8px 0;'>
      
      <b>Key Covariates:</b><br>
      <div style='margin-left: 10px;'>
        • Functional class: %s<br>
        • Road category: %s<br>
        • Road system: %s<br>
        • Max lanes: %s<br>
        • Is ramp: %s
      </div>
    </div>
  ",
                           pred_text,
                           sd_text,
                           ifelse(is.na(aadt), "None", aadt),
                           pct_fixed,
                           pct_roadSystem,
                           pct_spatial,
                           functionalRoadClass,
                           roadCategory,
                           roadSystem,
                           maxLanes,
                           ifelse(isRamp == 1, "Yes", "No")
      )
      )
  return(decomp_df)
}

add_compact_decomposition_popup <- function(decomp_df){
  decomp_df <- decomp_df %>% 
    mutate(pred_text = format(round(inla_pred), scientific = FALSE),
           sd_text = format(round(inla_sd), scientific = FALSE),
           text = sprintf(
             "<b>INLA:</b> %s AADT (±%s) | <b>Measured value:<b> %s <br>
     <b>Breakdown:</b> Fixed %s%% | Road sys %s%% | Spatial %s%%<br>
     <b>Class:</b> %s | <b>Category:</b> %s | <b>Ramp:</b> %s",
             pred_text,
             sd_text,
             ifelse(is.na(aadt), "None", aadt),
             round(pct_fixed),
             round(pct_roadSystem),
             round(pct_spatial),
             functionalRoadClass,
             roadCategory,
             ifelse(isRamp == 1, "Yes", "No")
           ))
  return(decomp_df)
}


plot_decomp(model, "Oslo")
plot_decomp(model, "Trøndelag")