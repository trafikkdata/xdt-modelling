library(sf)
library(dplyr)
library(INLA)

config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/model_fitting.R")
source("R/model_validation.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Load data and matrices ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

data <- readRDS("data/processed/preprocessed_data.rds")
aadt2024 <- load_data(config$data_paths$raw$aadt_results)


data$aadt_without_bus <- data$aadt
data$aadt_without_bus[!is.na(data$bus_aadt)] <- NA

# Create spatial index - this is simply the row number for each traffic link
data$spatial.idx <- 1:nrow(data)

adj_sparse <- readRDS("data/processed/adjacency_matrix_2024.rds")
constraint_matrix <- readRDS("data/processed/constraint_matrix_2024.rds")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Define candidate models ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# Here I run the models and check the auto-approval percentages, but I should 
# run the actual cross-validation here.


## Model without bus data ----
formula_nobus <- aadt_without_bus ~ minLanes +
  f(spatial.idx, model = "besag", graph = adj_sparse, 
    adjust.for.con.comp = FALSE, scale.model = FALSE, constr = TRUE) +
  f(roadSystem, model="iid")

mod_nobus <- inla(formula_nobus, 
                  family = "poisson",
                  data = data,
                  control.predictor=list(link=1))

summary(mod_nobus)

## Model with bus data ----
formula_bus <- update(formula_nobus, aadt ~ . + hasOnlyPublicTransportLanes)
formula_bus

mod_bus <- inla(formula_bus, 
                family = "poisson",
                data = data,
                control.predictor=list(link=1),
                control.compute = list(dic = TRUE, waic = TRUE))
summary(mod_bus)

## Negative binomial model
mod_nb <- inla(formula_bus, 
                family = "nbinomial",
                data = data,
                control.predictor=list(link=1),
               control.compute = list(dic = TRUE, waic = TRUE))
summary(mod_nb)

mod_bus$dic$dic
mod_nb$dic$dic

hist(mod_bus$summary.fitted.values[, "0.5quant"])
hist(mod_nb$summary.fitted.values[, "0.5quant"])

data.frame(poisson = mod_bus$summary.fitted.values[, "0.5quant"],
           nb = mod_nb$summary.fitted.values[, "0.5quant"]) %>% 
  tidyr::pivot_longer(cols = c("poisson", "nb"), names_to = "model", values_to = "fitted_value") %>% 
  ggplot(aes(x = fitted_value, fill = model)) +
  geom_histogram(alpha = 0.5)


## Comparing auto-approval percentages ----
approved_nobus <- calculate_approved(model = mod_nobus, data = data, 
                                     data_manual = aadt2024,
                                     truth_name = "ÅDT.offisiell",
                                     model_name = "no_bus_data")

approved_bus <- calculate_approved(model = mod_bus, data = data, 
                                   data_manual = aadt2024,
                                   truth_name = "ÅDT.offisiell",
                                   model_name = "bus_data")

approved_nb <- calculate_approved(model = mod_nb, data = data, 
                                   data_manual = aadt2024,
                                   truth_name = "ÅDT.offisiell",
                                   model_name = "negative binomial")

rbind(approved_bus$approved, approved_nobus$approved, approved_nb$approved)
mod_poisson <- mod_bus







# COMPREHENSIVE MODEL COMPARISON ----
# Poisson | Negative Binomial | Gamma 


# Check for overdispersion in your original data
observed_var <- var(data$aadt, na.rm = TRUE)
observed_mean <- mean(data$aadt, na.rm = TRUE)
overdispersion_raw <- observed_var / observed_mean
print(paste("Raw overdispersion ratio:", round(overdispersion_raw, 2)))
print("If >> 1, negative binomial is likely better")

# Prepare dataset
data$aadt[data$aadt == 0] <- 1 # For Gamma to work

formula <- aadt ~ 
  f(spatial.idx, model = "besagproper", graph = adj_sparse,
    adjust.for.con.comp = FALSE, constr=TRUE) + 
  f(roadSystem, model = "iid") + 
  #f(county, model = "iid") +  
  functionalRoadClass:maxLanes +
  minLanes:roadCategory +
  functionalRoadClass:roadCategory +  
  functionalRoadClass +
  maxLanes +
  minLanes +
  roadCategory +
  hasOnlyPublicTransportLanes +
  isFerryRoute+
  isNorwegianScenicRoute

# 1. POISSON MODEL
model_poisson <- inla(formula, 
                      family = "poisson",
                      data = data,
                      control.predictor = list(link = 1),
                      control.compute = list(cpo = TRUE))

# 2. NEGATIVE BINOMIAL MODEL  
model_nb <- inla(formula, 
                 family = "nbinomial",
                 data = data,
                 control.predictor = list(link = 1),
                 control.compute = list(cpo = TRUE))

# 3. GAMMA MODEL (positive values only)
model_gamma <- inla(formula, 
                    family = "gamma",
                    data = data,
                    control.predictor = list(link = 1),
                    control.compute = list(cpo = TRUE))

# Performance comparison table
# Initialize results table
results <- data.frame(
  Model = c("Poisson", "Negative Binomial", "Gamma"),
  Dataset = c("Full", "Full", "Positive Only"),
  N_Obs = c(nrow(data_full), nrow(data_full), nrow(data_positive)),
  MAE = NA,
  MAPE = NA,
  Coverage = NA,
  Log_CPO = NA,
  Status = NA,
  stringsAsFactors = FALSE
)

# Helper function to calculate metrics
calculate_metrics <- function(model, dataset_name) {
  obs_ind <- which(!is.na(data$aadt))
  obs_data <- data$aadt[obs_ind]
  
  fitted_median <- model$summary.fitted.values$"0.5quant"[obs_ind]
  fitted_lower <- model$summary.fitted.values$"0.025quant"[obs_ind]
  fitted_upper <- model$summary.fitted.values$"0.975quant"[obs_ind]
  
  # Calculate metrics
  mae <- mean(abs(obs_data - fitted_median), na.rm = TRUE)
  mape <- mean(abs((obs_data - fitted_median) / obs_data) * 100, na.rm = TRUE)
  coverage <- mean(obs_data >= fitted_lower & obs_data <= fitted_upper, na.rm = TRUE)
  log_cpo <- sum(log(model$cpo$cpo), na.rm = TRUE) # Why log cpo?
  
  # Check for numerical issues
  mean_pred <- mean(model$summary.fitted.values$mean[obs_ind], na.rm = TRUE)
  infinite_sds <- sum(is.infinite(model$summary.fitted.values$sd))
  status <- ifelse(mean_pred < 100000 & infinite_sds == 0, "Good", "Issues")
  
  return(list(mae = mae, mape = mape, coverage = coverage, log_cpo = log_cpo, status = status))
}

# Calculate metrics for each model
models_list <- list(
  list(model = model_poisson, name = "Full"),
  list(model = model_nb, name = "Full"), 
  list(model = model_gamma, name = "Positive Only")
)

for(i in 1:3) {
  metrics <- calculate_metrics(models_list[[i]]$model, models_list[[i]]$name)
  results[i, "MAE"] <- round(metrics$mae, 2)
  results[i, "MAPE"] <- round(metrics$mape, 2) 
  results[i, "Coverage"] <- round(metrics$coverage * 100, 1)
  results[i, "Log_CPO"] <- round(metrics$log_cpo, 1)
  results[i, "Status"] <- metrics$status
}

# Print results table
print(results)


approved_poisson <- calculate_approved(model = model_poisson, data = data, 
                                  data_manual = aadt2024,
                                  truth_name = "ÅDT.offisiell",
                                  model_name = "Poisson")
approved_nb <- calculate_approved(model = model_nb, data = data, 
                                  data_manual = aadt2024,
                                  truth_name = "ÅDT.offisiell",
                                  model_name = "Negative binomial")
approved_gamma <- calculate_approved(model = model_gamma, data = data, 
                                  data_manual = aadt2024,
                                  truth_name = "ÅDT.offisiell",
                                  model_name = "Gamma")

rbind(approved_poisson$approved, approved_nb$approved, approved_gamma$approved)


# DIAGNOSTIC PLOTS 
# Create comprehensive diagnostic plots
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

# For each model, create 3 diagnostic plots
plot_diagnostics <- function(model, observed, model_name, dataset_type) {
  
  obs_ind <- which(!is.na(data$aadt))
  
  obs_data <- data$aadt[obs_ind]
  fitted_median <- model$summary.fitted.values$"0.5quant"[obs_ind]
  
  residuals <- obs_data - fitted_median
  
  # 1. Predicted vs Observed
  plot(obs_data, fitted_median, 
       main = paste(model_name, "- Pred vs Obs"),
       xlab = "Observed", ylab = "Predicted", 
       xlim = c(0, max(obs_data)), ylim = c(0, max(fitted_median)))
  abline(0, 1, col = "red", lwd = 2)
  
  # 2. Residuals vs Fitted
  plot(fitted_median, residuals,
       main = paste(model_name, "- Residuals"),
       xlab = "Fitted", ylab = "Residuals")
  abline(h = 0, col = "red")
  
  # 3. QQ Plot of residuals
  qqnorm(residuals, main = paste(model_name, "- QQ Plot"))
  qqline(residuals)
}

# Generate plots for all models
plot_diagnostics(model_poisson, data_full$aadt, "Poisson", "Full")
plot_diagnostics(model_nb, data_full$aadt, "Neg Binomial", "Full") 
plot_diagnostics(model_gamma, data_positive$aadt, "Gamma", "Positive Only")

# INTERPRETATION GUIDE
# METRICS:
# MAE: Lower is better (absolute prediction error)
# MAPE: Lower is better (relative prediction error)
# Coverage: Should be close to 95% (calibration)
# Log_CPO: Higher is better (predictive performance)

# MODEL CHARACTERISTICS:
# Poisson: For count data, assumes mean = variance
# Negative Binomial: For overdispersed count data
# Gamma: For positive continuous data with constant CV
# Log-Normal: For positive data with multiplicative errors


## OVERDISPERSION ANALYSIS ----

# UNDERSTANDING THE RESULTS
# Why Poisson might be winning despite overdispersion:
# 1. Your spatial/random effects are capturing much of the overdispersion
# 2. With strong covariates, remaining overdispersion may be minimal
# 3. NB adds complexity that may not be needed after accounting for spatial structure

# Let's verify this hypothesis
# CHECKING RESIDUAL OVERDISPERSION

# Fit models without spatial effects to see raw overdispersion
formula_no_spatial <- aadt ~ functionalRoadClass

model_pois_no_spatial <- inla(formula_no_spatial,
                              family = "poisson", 
                              data = data,
                              control.predictor = list(link = 1))

model_nb_no_spatial <- inla(formula_no_spatial,
                            family = "nbinomial",
                            data = data, 
                            control.predictor = list(link = 1))

# Calculate overdispersion ratios
obs_indices <- !is.na(data$aadt)
observed <- data$aadt[obs_indices]

# Without spatial effects
fitted_pois_no_spatial <- model_pois_no_spatial$summary.fitted.values$"0.5quant"[obs_indices]
pearson_resid_no_spatial <- (observed - fitted_pois_no_spatial) / sqrt(fitted_pois_no_spatial)
overdispersion_no_spatial <- sum(pearson_resid_no_spatial^2, na.rm=TRUE) / 
  (length(observed) - length(model_pois_no_spatial$summary.fixed$mean))

# With spatial effects (your full model)
fitted_pois_spatial <- model_poisson$summary.fitted.values$"0.5quant"[obs_indices]
pearson_resid_spatial <- (observed - fitted_pois_spatial) / sqrt(fitted_pois_spatial)
overdispersion_spatial <- sum(pearson_resid_spatial^2, na.rm=TRUE) / 
  (length(observed) - length(model_pois_compare$summary.fixed$mean))

print(paste("Overdispersion without spatial effects:", round(overdispersion_no_spatial, 3)))
print(paste("Overdispersion with spatial effects:", round(overdispersion_spatial, 3)))

# RECOMMENDATION LOGIC
if(overdispersion_spatial < 1.5 && overdispersion_no_spatial > 3) {
  print("✓ Spatial effects are capturing the overdispersion")
  print("✓ Poisson with spatial structure is adequate")
  recommendation <- "POISSON"
} else if(abs(mae_median - mae_pois) < 0.5 && nb_cpo > pois_cpo) {
  print("✓ Similar point predictions but NB has better likelihood")
  recommendation <- "NEGATIVE BINOMIAL"
} else if(mae_pois < mae_median && pois_cpo > nb_cpo) {
  print("✓ Poisson wins on both point predictions and likelihood")
  recommendation <- "POISSON"
} else {
  print("✓ Mixed results - go with theoretical appropriateness")
  recommendation <- "NEGATIVE BINOMIAL"
}
