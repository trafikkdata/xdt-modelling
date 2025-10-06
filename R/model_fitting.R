# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Main orchestration functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

run_modeling_pipeline <- function(
    data = NULL,
    inla_grouping_variable = NULL,
    inla_groups_to_process = "all",
    balancing_grouping_variable = inla_grouping_variable, # Can be a character vector, or a data frame containing the ID's and the group assignments
    #grouping_variable = "county",
    #groups_to_process = "Buskerud", # vector of group names or "all"
    covariates = c("functionalRoadClass"),
    #inla_scope = "local",  # "local" or "national"
    balance_predictions = TRUE,
    nodes_to_balance = "all", # One of "all", "all_except_i_intersections", "complete_nodes"
    generate_plots = TRUE,
    model_name = "model"
) {
  
  config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)
  
  if(is.null(data)){
    data <- readRDS("data/processed/engineered_data.rds")
  }
  aadt2024 <- load_data(config$data_paths$raw$aadt_results)
  nodes <- sf::read_sf("data/raw/traffic-nodes-2024.geojson")
  
  # Filter out groups to process
  if(inla_groups_to_process != "all"){
    data <- dplyr::filter(data, .data[[inla_grouping_variable]] %in% inla_groups_to_process)
  }
  
  diagnostics <- list()
  
  # Fit national model either all in one, or divided up by the grouping categories
  national_model <- fit_national_model(data, 
                                       covariates,
                                       inla_grouping_variable)
  
  data <- dplyr::full_join(data, national_model$inla_result)
  
  diagnostics$inla_summary <- national_model$model_summary
  
  cat("INLA model successfully fit.\n")
  
  # if(!balance_predictions){
  #   # Return comprehensive results
  #   return(list(data = data, 
  #               diagnostics = diagnostics))
  # }
  
  if(balance_predictions){
    # Balancing happens here
    balanced_predictions <- balance_predictions(data, nodes, 
                                                balancing_grouping_variable, 
                                                nodes_to_balance)
    
    data <- dplyr::full_join(data, balanced_predictions$balanced_res)
    
    diagnostics$balancing_diagnostics <- balanced_predictions$diagnostics
    
    cat("Predictions successfully balanced.\n")
  }
  
  diagnostics$approval <- calculate_approved(data = data,
                                             data_manual = aadt2024,
                                             model_name = model_name)
  
  # Return comprehensive results
  return(list(data = data, 
              diagnostics = diagnostics))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Data preparation functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

extract_group_data <- function(all_data, group_name, grouping_variable) {
  # Filter using the .data pronoun to reference the column name
  group_data <- dplyr::filter(all_data, .data[[grouping_variable]] == group_name)

  return(group_data)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# INLA model fitting functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fit_national_model <- function(data, covariates, inla_grouping_variable) {
  if (!is.null(inla_grouping_variable)) {
    # Fit model on group_data only
    cat("Fitting INLA model on all groups... ------------- \n")
    
    groups <- unique(data[[inla_grouping_variable]])
    
    group_data_list <- list()
    summary_list <- list()
    
    for(group in groups){
      cat("  Fitting model for group: ", group, "\n")
      # Extract group data
      group_data <- extract_group_data(data, group, 
                                       grouping_variable = inla_grouping_variable)
      
      # Fit model on group data
      results <- fit_group_model(group_data, covariates)
      
      # Extract results
      group_data$pred <- results$posterior_median
      group_data$sd <- results$posterior_sd
      group_data_list[[group]] <- dplyr::select(group_data, id, pred, sd)
      summary_list[[group]] <- results$model_summary
    }
    predictions <- dplyr::bind_rows(group_data_list, .id = "county")
    
    return(list(inla_result = predictions, model_summary = summary_list))
      
  } else if (is.null(inla_grouping_variable)) {
    # Fit model on national data, extract group predictions
    cat("Fitting national INLA model... ------------- \n")
    
    adjacency_matrix <- readRDS("data/processed/adjacency_matrix_2024.rds")
    
    results <- fit_group_model(data, covariates, adjacency_matrix)
    data$pred <- results$posterior_median
    data$sd <- results$posterior_sd
    
    predictions <- dplyr::select(data, id, pred, sd, county)
    
    return(list(inla_result = predictions, model_summary = results$model_summary))
  }
}

fit_group_model <- function(data, covariates, adjacency_matrix = NULL){
  # Create spatial index - this is simply the row number for each traffic link
  data$spatial.idx <- 1:nrow(data)
  
  if(is.null(adjacency_matrix)){
    adjacency_matrix <- build_adjacency_matrix(data) 
  }
  
  formula_base <- aadt ~ f(spatial.idx, model = "besagproper", 
                           graph = adjacency_matrix, 
      adjust.for.con.comp = FALSE, constr = TRUE) +
    f(roadSystem, model="iid")
  
  covariate_string <- paste(covariates, collapse = " + ")
  formula <- update(formula_base, as.formula(paste("~ . + ", covariate_string)))
  
  model <- INLA::inla(formula, 
                family = "poisson",
                data = data,
                control.predictor=list(link=1))
  
  return(list(
    model_summary = summary(model),
    posterior_median = round(model$summary.fitted.values[, "0.5quant"]),
    posterior_sd = round(model$summary.fitted.values[, "sd"])
    )
    )
}





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Balance predictions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

balance_predictions <- function(data, nodes, balancing_grouping_variable, 
                                nodes_to_balance){
  # Here we need to assign the balancing groups. 
  # When joining the balancing groups to the data, this will create duplicates.
  
  if(is.character(balancing_grouping_variable) &&
     length(balancing_grouping_variable) == 1 &&
     balancing_grouping_variable == "run_clustering"){
    balancing_grouping_variable <- strategic_network_clustering(data)
  }
  # "balancing_grouping_variable" can be either a column name (then no duplicates)
  # or a data frame with the group mapping. In the latter case, the cluster id column will be "cluster_id".
  if(is.data.frame(balancing_grouping_variable)){
    data <- dplyr::full_join(data, balancing_grouping_variable, 
                             by = join_by(parentTrafficLinkId == id),
                             relationship = "many-to-many")
    balancing_grouping_variable <- "cluster_id"
  }
  
  
  groups <- sort(unique(data[[balancing_grouping_variable]]))
  
  
  # Initialize result list
  group_data_list <- list()
  group_diagnostics <- list()
  
  cat("Balancing predictions for all groups... --------------\n")
  # Loop through groups calling fit_and_balance_for_group()
  for(group in groups){
    cat("  Balancing predictions for group: ", group, "\n")
    group_data <- extract_group_data(data, group, balancing_grouping_variable)
    
    balanced_group_results <- balance_group_predictions(
      data = group_data, 
      nodes = nodes, 
      pred = group_data$pred, 
      sd = group_data$sd,
      nodes_to_balance = nodes_to_balance)
    
    group_data$balanced_pred <- balanced_group_results$results$balanced_pred
    group_data$balanced_sd <- balanced_group_results$results$balanced_sd
    group_data_list[[group]] <- dplyr::select(group_data, id, 
                                              balanced_pred, balanced_sd)
    
    group_diagnostics[[group]] <- list(
      diagnostics = balanced_group_results$diagnostics,
      incidence_matrix = balanced_group_results$matrices$A1)
  }
  
  
  # Combine results across all groups
  predictions <- dplyr::bind_rows(group_data_list, .id = "grouping") %>% 
    group_by(id) %>% # Here we need to handle the possible duplicates.
    summarise(n_duplicates = n(), # Number of duplicates
              balanced_pred = mean(balanced_pred), # prediction is average of predictions
              balanced_sd = 1/n_duplicates*(sqrt(sum(balanced_sd^2))) # sd is 1/n_dup*sqrt(sum(sd^2))
    )
  
  
  return(list(balanced_res = predictions, 
              diagnostics = group_diagnostics))
}

balance_group_predictions <- function(data, nodes, model = NULL, pred = NULL, sd = NULL, 
                                      constraint_matrix = NULL,
                                      colname_aadt = "aadt", colname_sd = "aadt_sd", 
                                      lambda = 1e-10, nodes_to_balance){
  start_time <- Sys.time()
  one_node_flag <- FALSE   # Flagging if a group has only one node
  
  if(!is.null(model)){
    mu_v <- round(model$summary.fitted.values[, "0.5quant"])
    marginal_sds <- model$summary.fitted.values[, "sd"]
  } else {
    mu_v <- pred
    marginal_sds <- sd
  }
  
  
  # Step 0: Set up data and constraint matrices
  # Flow constraints
  cat("    Building incidence matrix...\n")
  if(is.null(constraint_matrix)){
    A1 <- build_incidence_matrix(nodes, data, 
                                 nodes_to_balance = nodes_to_balance)
  }else{
    A1 <- constraint_matrix
  }
  
  n_p <- sum(!is.na(data[[colname_aadt]])) # No. of AADT values
  n_e <- nrow(data) # No. of traffic links (edges)
  n_n <- nrow(A1) # No. of traffic nodes (with entering and exiting traffic)
  
  if(n_p == 0 & n_n == 0){ # No data and no nodes to balance, both A1 and A2 will be meaningless
    result_data <- data %>% 
      mutate(inla_pred = mu_v, 
             inla_sd = marginal_sds,
             balanced_pred = mu_v,
             balanced_sd = marginal_sds)
    
    end_time <- Sys.time()
    
    return(list(
      results = result_data,
      diagnostics = list(
        method_used = NA,
        rank_deficit = NA,
        condition_number = NA,
        n_measurements = n_p,
        n_links = n_e,
        n_constraints = n_n,
        underdetermined = NA,
        runtime = end_time - start_time
      ),
      matrices = list(A1 = NULL, A2 = NULL)
    ))
  }
  
  if(is.null(n_n)){ # This happens if there is only one row/node
    n_n <- 1
    one_node_flag <- TRUE
    cat("THIS GROUP HAS ONLY ONE NODE!\n")
  }
  
  cat("    Building measurement matrix...\n")
  if(n_p == 0){
    cat("This cluster has no measured points. \n")
  }else{
    A2 <- as.matrix(build_measurement_matrix(data, colname_aadt = colname_aadt))
    d <- na.omit(data[[colname_aadt]])
    Sigma_epsilon_mark <- diag(as.vector(na.omit(data[[colname_sd]])^2), 
                               nrow = length(na.omit(data[[colname_sd]])))
  }
  
  
  Sigma_v <- diag(as.vector(marginal_sds^2), nrow = length(marginal_sds))

  
  d <- if (n_p == 0) numeric(0) else d
  b <- c(rep(0, n_n), d)
  
  # Step 1: Handle extreme variances
  if (max(diag(Sigma_v)) > 1e10 || kappa(Sigma_v) > 1e12) {
    normal_vars <- min(diag(Sigma_v)[diag(Sigma_v) < 1e6], 1e10)
    max_reasonable <- max(normal_vars) * 10
    capped_count <- sum(diag(Sigma_v) > max_reasonable)
    diag(Sigma_v)[diag(Sigma_v) > max_reasonable] <- max_reasonable
    
    warning(paste("Capped", capped_count, "extreme variances"))
  }
  
  # Step 2: Convert to dense matrices
  A1 <- as.matrix(A1)
  if(one_node_flag){ # If only one node the matrix will be wrong
    A1 <- t(A1)
  }    
  A2 <- if (n_p == 0) matrix(numeric(0), nrow = 0, ncol = ncol(A1)) else A2
  
  A <- rbind(A1, A2)
  
  
  cat("    Creating Sigma_vb...\n")
  Sigma_vb <- Sigma_v %*% t(A)
  
  
  # Step 3: Check system properties
  rank_A1 <- qr(A1)$rank
  rank_A2 <- qr(A2)$rank  
  rank_A <- qr(A)$rank
  rank_deficit <- nrow(A) - rank_A
  
  # Step 4: Build covariance matrix
  Sigma_A1 <- matrix(0, nrow = n_n, ncol = n_n)
  
  Sigma_epsilon_mark <- if (n_p == 0) 
    matrix(numeric(0), 0, 0) else Sigma_epsilon_mark
  
  Sigma_epsilon <- as.matrix(Matrix::bdiag(
    list(Sigma_A1, Sigma_epsilon_mark)
  ))
  Sigma_b <- A %*% Sigma_v %*% t(A) + Sigma_epsilon
  Sigma_b <- (Sigma_b + t(Sigma_b))/2  # Ensure symmetry
  
  cat("    Inverting Sigma_b...\n")
  # Step 5: Robust inversion
  if (rank_deficit > 0) {
    # Use pseudoinverse for rank-deficient systems
    Sigma_b_inv <- MASS::ginv(Sigma_b)
    method <- "pseudoinverse"
  } else if (kappa(Sigma_b) > 1e12) {
    # Use regularization for ill-conditioned systems
    Sigma_b_reg <- Sigma_b + lambda * diag(nrow(Sigma_b))
    Sigma_b_inv <- solve(Sigma_b_reg)
    method <- "regularized"
  } else {
    # Standard inversion
    Sigma_b_inv <- solve(Sigma_b)
    method <- "standard"
  }
  
  # Step 6: Calculate posterior mean and variance
  mu_v_given_b <- mu_v + Sigma_vb %*% Sigma_b_inv %*% (b - A %*% mu_v)
  Sigma_v_given_b <- Sigma_v - Sigma_vb %*% Sigma_b_inv %*% t(Sigma_vb)
  
  # Some elements may be less than 0, set them to small number
  mu_v_given_b[mu_v_given_b <= 0] <- 1
  
  end_time <- Sys.time()
  
  # Step 7: Return results with diagnostics
  
  # Return the data frame with added columns for inla model and balanced results.
  result_data <- data %>% 
    mutate(inla_pred = mu_v, 
           inla_sd = marginal_sds,
           balanced_pred = mu_v_given_b,
           balanced_sd = diag(Sigma_v_given_b))
  
  return(list(
    results = result_data,
    diagnostics = list(
      method_used = method,
      rank_deficit = rank_deficit,
      condition_number = kappa(Sigma_b),
      n_measurements = nrow(A2),
      n_links = ncol(A),
      n_constraints = nrow(A1),
      underdetermined = ncol(A2) > nrow(A2),
      runtime = end_time - start_time
    ),
    matrices = list(A1 = A1, A2 = A2)
  ))
}

