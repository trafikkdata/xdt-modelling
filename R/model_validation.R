# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate approved ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

calculate_approved <- function(model = NULL, 
                               pred = round(model$summary.fitted.values[, "0.5quant"]), 
                               sd = round(model$summary.fitted.values[, "sd"]), 
                               data, data_manual, truth_name = "ÅDT.offisiell", model_name = "inla"){
  
  # Extract predictions
  data$pred <- pred
  data$sd <- sd
  
  # Check that truth column is numeric
  data_manual[[truth_name]] <- as.numeric(data_manual[[truth_name]])
  
  ## If NaN or above 20,000, cap the value to 20,000
  data$sd[is.na(data$sd) | data$sd > 20000] <- 20000
  
  sd_of_sum <- function(sd){
    return(sqrt(sum(sd^2)))
  }
  
  # Aggregate to undirected links
  data_uretta <- data %>% dplyr::select(parentTrafficLinkId, pred, sd) %>%
    group_by(parentTrafficLinkId) %>% 
    summarise(pred = sum(pred),
              sd = sd_of_sum(sd))
  
  uretta_med_manuell <- data_manual %>% 
    full_join(data_uretta, by = join_by(ID == parentTrafficLinkId)) %>% 
    mutate(!!truth_name := as.numeric(.data[[truth_name]])) %>% 
    tidyr::drop_na(all_of(c(truth_name, "pred"))) %>% 
    mutate(approved = autoapprove(aadt = .data[[truth_name]], 
                                  aadt_pred = pred, 
                                  aadt_pred_lower = pred - 1.96*sd,
                                  aadt_pred_upper = pred + 1.96*sd))
  
  lenker_uten_trp <- filter(uretta_med_manuell, Datagrunnlag.TRP.ID == "null")
  autogodkjente_lenker <- filter(lenker_uten_trp, approved)
  
  andel_lenker <- nrow(autogodkjente_lenker)/nrow(lenker_uten_trp)
  andel_km <- sum(autogodkjente_lenker$Strekningslengde..m.)/sum(lenker_uten_trp$Strekningslengde..m.)
  
  lenker_uten_trp$nedre_grense <- lenker_uten_trp$pred - 1.96*lenker_uten_trp$sd
  lenker_uten_trp$ovre_grense <- lenker_uten_trp$pred + 1.96*lenker_uten_trp$sd # Eller hent ut disse kvantilene direkte fra INLA
  
  lenker_uten_trp$dekket <- lenker_uten_trp[[truth_name]] > lenker_uten_trp$nedre_grense & lenker_uten_trp[[truth_name]] < lenker_uten_trp$ovre_grense
  dekningsandel <- sum(lenker_uten_trp$dekket)/nrow(lenker_uten_trp)
  
  approved <- data.frame(model = model_name, 
                         andel_lenker = andel_lenker, 
                         andel_km = andel_km,
                         dekningsandel = dekningsandel)
  
  colnames(data)[which(colnames(data) == "pred")] <- paste0(model_name, "_pred")
  colnames(data)[which(colnames(data) == "sd")] <- paste0(model_name, "_sd")
  
  colnames(uretta_med_manuell)[which(colnames(uretta_med_manuell) == "pred")] <- paste0(model_name, "_pred")
  colnames(uretta_med_manuell)[which(colnames(uretta_med_manuell) == "sd")] <- paste0(model_name, "_sd")
  
  
  return(list(approved = approved, retta = data, uretta = uretta_med_manuell))
}

autoapprove <- function(aadt, aadt_pred, aadt_pred_lower, aadt_pred_upper){
  #aadt_pred <- ROUND THIS ACCORDING TO RULES?
  threshold <- eale_threshold(aadt_pred)
  eALE <- exp(abs(log(aadt) - log(aadt_pred))) -1
  approved_eale <- eALE < threshold
  
  aadt_lower <- aadt - 1.96*estimate_standard_error(aadt)
  aadt_upper <- aadt + 1.96*estimate_standard_error(aadt)
  
  approved_lower_bound <- aadt_pred_lower < aadt_upper
  approved_upper_bound <- aadt_pred_upper > aadt_lower
  
  approved <- approved_eale & approved_lower_bound & approved_upper_bound
  # return(data.frame(aadt_lower = aadt_lower, aadt = aadt, aadt_upper = aadt_upper,
  #                  aadt_pred_lower = aadt_pred_lower, aadt_pred = aadt_pred, 
  #                  aadt_pred_upper = aadt_pred_upper,
  #                  approved_eale = approved_eale,
  #                  approved_lower_bound = approved_lower_bound,
  #                  approved_upper_bound = approved_upper_bound,
  #                  id = 1:length(aadt)))
  return(approved)
}

eale_threshold <- function(aadt_pred){
  # Threshold function for eALE for balanced AADT
  #  Args:
  #      aadt: BalancedAadt
  #  Returns: Maximum allowed value for eALE for given balanced AADT
  
  # Calculate the sigmoid value at 1000 outside the conditional branches
  sigmoid_value_at_1000 <- 2 - (1.6 / (1 + exp(-0.01 * (1000 - 500))))
  
  # Calculate value at 10000
  value_at_10000 <- sigmoid_value_at_1000 - 0.15 * ((10000 - 1000) / 9000)^0.5
  
  dplyr::case_when(
    # Sigmoid part: Start high and decrease to about 40% at 1000
    aadt_pred <= 1000  ~ 2 - (1.6 / (1 + exp(-0.01 * (aadt_pred - 500)))),
    # Transition curve from 1000 to 10000, aiming to reach around 25% at 10000
    aadt_pred <= 10000 ~ sigmoid_value_at_1000 - 0.15 * ((aadt_pred - 1000) / 9000)^0.5,
    # Custom decreasing function from 10000 to 50000, reaching 20% at 50000
    aadt_pred <= 50000 ~ value_at_10000 - 0.06 * ((aadt_pred - 10000) / 40000)^0.5,
    TRUE               ~ 0.20
  )
}

estimate_standard_error <- function(aadt) {
  dplyr::case_when(
    aadt < 500    ~ 40,
    aadt < 1000   ~ 100,
    aadt < 5000   ~ 200,
    aadt < 10000  ~ 400,
    aadt < 30000  ~ 1000,
    TRUE          ~ 2000
  )
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cross validate ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cross_validate <- function(data, n_folds, formula_list, graph){
  # model_list might be a list of functions that all take the data as input and 
  # return the model predictions as output, for example.
  scores_df <- data.frame(mae_cv = numeric(),
                          median_ae_cv = numeric(),
                          rmse_cv = numeric(),
                          exp_male_cv = numeric(),
                          median_sd_cv = numeric(), 
                          model = character(),
                          fold = numeric())
  data_with_response <- filter(data, !is.na(prelimAadt))
  n_obs <- nrow(data_with_response)
  
  # Set seed to ensure the same folds each run
  set.seed(1)
  
  # This divides the observations as evenly as possible across the folds, 
  # with no more than 1 in difference between the folds.
  # `assignments` is a vector of length n_obs. 
  assignments <- sample(rep(seq_len(n_folds), length.out = n_obs))
  table(assignments)
  
  # Add folds to the data
  data_with_response$fold <- assignments
  
  for(i in 1:n_folds){
    # Training data: n_folds-1 folds
    # Testing data: 1 fold
    
    # We make a new response column that has NA for the testing data:
    data_with_response$response <- ifelse(data_with_response$fold == i, 
                                          NA,
                                          data_with_response$prelimAadt)
    # Remember that this column is overwritten for each fold.
    
    # Now fit the models using 'response' as the response.
    model_results <- lapply(formula_list, model_template, data = data_with_response)
    
    # Calculate scores for this fold
    scores_list <- lapply(model_results, calculate_scores, data_with_response)
    scores_curr <- bind_rows(scores_list, .id = "model")
    scores_curr$fold <- i
    scores_df <- bind_rows(scores_df, scores_curr)
  }
  # Calculate the average scores for each model candidate: 
  scores_across_folds <- scores_df %>% 
    group_by(model) %>% 
    summarise_all(mean)
  
  return(scores_across_folds)
}


calculate_scores <- function(model, data){
  pred_medians <- round(model$summary.fitted.values[, "0.5quant"])
  pred_sd <- round(model$summary.fitted.values[, "sd"])
  
  validation_indices <- is.na(data$response) 
  true_values <- data$prelimAadt[validation_indices]
  predicted_values <- pred_medians[validation_indices]
  
  mae_cv <- mean(abs(true_values - predicted_values))
  median_ae_cv <- median(abs(true_values - predicted_values))
  rmse_cv <- sqrt(mean((true_values - predicted_values)^2))
  
  absolute_log_errors <- abs(log1p(true_values/predicted_values))
  exp_male_cv <- exp(mean(absolute_log_errors))
  median_sd_cv <- median(pred_sd)
  
  rmsre_cv <- sqrt(mean(((true_values - predicted_values)/true_values)^2))
  mare_cv <- mean(abs(true_values-predicted_values)/true_values)
  mrpd_cv <- mean(abs(true_values-predicted_values)/((abs(true_values)+abs(predicted_values))/2))
  
  # OBS: Here we calculate the coverage probability based on the credibility interval,
  # not the standard deviation. Might reconsider this.
  # Should not matter much if the distributions for the fitted values are symmetric.
  cr95_low <- model$summary.fitted.values[validation_indices, "0.025quant"]
  cr95_high <- model$summary.fitted.values[validation_indices, "0.975quant"]
  true_within <- sum(true_values >= cr95_low & true_values <= cr95_high)/length(true_values)
  
  return(data.frame(mae_cv = mae_cv,
                    median_ae_cv = median_ae_cv,
                    rmse_cv = rmse_cv,
                    exp_male_cv = exp_male_cv,
                    median_sd_cv = median_sd_cv,
                    rmsre_cv = rmsre_cv,
                    mare_cv = mare_cv,
                    mrpd_cv = mrpd_cv,
                    true_within = true_within))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Examine node flow ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

examine_node_flow <- function(A1, node_id){
  rownames_contain_string <- which(grepl(node_id, rownames(A1)))
  node_rows <- A1[rownames_contain_string, ]
  
  if(nrow(nonzero_cols) == 1){
    nonzero_cols <- node_rows[node_rows != 0, drop = FALSE]
  }
  nonzero_cols <- node_rows[, colSums(abs(node_rows) != 0) > 0, drop = FALSE]
  
  return(nonzero_cols)
}

get_turning_movements <- function(nodes, node_id){
  node_row <- filter(nodes, id == node_id)
  
  turning_movements_df <- make_turning_movements_df(
    turning_movements_json = node_row$legalTurningMovements, node_id = node_id)
  
  return(turning_movements_df)
}

make_turning_movements_df <- function(turning_movements_json, node_id){
  
  # Clean and parse JSON
  movements <- jsonlite::fromJSON(turning_movements_json, simplifyVector = FALSE)
  
  # Convert to data frame for easier processing
  movements_df <- data.frame(
    incoming = character(length(movements)),
    outgoing = I(vector("list", length(movements))),  # Use I() to keep as list column
    stringsAsFactors = FALSE
  )
  
  for(i in seq_along(movements)) {
    movement <- movements[[i]]
    movements_df$incoming[i] <- movement$incomingId
    movements_df$outgoing[[i]] <- unlist(movement$outgoingIds)
  }
  
  return(movements_df)
}

print_turning_movements_for_link_at_node <- function(node_id, link_id, nodes){
  turns_df <- get_turning_movements(nodes = nodes, node_id = node_id)
  incoming <- turns_df[, 1]
  if(!link_id %in% incoming){
    cat("Traffic link", link_id, "is not an incoming link to node", node_id, ".")
  }else{
    cat("Legal turning movements for traffic link", link_id, "at traffic node", node_id, ": \n")
    print(turns_df[turns_df[,1] == link_id, 2][[1]])
  }
}
