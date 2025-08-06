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

calculate_approved <- function(model = NULL, 
                               pred = round(model$summary.fitted.values[, "0.5quant"]), 
                               sd = round(model$summary.fitted.values[, "sd"]), 
                               data, data_manual, model_name = "inla"){
  
  # Extract predictions
  data$pred <- pred
  data$sd <- sd
  
  ## If NaN or above 20,000, cap the value to 20,000
  data$sd[is.na(data$sd) | data$sd > 20000] <- 20000
  
  sd_of_sum <- function(sd){
    return(sqrt(sum(sd^2)))
  }
  
  # Aggreger til uretta lenker
  data_uretta <- data %>% dplyr::select(parentTrafficLinkId, pred, sd) %>%
    group_by(parentTrafficLinkId) %>% 
    summarise(pred = sum(pred),
              sd = sd_of_sum(sd))
  
  uretta_med_manuell <- data_manual %>% 
    full_join(data_uretta, by = join_by(ID == parentTrafficLinkId)) %>% 
    mutate(ÅDT.offisiell = as.numeric(ÅDT.offisiell)) %>% 
    tidyr::drop_na(all_of(c("ÅDT.offisiell", "pred"))) %>% 
    mutate(approved = autoapprove(aadt = ÅDT.offisiell, 
                                  aadt_pred = pred, 
                                  aadt_pred_lower = pred - 1.96*sd,
                                  aadt_pred_upper = pred + 1.96*sd))
  
  lenker_uten_trp <- filter(uretta_med_manuell, Datagrunnlag.TRP.ID == "null")
  autogodkjente_lenker <- filter(lenker_uten_trp, approved)
  
  andel_lenker <- nrow(autogodkjente_lenker)/nrow(lenker_uten_trp)
  andel_km <- sum(autogodkjente_lenker$Strekningslengde..m.)/sum(lenker_uten_trp$Strekningslengde..m.)
  
  lenker_uten_trp$nedre_grense <- lenker_uten_trp$pred - 1.96*lenker_uten_trp$sd
  lenker_uten_trp$ovre_grense <- lenker_uten_trp$pred + 1.96*lenker_uten_trp$sd # Eller hent ut disse kvantilene direkte fra INLA
  
  lenker_uten_trp$dekket <- lenker_uten_trp$ÅDT.offisiell > lenker_uten_trp$nedre_grense & lenker_uten_trp$ÅDT.offisiell < lenker_uten_trp$ovre_grense
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

