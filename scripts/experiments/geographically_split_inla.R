# Geographically split INLA



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Running the INLA model separately vs jointly for all of Norway ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

separate_model <- run_modeling_pipeline(groups_to_process = "all", 
                                        inla_scope = "local",
                                        covariates = good_formula_merged,
                                        balance_predictions = FALSE)
joint_model <- run_modeling_pipeline(groups_to_process = "all", 
                                     inla_scope = "national",
                                     covariates = good_formula_merged,
                                     balance_predictions = FALSE)

separate <- calculate_approved(data = separate_model$data,
                               pred = separate_model$data$pred,
                               sd = separate_model$data$sd,
                               data_manual = aadt2024,
                               model_name = "separate")
joint <- calculate_approved(data = joint_model$data,
                            pred = joint_model$data$pred,
                            sd = joint_model$data$sd,
                            data_manual = aadt2024,
                            model_name = "joint")
rbind(separate$approved, joint$approved)




covariates <- c("functionalRoadClass:maxLanes",
                "minLanes:roadCategory",
                "functionalRoadClass",
                "maxLanes",
                "roadCategory")

joint_model <- run_modeling_pipeline(inla_groups_to_process = "all", 
                                     covariates = covariates,
                                     balance_predictions = FALSE)

joint <- calculate_approved(data = joint_model$data,
                            pred = joint_model$data$pred,
                            sd = joint_model$data$sd,
                            data_manual = aadt2024,
                            model_name = "joint")

joint$approved