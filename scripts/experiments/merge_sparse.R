# Merging sparse covariate categories

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Merged vs not merged sparse covariate categories ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

good_formula <- c("functionalRoadClass:maxLanes", 
                  "minLanes:roadCategory", 
                  #"functionalRoadClass:roadCategory", 
                  "functionalRoadClass", 
                  "maxLanes", 
                  "minLanes", 
                  "roadCategory", 
                  "hasOnlyPublicTransportLanes", 
                  "isFerryRoute", 
                  "isNorwegianScenicRoute")

good_formula_merged <- c("functionalRoadClass_merged:maxLanes_merged", 
                         "minLanes_merged:roadCategory", 
                         #"functionalRoadClass_merged:roadCategory", 
                         "functionalRoadClass_merged", 
                         "maxLanes_merged", 
                         "minLanes_merged", 
                         "roadCategory", 
                         "hasOnlyPublicTransportLanes", 
                         "isFerryRoute", 
                         "isNorwegianScenicRoute")

merged_covariates <- run_modeling_pipeline(groups_to_process = "Trøndelag", 
                                           covariates = good_formula_merged)
original_covariates <- run_modeling_pipeline(groups_to_process = "Trøndelag",
                                             covariates = good_formula)

merged_approved <- calculate_approved(data = merged_covariates$data,
                                      data_manual = aadt2024,
                                      model_name = "merged_covariates")
original_approved <- calculate_approved(data = original_covariates$data,
                                        data_manual = aadt2024,
                                        model_name = "original_covariates")
rbind(merged_approved$approved, original_approved$approved)

