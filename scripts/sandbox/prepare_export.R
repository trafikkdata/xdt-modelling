latest_model <- readRDS("results/latest_model.rds")

model <- latest_model$data

latest_heavy_model <- readRDS("results/heavy_vehicle.rds")

heavy_model <- latest_heavy_model$directed_data

export_data <- model %>% 
  dplyr::select(
  directedTrafficLinkId = id, 
  estimatedAadt = balanced_pred, 
  estimatedAadtStandardDeviation = balanced_sd)

export_data$estimatedAadtHeavy <- heavy_model$pred
export_data$estimatedAadtHeavyStandardDeviation <- heavy_model$sd



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export data frame to JSON file ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(jsonlite)

# Write to file
write_json(export_data, "newest_aadt_model_results.json", 
           auto_unbox = TRUE, 
           pretty = FALSE)
