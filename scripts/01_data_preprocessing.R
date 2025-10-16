
config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/data_preprocessing.R")
source("R/feature_engineering.R")

library(dplyr)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load raw data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

directed_traffic_links_json <- load_data(config$data_paths$raw$directed_traffic_links_json)
directed_traffic_links_geojson <- load_data(config$data_paths$raw$directed_traffic_links_geojson)
#traffic_links_aadt_data <- load_data(config$data_paths$raw$aadt_results)
bus_counts <- load_data(config$data_paths$raw$bus_counts)
stops_on_traffic_links <- load_data(config$data_paths$raw$stops_on_traffic_links)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add bus counts to traffic links ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bus_counts_on_traffic_links <- connect_busstop_counts_to_traffic_links(
  stops_on_traffic_links_data = stops_on_traffic_links,
  bus_counts_data = bus_counts, 
  no_of_days = 366,
  location_uncertainties = c(0, 0.5, 1.5))

saveRDS(bus_counts_on_traffic_links, "data/processed/bus_counts_on_traffic_links.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preprocess directed traffic links (without geometry) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

preprocessed_data <- preprocess_traffic_data(
  raw_data = directed_traffic_links_json, 
  year = 2024,
  no_of_days = 366,
  stops_on_traffic_links_data = stops_on_traffic_links, 
  bus_counts_data = bus_counts
)

# Save the preprocessed data
saveRDS(preprocessed_data, "data/processed/preprocessed_data.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Engineer features ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

engineered_data <- preprocessed_data %>% engineer_features(scale_cols = NULL)

saveRDS(engineered_data, "data/processed/engineered_data.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate data with only id and geometry ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geometry_id <- directed_traffic_links_geojson %>% 
  dplyr::select(id, parentTrafficLinkId, geometry)

saveRDS(geometry_id, "data/processed/traffic_link_geometries.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate data with only id and road placement ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
roadPlacementsId <- directed_traffic_links_json %>% 
  dplyr::select(id, roadPlacements)

saveRDS(roadPlacementsId, "data/processed/traffic_link_roadPlacements.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create adjacency and constraint matrix ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# WARNING: This takes a while to run, maybe around 30 minutes.
source("R/model_fitting.R")

adj_sparse <- build_adjacency_matrix(preprocessed_data)
saveRDS(adj_sparse, "data/processed/adjacency_matrix_2024.rds")

constraint_matrix <- build_flow_constraints(preprocessed_data)
saveRDS(constraint_matrix, "data/processed/constraint_matrix_2024.rds")



