
config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/data_preprocessing.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load raw data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

directed_traffic_links_json <- load_data(config$data_paths$raw$directed_traffic_links_json)
directed_traffic_links_geojson <- load_data(config$data_paths$raw$directed_traffic_links_geojson)
#traffic_links_aadt_data <- load_data(config$data_paths$raw$aadt_results)
bus_counts <- load_data(config$data_paths$raw$bus_counts)
stops_on_traffic_links <- load_data(config$data_paths$raw$stops_on_traffic_links)


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
# Generate data with only id and geometry ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geometry_id <- directed_traffic_links_geojson %>% 
  dplyr::select(id, geometry)

saveRDS(geometry_id, "data/processed/traffic_link_geometries.rds")

