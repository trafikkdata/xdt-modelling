
config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)
source("R/utilities.R")
source("R/data_preprocessing.R")

directed_traffic_links_json <- load_data(config$data_paths$raw$directed_traffic_links_json)
directed_traffic_links_geojson <- load_data(config$data_paths$raw$directed_traffic_links_geojson)
traffic_links_aadt_data <- load_data(config$data_paths$raw$aadt_results)
bus_counts <- load_data(config$data_paths$raw$bus_counts)
stops_on_traffic_links <- load_data(config$data_paths$raw$stops_on_traffic_links)

preprocessed_data <- preprocess_traffic_data(
  directed_traffic_links_json, 
  year = 2024,
  stops_on_traffic_links_data = stops_on_traffic_links, 
  bus_counts_data = bus_counts
  )

with_bus_counts <- add_busstop_counts(preprocessed_data, 
                   stops_on_traffic_links_data = stops_on_traffic_links, 
                   bus_counts_data = bus_counts, lowest_certainty = "medium")




