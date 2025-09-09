library(sf)
library(dplyr)
library(reticulate)
library(jsonlite)

#source("R/utilities.R")
#source("R/model_fitting.R")
#source("R/model_validation.R")

# Load the Python functions once
source_python("python/balancing/cluster_on_prelim_aadt.py")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Load data and matrices ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

data <- readRDS("data/processed/preprocessed_data.rds")
geojson_nodes <- fromJSON("data/raw/traffic-nodes-2024.geojson", simplifyVector = FALSE)

data$prelimAadt <- data$aadt

# Run Python via R + reticulate
cluster_data <- add_cluster_id_to_df(df = data, geojson_data = geojson_nodes)


# Get balancing clusters generated directly in Python 
balancing_clusters <- read.csv("data/processed/balancing_clusters.csv")


cluster_data <- full_join(data, balancing_clusters) %>% add_geometry_to_traffic_links()

nvdb <- nvdb_objects()

cluster1 <- filter(cluster_data, cluster_id == 100)

leaflet::leaflet(cluster1, options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    opacity = 1)


# Number of unique cluster id's:
length(unique(cluster_data$cluster_id))
# Max cluster label:
max(cluster_data$cluster_id, na.rm = TRUE)


# Finn en trafikklenke, plott alle klyngene som den trafikklenka tilhører
en_lenke <- "0.0-0.03281052@1801368-WITH"
lenke_grupper <- cluster_data %>% filter(id == en_lenke) %>% select(cluster_id) %>% unique() %>% st_drop_geometry() %>% unlist()

subset_lenke <- filter(cluster_data, cluster_id %in% lenke_grupper)


pal <- leaflet::colorBin(
  palette = "viridis",
  domain = NULL, 
  reverse = TRUE,
  na.color = "#88807b"
)

leaflet::leaflet(subset_lenke, options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    #color = ~ pal(subset_lenke[["cluster_id"]]),
    opacity = 1)



# Count number of groups per traffic link, and find the traffic link with the fewest groups


