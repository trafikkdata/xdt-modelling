library(sf)
library(dplyr)
library(reticulate)
library(jsonlite)

source("R/utilities.R")
source("R/model_fitting.R")
source("R/model_validation.R")

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




# New approach -----------------------------------------------------------------
# Run the clustering
cluster_mapping <- cluster_road_network(data)

cluster_mapping <- final_assignments
# Merge with your original data
data_with_clusters <- data %>% left_join(cluster_mapping, by = "id")

# Check results
print(paste("Missing cluster assignments:", sum(is.na(data_with_clusters$cluster_id))))
print(paste("Duplicate assignments:", sum(duplicated(cluster_mapping$id))))

telemark_clusters <- data_with_clusters %>% filter(county == "Trøndelag") %>% add_geometry_to_traffic_links()

nvdb <- nvdb_objects()
pal <- leaflet::colorBin(
  palette = "viridis",
  domain = NULL, 
  reverse = TRUE,
  na.color = "#88807b"
)
leaflet::leaflet(telemark_clusters, 
                 options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    color = ~ pal(telemark_clusters[["cluster_id"]]),
    opacity = 1)

# Looking at results from Johannes ----
clustered <- read.csv("../../directed-traffic-links-2024-clustered.csv")
clustered_simple <- read.csv("../../directed-traffic-links-2024-clustered-simple.csv")


(number_of_groups <- clustered$cluster_id %>% unique() %>% length())
(traffic_links_per_group <- length(unique(clustered$id))/number_of_groups)
(number_of_groups_per_traffic_link <- clustered %>% group_by(id) %>% count() %>% arrange(desc(n)))



(number_of_groups <- clustered_simple$cluster_id %>% unique() %>% length())
(traffic_links_per_group <- length(unique(clustered_simple$id))/number_of_groups)
(number_of_groups_per_traffic_link <- clustered_simple %>% group_by(id) %>% count() %>% arrange(desc(n)))


