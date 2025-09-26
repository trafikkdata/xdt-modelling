library(sf)
library(dplyr)
library(reticulate)
library(jsonlite)
library(tidyverse)

source("R/utilities.R")
source("R/model_fitting.R")
source("R/model_validation.R")
source("R/balancing_clusters.R")

# Load the Python functions once
source_python("python/balancing/cluster_on_prelim_aadt.py")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Load data and matrices ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

data <- readRDS("data/processed/engineered_data.rds")
geojson_nodes <- fromJSON("data/raw/traffic-nodes-2024.geojson", simplifyVector = FALSE)

data$prelimAadt <- data$aadt


# First approach ---------------------------------------------------------------

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
clustered <- cluster_road_network(data)

#cluster_mapping <- final_assignments
# Merge with your original data
data_with_clusters <- data %>% left_join(clustered, by = join_by(parentTrafficLinkId == id))

# Check results
print(paste("Missing cluster assignments:", sum(is.na(data_with_clusters$cluster_id))))
print(paste("Duplicate assignments:", sum(duplicated(clustered$id))))

telemark_clusters <- data_with_clusters %>% dplyr::filter(county == "Telemark") %>% add_geometry_to_traffic_links()

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
(number_of_traffic_links_per_group <- clustered %>% group_by(cluster_id) %>% count() %>% arrange(desc(n)))



(number_of_groups <- clustered_simple$cluster_id %>% unique() %>% length())
(traffic_links_per_group <- length(unique(clustered_simple$id))/number_of_groups)
(number_of_groups_per_traffic_link <- clustered_simple %>% group_by(id) %>% count() %>% arrange(desc(n)))
(number_of_traffic_links_per_group <- clustered_simple %>% group_by(cluster_id) %>% count() %>% arrange(desc(n)))


one_cluster <- clustered %>% filter(cluster_id == 25) %>% add_geometry_to_traffic_links()

nvdb <- nvdb_objects()
leaflet::leaflet(one_cluster, 
                 options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    opacity = 1)

hist(number_of_traffic_links_per_group)

# Cluster 113 has no measured links
filter(clustered, cluster_id == 25)

# Do all clusters have data?
no_data_clusters <- clustered %>% group_by(cluster_id) %>% summarise(num_links_with_data = sum(!is.na(prelimAadt))) %>% arrange(num_links_with_data)

# No, this many clusters are missing data:
filter(no_data_clusters, num_links_with_data == 0) %>% nrow()


# Select traffic links that have only one group
traffic_links_with_one_group <- filter(number_of_groups_per_traffic_link, n == 1) %>% select(id)

groups <- clustered %>% filter(id %in% traffic_links_with_one_group$id) %>% add_geometry_to_traffic_links()

leaflet::leaflet(groups, 
                 options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    color = ~ pal(groups$cluster_id),
    opacity = 1)


# Another option ---------------------------------------------------------------

#undirected_traffic_links <- jsonlite::fromJSON("data/raw/traffic-links-2025.json")

new_undirected <- data %>% distinct(parentTrafficLinkId, .keep_all = TRUE) %>% 
  select(parentTrafficLinkId, startTrafficNodeId, endTrafficNodeId)

parent_links_with_data <- data %>% group_by(parentTrafficLinkId) %>% 
  summarise(child_link_has_data = all(!is.na(aadt)))


undirected <- full_join(new_undirected, parent_links_with_data)

sample <- undirected_traffic_links %>% 
  dplyr::select(parentTrafficLinkId, 
                startTrafficNodeId, 
                endTrafficNodeId, 
                child_link_has_data) %>% head(20)


clustered <- strategic_network_clustering(undirected)




# Check how the strategic sampling is working
measurement_links <- data$id[data$parent_link_has_data == TRUE & 
                               !is.na(data$startTrafficNodeId) & 
                               !is.na(data$endTrafficNodeId)]

print(paste("Clean measurement links:", length(measurement_links)))

# Test with more aggressive sampling parameters
test_barriers_conservative <- strategic_sample_barriers(network_graph, measurement_links, 
                                                        min_distance = 5, target_count = 200)
test_barriers_aggressive <- strategic_sample_barriers(network_graph, measurement_links, 
                                                      min_distance = 2, target_count = 1000)

print(paste("Conservative (dist=5, n=200):", length(test_barriers_conservative)))
print(paste("Aggressive (dist=2, n=1000):", length(test_barriers_aggressive)))



# Test actual network distances between some random measurement points
sample_measurements <- sample(measurement_links, min(10, length(measurement_links)))

if(length(sample_measurements) >= 2) {
  # Calculate distances between first measurement point and others
  distances_sample <- distances(network_graph, 
                                v = sample_measurements[1], 
                                to = sample_measurements[2:length(sample_measurements)])
  
  print("Sample distances between measurement points:")
  print(distances_sample[1,])
  print(paste("Mean distance:", round(mean(distances_sample[1,], na.rm = TRUE), 2)))
}



# Let's look at the actual many-to-many cases
node_to_links_clean <- data %>%
  filter(!is.na(startTrafficNodeId) & !is.na(endTrafficNodeId)) %>%
  select(id, startTrafficNodeId, endTrafficNodeId) %>%
  pivot_longer(cols = c(startTrafficNodeId, endTrafficNodeId), 
               names_to = "endpoint", 
               values_to = "traffic_node") %>%
  select(traffic_node, link_id = id)

# Find cases where same link appears multiple times for same node
link_node_duplicates <- node_to_links_clean %>%
  count(traffic_node, link_id) %>%
  filter(n > 1)

print("Links appearing multiple times for same node:")
print(head(link_node_duplicates))

# Also check: are there links that connect a node to itself?
self_loops <- data %>%
  filter(!is.na(startTrafficNodeId) & !is.na(endTrafficNodeId)) %>%
  filter(startTrafficNodeId == endTrafficNodeId) %>%
  select(id, startTrafficNodeId, endTrafficNodeId)

print("Self-loop links (same start and end node):")
print(nrow(self_loops))
print(head(self_loops))
