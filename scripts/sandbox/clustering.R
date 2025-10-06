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
#source_python("python/balancing/cluster_on_prelim_aadt.py")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Load data and matrices ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

data <- readRDS("data/processed/engineered_data.rds")
geojson_nodes <- fromJSON("data/raw/traffic-nodes-2024.geojson", simplifyVector = FALSE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Do clustering (or read in clusters) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# Results from Johannes ----
clustered_johannes <- read.csv("../../directed-traffic-links-2024-clustered.csv")


# Use "strategic_network_clustering", custom R function ----
# First make undirected data
# undirected <- data %>% distinct(parentTrafficLinkId, .keep_all = TRUE) %>% 
#   select(parentTrafficLinkId, startTrafficNodeId, endTrafficNodeId)
# 
# # Find parent traffic links where both children have data
# parent_links_with_data <- data %>% group_by(parentTrafficLinkId) %>% 
#   summarise(child_link_has_data = all(!is.na(aadt)))
# 
# undirected <- full_join(undirected, parent_links_with_data)

clustered <- strategic_network_clustering(data)
saveRDS(clustered, "results/cluster_assignments.rds")

data_w_clusters <- dplyr::full_join(data, clustered, by = join_by(parentTrafficLinkId == id))
saveRDS(data_w_clusters, "results/data_with_clusters.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Examine results ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

(number_of_groups <- clustered$cluster_id %>% unique() %>% length())
(traffic_links_per_group <- length(unique(clustered$id))/number_of_groups)
(number_of_groups_per_traffic_link <- clustered %>% group_by(id) %>% count() %>% arrange(desc(n)))
(number_of_traffic_links_per_group <- clustered %>% group_by(cluster_id) %>% count() %>% arrange(desc(n)))


one_cluster <- clustered %>% filter(cluster_id == 25) %>% add_geometry_to_traffic_links()

nvdb <- nvdb_objects()
leaflet::leaflet(one_cluster, 
                 options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    opacity = 1)

hist(number_of_traffic_links_per_group$n)

# Do all clusters have data?
no_data_clusters <- data_w_clusters %>% group_by(cluster_id) %>% summarise(num_links_with_data = sum(!is.na(aadt))) %>% arrange(num_links_with_data)

# No, this many clusters are missing data:
filter(no_data_clusters, num_links_with_data == 0) %>% nrow()

