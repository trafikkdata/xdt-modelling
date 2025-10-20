# Clustering

# Constructing the clusters to be used in balancing the predictions.
# The balancing is too computationally intensive to be run on all of Norway in 
# one, but it can be run on a smaller sub-area. 
# In this script we examine properties of the constructed clusters.

# Load functions
files.sources = list.files("R/", full.names = TRUE)
sapply(files.sources, source)

# Load packages
library(sf)
library(dplyr)
library(tidyverse)

# Data
data <- readRDS("data/processed/engineered_data.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Do clustering (or read in clusters) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

clustered <- strategic_network_clustering(data)
#saveRDS(clustered, "results/cluster_assignments.rds")

data_w_clusters <- dplyr::full_join(data, clustered, by = join_by(parentTrafficLinkId == id))
#saveRDS(data_w_clusters, "results/data_with_clusters.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Examine results ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

(number_of_groups <- clustered$cluster_id %>% unique() %>% length())
(traffic_links_per_group <- length(unique(clustered$id))/number_of_groups)
(number_of_groups_per_traffic_link <- clustered %>% group_by(id) %>% count() %>% arrange(desc(n)))
(number_of_traffic_links_per_group <- clustered %>% group_by(cluster_id) %>% count() %>% arrange(desc(n)))


one_cluster <- clustered %>% filter(cluster_id == 1) %>% add_geometry_to_traffic_links()

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

