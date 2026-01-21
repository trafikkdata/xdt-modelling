# I-intersections

# In this script we do two things:
# 1. Evaluate the number of intersections with different properties
#   - Two undirected traffic links
#   - Two incoming and outgoing directed traffic links
#   - More candidate links than traffic links
#   and different combinations of these.
# 2. Balance predictions with different sets of intersections for which to 
#   balance/not balance.

# Load functions
files.sources = list.files("R/", full.names = TRUE)
sapply(files.sources, source)

# Load packages
library(sf)
library(tidyverse)
library(tictoc)

# Data
data <- readRDS("data/processed/engineered_data.rds")
aadt2024 <- read.csv("data/raw/traffic-links-aadt-data-2024.csv")
nodes <- readRDS("data/processed/nodes.RDS")
old_nodes <- read_sf("data/raw/traffic-nodes-2024-old.geojson")

nvdb <- nvdb_objects()



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Identify number of nodes with different properties ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# Unnest to get one row per node-traffic link combination
# node_traffic_links <- nodes %>% st_drop_geometry %>% 
#   select(id, connectedTrafficLinkIds, roadSystems) %>%
#   unnest(connectedTrafficLinkIds)
# 
# # Join with data to get road systems from traffic links
# node_covered_systems <- node_traffic_links %>%
#   left_join(data %>% select(parentTrafficLinkId, roadSystem) %>% distinct(), 
#             by = c("connectedTrafficLinkIds" = "parentTrafficLinkId")) %>%
#   group_by(id) %>%
#   summarise(
#     roadSystems = list(first(roadSystems)),
#     n_roadSystems = length(first(roadSystems)),
#     trafficLinkRoadSystems = list(unique(roadSystem)),
#     n_trafficLinkRoadSystems = length(unique(roadSystem))
#   )
# 
# nodes <- nodes %>% left_join(node_covered_systems)
# 
# nodes$number_of_traffic_links <- lengths(nodes$connectedTrafficLinkIds)
# nodes$number_of_candidate_links <- lengths(nodes$connectedTrafficLinkCandidateIds)


# Nodes that have two incoming and two outgoing
two_in_two_out <- dplyr::filter(nodes, numberOfIncomingLinks == 2 & numberOfOutgoingLinks == 2)$id

# Nodes that have two parent links
two_parent_links <- dplyr::filter(nodes, numberOfUndirectedLinks == 2)$id

# Nodes that have more candidate links than traffic links
more_candidates <- dplyr::filter(nodes, number_of_candidate_links > number_of_traffic_links)$id

# Nodes that have more road systems than those of the traffic links
more_road_systems <- dplyr::filter(nodes, n_roadSystems > n_trafficLinkRoadSystems)$id


two_parent_links_and_two_in_two_out <- intersect(two_in_two_out, two_parent_links)
two_parent_links_and_more_candidates <- intersect(more_candidates, two_parent_links)
two_in_two_out_and_more_candidates <- intersect(two_in_two_out, more_candidates)

two_parent_links_and_two_in_two_out_and_more_candidates <- intersect(two_parent_links_and_two_in_two_out, more_candidates)

length(two_in_two_out)
length(two_parent_links)
length(more_candidates)

length(two_parent_links_and_two_in_two_out)
length(two_parent_links_and_more_candidates)
length(two_in_two_out_and_more_candidates)

length(two_parent_links_and_two_in_two_out_and_more_candidates)



library(eulerr)
euler(list(two_in_two_out = two_in_two_out, two_parent_links = two_parent_links, 
           more_candidates = more_candidates, more_road_systems = more_road_systems, 
           all_nodes = nodes$id)) %>% 
  plot(labels = FALSE,  # Remove labels from circles
       legend = list(side = "right", fontsize = 10),
       fills = list(fill = c("darkcyan", "#E41A1C", "#4DAF4A", "#984EA3", "grey90"),
                    alpha = 0.99))  # Transparency


euler(list(more_candidates = more_candidates,
           more_road_systems = more_road_systems, 
           all_nodes = nodes$id)) %>% 
  plot(labels = FALSE,  # Remove labels from circles
       legend = list(side = "right", fontsize = 10),
       fills = list(fill = c("#4DAF4A", "#984EA3", "grey90"),
                    alpha = 0.99))  # Transparency

setdiff(more_candidates, more_road_systems) %>% length()
setdiff(more_road_systems, more_candidates) %>% length()

euler(list(two_in_two_out = two_in_two_out, two_parent_links = two_parent_links, 
           all_nodes = nodes$id)) %>% 
  plot(labels = FALSE,  # Remove labels from circles
       legend = list(side = "right", fontsize = 10),
       fills = list(fill = c("darkcyan", "#E41A1C", "grey90"),
                    alpha = 0.99))  # Transparency

# Kartet viser steder hvor det er flere kandidater enn TL'er, men ikke flere vegsystem enn de representert av TL'ene.
# De vil karakteriseres som ubalanserbare noder, men ikke som I-kryss
# Kanskje det hadde vært nyttig å balansere på disse nodene?
leaflet::leaflet(filter(nodes, id %in% setdiff(more_candidates, more_road_systems)), 
                 options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addCircleMarkers()


# These are the final two sets that will not be balanced
i_intersections <- intersect(two_in_two_out, two_parent_links) %>% intersect(more_road_systems)
unbalancable_nodes <- more_candidates

length(i_intersections)
length(unbalancable_nodes)
union(i_intersections, unbalancable_nodes) %>% length()

euler(list(i_intersections = i_intersections,
           unbalancable_nodes = unbalancable_nodes, 
           all_nodes = nodes$id)) %>% 
  plot(labels = FALSE,  # Remove labels from circles
       legend = list(side = "right", fontsize = 10),
       fills = list(fill = c("hotpink", "darkblue", "grey90"),
                    alpha = 0.99))  # Transparency


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Create UpSet plot for node category overlaps ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(UpSetR)

# Create a binary matrix indicating membership in each category
all_node_ids <- nodes$id

upset_data <- data.frame(
  id = all_node_ids,
  two_in_two_out = all_node_ids %in% two_in_two_out,
  two_parent_links = all_node_ids %in% two_parent_links,
  more_candidates = all_node_ids %in% more_candidates,
  more_road_systems = all_node_ids %in% more_road_systems
) %>% 
  mutate(across(-id, as.integer))


# Create the UpSet plot
upset(upset_data, 
      sets = c("two_in_two_out", "two_parent_links", 
               "more_candidates", "more_road_systems"),
      order.by = "freq",
      keep.order = TRUE)



# Create pairwise overlap table
categories <- list(
  two_in_two_out = two_in_two_out,
  two_parent_links = two_parent_links,
  more_candidates = more_candidates,
  more_road_systems = more_road_systems
)

overlap_matrix <- matrix(NA, nrow = length(categories), ncol = length(categories))
rownames(overlap_matrix) <- colnames(overlap_matrix) <- names(categories)

for (i in 1:length(categories)) {
  for (j in 1:length(categories)) {
    overlap_matrix[i, j] <- length(intersect(categories[[i]], categories[[j]]))
  }
}

print(overlap_matrix)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Balancing or not balancing I-intersections ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

covariates <- c("functionalRoadClass:maxLanes",
                "minLanes:roadCategory",
                "functionalRoadClass:isRamp",
                "functionalRoadClass",
                "maxLanes",
                "roadCategory")

covariates <- c("functionalRoadClass:maxLanes",
                "minLanes:roadCategory",
                "functionalRoadClass",
                "functionalRoadClass:isRamp",
                "maxLanes",
                "roadCategory",
                "hasOnlyPublicTransportLanes",
                "log_last_year_aadt")

data$log_last_year_aadt <- log(data$lastYearAadt_trafficVolumeValue + 1)

clusters <- strategic_network_clustering(data)

tic()
balancing_all <- run_modeling_pipeline(
  data = data,
  balancing_grouping_variable = clusters,
  covariates = covariates,
  nodes_to_balance = "all",
  model_name = "all"
)
toc()

tic()
balancing_all_except_i_intersections <- run_modeling_pipeline(
  data = data,
  balancing_grouping_variable = clusters,
  covariates = covariates,
  nodes_to_balance = "all_except_i_intersections",
  model_name = "all_except_i_intersections"
)
toc()

tic()
balancing_complete_nodes <- run_modeling_pipeline(
  data = data,
  balancing_grouping_variable = clusters,
  covariates = covariates,
  nodes_to_balance = "complete_nodes",
  model_name = "complete_nodes"
)
toc()

tic()
no_balancing <- run_modeling_pipeline(
  data = data,
  covariates = covariates,
  balance_predictions = FALSE,
  model_name = "no_balancing"
)
toc()

print_approval_summary(balancing_all, 
                       balancing_all_except_i_intersections, 
                       balancing_complete_nodes, 
                       no_balancing)

rbind(balancing_all$diagnostics$approval$summary,
      balancing_all_except_i_intersections$diagnostics$approval$summary,
      balancing_complete_nodes$diagnostics$approval$summary,
      no_balancing$diagnostics$approval$summary)[, c(1:4, 8)]

plot_directed_links(df = balancing_complete_nodes$data, county = "Trøndelag")
plot_directed_links(df = balancing_complete_nodes$data, county = "Oslo")
plot_directed_links(df = balancing_complete_nodes$data, county = "Akershus")
plot_directed_links(df = balancing_complete_nodes$data, county = "Møre og Romsdal")
plot_directed_links(df = balancing_complete_nodes$data, county = "Troms")
plot_directed_links(df = balancing_complete_nodes$data, county = "Rogaland")
plot_directed_links(df = balancing_complete_nodes$data, county = "Nordland")

ggplot(balancing_complete_nodes$diagnostics$approval$undirected_data, 
       aes(x = as.numeric(ÅDT.offisiell), y = pred)) +
  geom_point() +
  #coord_equal() +
  #ylim(0, 100000) +
  theme_minimal()


undirected_results <- balancing_complete_nodes$diagnostics$approval$undirected_data
plot_eale(undirected_results, county_to_plot = "50")


extremes <- filter(balancing_complete_nodes$diagnostics$approval$undirected_data, pred > 100000)
plot_undirected_links(extremes)

