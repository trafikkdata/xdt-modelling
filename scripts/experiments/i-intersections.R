# I-intersections

library(sf)
library(dplyr)
library(INLA)
library(tictoc)

nvdb <- nvdb_objects()

#config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/model_fitting.R")


nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")

nodes$number_of_traffic_links <- lengths(nodes$connectedTrafficLinkIds)
nodes$number_of_candidate_links <- lengths(nodes$connectedTrafficLinkCandidateIds)

two_in_two_out <- dplyr::filter(nodes, numberOfIncomingLinks == 2 & numberOfOutgoingLinks == 2)
two_parent_links <- dplyr::filter(nodes, numberOfUndirectedLinks == 2)
generalised_i_intersections <- dplyr::filter(nodes, number_of_candidate_links > number_of_traffic_links)

two_parent_links_and_two_in_two_out <- intersect(two_in_two_out$id, two_parent_links$id)
two_parent_links_and_more_candidates <- intersect(generalised_i_intersections$id, two_parent_links$id)
two_in_two_out_and_more_candidates <- intersect(two_in_two_out$id, generalised_i_intersections$id)

two_parent_links_and_two_in_two_out_and_more_candidates <- intersect(two_parent_links_and_two_in_two_out, generalised_i_intersections$id)

nrow(two_in_two_out)
nrow(two_parent_links)
nrow(generalised_i_intersections)

length(two_parent_links_and_two_in_two_out)
length(two_parent_links_and_more_candidates)
length(two_in_two_out_and_more_candidates)

length(two_parent_links_and_two_in_two_out_and_more_candidates)


# Load library
library(VennDiagram)

# Generate 3 sets of 200 words
set1 <- two_in_two_out$id
set2 <- two_parent_links$id
set3 <- generalised_i_intersections$id

# Chart
venn.diagram(
  x = list(set1, set2, set3),
  category.names = c("Set 1" , "Set 2 " , "Set 3"),
  filename = '#14_venn_diagramm.png',
  output=TRUE
)

library(eulerr)
euler(list(two_in_two_out = set1, two_parent_links = set2, more_candidates_than_links = set3, all_nodes = nodes$id)) %>% plot()
euler(list(two_in_two_out = set1, two_parent_links = set2, more_candidates_than_links = set3))

used_all_candidates <- dplyr::filter(two_parent_links, number_of_candidate_links == number_of_traffic_links)

leaflet::leaflet(used_all_candidates, options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addCircleMarkers()



nrow(generalised_i_intersections)/nrow(nodes)*100

nrow(used_all_candidates)/nrow(nodes)*100



# Ende-noder (en retta lenke inn og en retta lenke ut)
ende_noder <- dplyr::filter(nodes, numberOfIncomingLinks == 1 & numberOfOutgoingLinks == 1)

leaflet::leaflet(ende_noder, options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addCircleMarkers()



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Balancing or not balancing I-intersections ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

covariates <- c("functionalRoadClass:maxLanes",
                "minLanes:roadCategory",
                "functionalRoadClass",
                "maxLanes",
                "roadCategory")

tic()
balancing_all <- run_modeling_pipeline(
  data = data,
  balancing_grouping_variable = "run_clustering",
  covariates = covariates,
  nodes_to_balance = "all",
  model_name = "all"
)
toc()

tic()
balancing_all_except_i_intersections <- run_modeling_pipeline(
  data = data,
  balancing_grouping_variable = "run_clustering",
  covariates = covariates,
  nodes_to_balance = "all_except_i_intersections",
  model_name = "all_except_i_intersections"
)
toc()

tic()
balancing_complete_nodes <- run_modeling_pipeline(
  data = data,
  balancing_grouping_variable = "run_clustering",
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

rbind(balancing_all$diagnostics$approval$approved,
      balancing_all_except_i_intersections$diagnostics$approval$approved,
      balancing_complete_nodes$diagnostics$approval$approved,
      no_balancing$diagnostics$approval$approved)

plot_directed_links(df = balancing_complete_nodes$data, county = "Trøndelag")
