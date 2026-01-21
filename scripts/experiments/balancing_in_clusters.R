# Balancing within clusters

# Verifying that the balancing works within the clusters as well as without the 
# clusters.
# The balancing is too computationally intensive to be run on all of Norway in 
# one, but it can be run on a smaller sub-area. In this demonstration we use 
# Trøndelag. We run the model for all of Trondelag in one, and for all of 
# Trøndelag in clusters, and compare the results.

# Load functions
files.sources = list.files("R/", full.names = TRUE)
sapply(files.sources, source)

# Load packages
library(sf)
library(dplyr)
library(ggplot2)

# Data
data <- readRDS("data/processed/engineered_data.rds")
aadt2024 <- read.csv("data/raw/traffic-links-aadt-data-2024.csv")
nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Run models with and without clusters ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# Formula to be used
covariates <- c("functionalRoadClass:maxLanes",
                "minLanes:roadCategory",
                "isRamp:functionalRoadClass",
                "isRamp",
                "functionalRoadClass",
                "maxLanes",
                "roadCategory",
                "hasOnlyPublicTransportLanes")

# Explicitly generating the clusters so we can examine them later
clustered_trond <- strategic_network_clustering(data %>% filter(county == "Trøndelag"))

trondelag_cluster <- run_modeling_pipeline(
  data = data,
  inla_grouping_variable = "county",
  inla_groups_to_process = "Trøndelag",
  balancing_grouping_variable = clustered_trond,
  covariates = covariates,
  nodes_to_balance = "complete_nodes",
  model_name = "balancing_in_clusters"
)

trondelag_in_one <- run_modeling_pipeline(
  data = data,
  inla_grouping_variable = "county",
  inla_groups_to_process = "Trøndelag",
  covariates = covariates,
  nodes_to_balance = "complete_nodes",
  model_name = "balancing_in_one"
)

saveRDS(list(trondelag_cluster = trondelag_cluster, 
             trondelag_in_one = trondelag_in_one), 
        "results/trondelag_cluster_or_not.rds")

saved_res <- readRDS("results/trondelag_cluster_or_not.rds")
trondelag_cluster <- saved_res$trondelag_cluster
trondelag_in_one <- saved_res$trondelag_in_one

# Total approval rates
rbind(trondelag_cluster$diagnostics$approval$approved,
      trondelag_in_one$diagnostics$approval$approved)

# Plot maps
plot_directed_links(trondelag_cluster$data)
plot_directed_links(trondelag_in_one$data)



# In principle, these two calls should produce roughly identical predictions.
kommunenavn <- read.csv("data/raw/kommunenummer.csv", sep = ";") %>% 
  mutate(kommunenummer = as.character(kommunenummer))

trondelag_comparison <- data.frame(
  id = trondelag_cluster$data$id,
  municipalityIds = trondelag_cluster$data$municipalityIds,
  aadt = trondelag_cluster$data$aadt,
  source = trondelag_cluster$data$bestDataSourceAadt_registrationFrequency,
  pred.cluster = trondelag_cluster$data$balanced_pred, 
  pred.in_one = trondelag_in_one$data$balanced_pred) %>% 
  mutate(difference = abs(pred.cluster - pred.in_one),
         ale = abs(log(pred.cluster) - log(pred.in_one)),
         municipalityIds = as.character(municipalityIds)) %>% 
  left_join(kommunenavn, join_by(municipalityIds == kommunenummer)) %>% 
  mutate(text = paste0("TRP ID: ", id,
                       "<br>pred.cluster: ", pred.cluster,
                       "<br>pred.in_one: ", pred.in_one,
                       "<br>Difference: ", difference,
                       "<br>Measured or derived: ", aadt,
                       "<br>Source: ", source)) %>% 
  add_geometry_to_traffic_links()

nvdb <- nvdb_objects()

pal <- leaflet::colorNumeric(
  palette = "viridis",
  reverse = TRUE,
  na.color = "#88807b",
  domain = trondelag_comparison$difference
)

leaflet::leaflet(trondelag_comparison,
                 options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) %>%
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution) %>%
  leaflet::addPolylines(
    color = ~ pal(difference),
    popup = ~ text,
    opacity = 1
  ) %>%
  leaflet::addLegend("bottomright",
                     pal = pal,
                     values = ~ difference,
                     opacity = 1)


ggplot(trondelag_comparison, aes(x = pred.in_one, y = pred.cluster)) +
  geom_point()

diff_per_county <- trondelag_comparison %>% group_by(kommunenavn) %>% 
  summarise(avg_diff = mean(difference), # Average absolute difference
            n = n(),
            percent_diff = sum(difference != 0)/n) # Percent of TLs that are different


# Approval aggregated by municipality
approved_trondelag_cluster <- get_approval_per_group(trondelag_cluster$diagnostics$approval$uretta, "Kommunenr")
approved_trondelag_in_one <- get_approval_per_group(trondelag_in_one$diagnostics$approval$uretta, "Kommunenr")
approved_trondelag <- full_join(approved_trondelag_cluster, 
                                approved_trondelag_in_one, 
                                by = join_by(Kommunenr, links_in_group, kommunenavn),
                                suffix = c("_cluster", "_in_one")) %>% 
  select(kommunenavn, links_in_group, fraction_approved_cluster, fraction_approved_in_one) %>% 
  mutate(difference = fraction_approved_cluster - fraction_approved_in_one)


# Looking closer at the municipalities with the biggest differences
tydal_cluster <- trondelag_cluster$data %>% filter(municipalityIds == 5033)
tydal_in_one <- trondelag_in_one$data %>% filter(municipalityIds == 5033)

problem_link <- "0.76569104@72834-0.48386792@72309-AGAINST"
prob_data <- filter(trondelag_cluster$data, id == problem_link)


# Look at A1 for problem link in cluster 149
cluster149 <- clustered_trond %>% filter(cluster_id == 149)

cluster149_data <- filter(trondelag_cluster$data, parentTrafficLinkId %in% cluster149$id)

A1 <- build_incidence_matrix(
  nodes = nodes, 
  traffic_links = cluster149_data, 
  nodes_to_balance = "complete_nodes")

plot_directed_links(cluster149_data)

# Look at A1 for problem link in cluster 269
cluster269 <- clustered_trond %>% filter(cluster_id == 269)

cluster269_data <- filter(trondelag_cluster$data, parentTrafficLinkId %in% cluster269$id)


plot_directed_links(cluster269_data)

