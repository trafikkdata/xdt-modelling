# Balancing within clusters

# Verifying that the balancing works within the clusters as well as without the 
# clusters.
# The balancing is too computationally intensive to be run on all of Norway in 
# one, but it can be run on a larger sub-area. In this demonstration I use 
# Trøndelag. I run the model for all of Trondelag in one, and for all of 
# Trøndelag in clusters, and compare the results.


library(sf)
library(dplyr)

config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/model_fitting.R")
source("R/build_matrices.R")
source("R/balancing_clusters.R")
source("R/model_validation.R")
source("R/visualization.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Load data and matrices ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

data <- readRDS("data/processed/engineered_data.rds")
aadt2024 <- load_data(config$data_paths$raw$aadt_results)
nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")


covariates <- c("functionalRoadClass:maxLanes",
                "minLanes:roadCategory",
                "functionalRoadClass",
                "maxLanes",
                "roadCategory")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Run models with and without clusters ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

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

# In principle, these two calls should produce roughly identical predictions.

# Total approval rates
rbind(trondelag_cluster$diagnostics$approval$approved,
      trondelag_in_one$diagnostics$approval$approved)

# Plot maps
plot_directed_links(trondelag_cluster$data)
plot_directed_links(trondelag_in_one$data)

# Aproval aggregated by municipality
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


