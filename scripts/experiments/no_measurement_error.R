# Testing model with no measurement error

library(sf)
library(dplyr)
library(INLA)

config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/model_fitting.R")
source("R/build_matrices.R")
source("R/balancing_clusters.R")
source("R/model_validation.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Load data and matrices ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

data <- readRDS("data/processed/engineered_data.rds")
aadt2024 <- load_data(config$data_paths$raw$aadt_results)
nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")

trd <- filter(data, municipalityIds == "5001")

covariates <- c("functionalRoadClass:maxLanes",
                "minLanes:roadCategory",
                "functionalRoadClass",
                "maxLanes",
                "roadCategory")

inla_res <- fit_group_model(trd, covariates = covariates)

trd$pred <- inla_res$posterior_median
trd$sd <- inla_res$posterior_sd
inla_metrics <- calculate_approval_metrics(data = trd, data_manual = aadt2024)

inla_metrics$summary



balanced_res <- balance_predictions(data = trd, nodes = nodes, 
                                    nodes_to_balance = "complete_nodes", 
                                    balancing_grouping_variable = "no_clustering")

trd$pred <- balanced_res$balanced_res$balanced_pred
trd$sd <- balanced_res$balanced_res$balanced_sd

balanced_metrics <- calculate_approval_metrics(data = trd, data_manual = aadt2024)
balanced_metrics$summary





