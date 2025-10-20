# No measurement error

# Testing model with no/less measurement error.

# Load functions
files.sources = list.files("R/", full.names = TRUE)
sapply(files.sources, source)

# Load packages
library(sf)
library(dplyr)

# Data
data <- readRDS("data/processed/engineered_data.rds")
aadt2024 <- read.csv("data/raw/traffic-links-aadt-data-2024.csv")
nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Fit models ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

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





