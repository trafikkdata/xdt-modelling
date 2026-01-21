# Ramp covariate
# 
# 
# 

# Load functions
files.sources = list.files("R/", full.names = TRUE)
sapply(files.sources, source)

# Load packages
library(sf)
library(dplyr)
library(tictoc)

# Data
data <- readRDS("data/processed/engineered_data.rds")


clusters <- strategic_network_clustering(data)

covariates_no_ramp <- c("functionalRoadClass:maxLanes",
                "minLanes:roadCategory",
                "functionalRoadClass",
                "maxLanes",
                "roadCategory",
                "hasOnlyPublicTransportLanes")

covariates_ramp <- c("functionalRoadClass:maxLanes",
                "minLanes:roadCategory",
                "functionalRoadClass",
                "functionalRoadClass:isRamp",
                "maxLanes",
                "roadCategory",
                "hasOnlyPublicTransportLanes")

no_ramp <- run_modeling_pipeline(
  data = data,
  balancing_grouping_variable = clusters,
  covariates = covariates_no_ramp,
  nodes_to_balance = "complete_nodes",
  model_name = "no ramp"
)

ramp <- run_modeling_pipeline(
  data = data,
  balancing_grouping_variable = clusters,
  covariates = covariates_ramp,
  nodes_to_balance = "complete_nodes",
  model_name = "ramp"
)

rbind(no_ramp$diagnostics$approval$approved,
      ramp$diagnostics$approval$approved)


plot_directed_links(df = no_ramp$data, county = "Møre og Romsdal")
plot_directed_links(df = ramp$data, county = "Møre og Romsdal")



