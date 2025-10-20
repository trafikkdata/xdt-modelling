# Merging sparse covariate categories

# Most of the covariates are categorical, with many very sparse categories. 
# We try merging some categories for better interpretability.

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
# Merged vs not merged sparse covariate categories ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

good_formula <- c("functionalRoadClass:maxLanes", 
                  "minLanes:roadCategory", 
                  #"functionalRoadClass:roadCategory", 
                  "functionalRoadClass", 
                  "maxLanes", 
                  "minLanes", 
                  "roadCategory", 
                  "hasOnlyPublicTransportLanes", 
                  "isFerryRoute", 
                  "isNorwegianScenicRoute")

good_formula_merged <- c("functionalRoadClass_merged:maxLanes_merged", 
                         "minLanes_merged:roadCategory", 
                         #"functionalRoadClass_merged:roadCategory", 
                         "functionalRoadClass_merged", 
                         "maxLanes_merged", 
                         "minLanes_merged", 
                         "roadCategory", 
                         "hasOnlyPublicTransportLanes", 
                         "isFerryRoute", 
                         "isNorwegianScenicRoute")

good_formula <- c("functionalRoadClass:maxLanes",
                  "minLanes:roadCategory",
                  "functionalRoadClass",
                  "maxLanes",
                  "roadCategory",
                  "hasOnlyPublicTransportLanes")
good_formula_merged <- c("functionalRoadClass_merged:maxLanes_merged",
                         "minLanes_merged:roadCategory",
                         "functionalRoadClass_merged",
                         "maxLanes_merged",
                         "roadCategory",
                         "hasOnlyPublicTransportLanes")

merged_covariates <- run_modeling_pipeline(inla_groups_to_process = "Trøndelag",
                                           inla_grouping_variable = "county",
                                           covariates = good_formula_merged,
                                           balance_predictions = FALSE,
                                           model_name = "merged")
original_covariates <- run_modeling_pipeline(inla_groups_to_process = "Trøndelag",
                                             inla_grouping_variable = "county",
                                             covariates = good_formula,
                                             balance_predictions = FALSE,
                                             model_name = "original")

rbind(merged_covariates$diagnostics$approval$approved, original_covariates$diagnostics$approval$approved)

