# Geographically split INLA

# Examining whether it makes sense to run the INLA-model for smaller 
# geographical areas rather than all of Norway in one run.
# This way, the coefficients can be tailored better to the specific region. 

# Load functions
files.sources = list.files("R/", full.names = TRUE)
sapply(files.sources, source)

# Load packages
library(sf)
library(dplyr)

# Data
data <- readRDS("data/processed/engineered_data.rds")
aadt2024 <- read.csv("data/raw/traffic-links-aadt-data-2024.csv")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Running the INLA model separately vs jointly for all of Norway ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

covariates <- c("functionalRoadClass:maxLanes",
                "minLanes:roadCategory",
                "functionalRoadClass",
                "maxLanes",
                "roadCategory")

separate_model <- run_modeling_pipeline(inla_groups_to_process = "all", 
                                        inla_grouping_variable = "county",
                                        covariates = covariates,
                                        balance_predictions = FALSE)

joint_model <- run_modeling_pipeline(inla_groups_to_process = "all", 
                                     covariates = covariates,
                                     balance_predictions = FALSE)

rbind(separate_model$diagnostics$approval$approved, joint_model$diagnostics$approval$approved)






