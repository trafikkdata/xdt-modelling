# Testing pipeline function - why are the function results worse than the direct results?

library(dplyr)
library(ggplot2)
library(INLA)

source("R/model_validation.R")

data <- readRDS("data/processed/engineered_data.rds")
data_old <- readRDS("data/processed/data_old.rds")
aadt2024 <- read.csv("data/processed/traffic-links-aadt-data-2024_old.csv")
aadt2024 <- read.csv("data/raw/traffic-links-aadt-data-2024.csv")


inla_graph <- readRDS("data/processed/inla_adj_mat.rds")

data$spatial.idx <- 1:nrow(data)
data_old$spatial.idx <- 1:nrow(data_old)

formula <- prelimAadt ~ 
  f(spatial.idx, model = "besagproper", graph = inla_graph,
    adjust.for.con.comp = FALSE, constr=T) + 
  f(roadSystemReference, model = "iid") + 
  functionalRoadClass:maxLanes +
  minLanes:roadCategory +
  functionalRoadClass +
  maxLanes +
  roadCategory

model <- inla(formula, family="poisson", data=data_old,
              control.predictor=list(link=1))


results <- calculate_approved(model = model, data = data_old, data_manual = aadt2024, model_name = "inla")
results$approved

# Trying to get 71% with pipeline

covariates <- c("functionalRoadClass:maxLanes",
                "minLanes:roadCategory",
                "functionalRoadClass",
                "maxLanes",
                "roadCategory",
                "hasOnlyPublicTransportLanes")

mod_pipeline <- run_modeling_pipeline(data, 
  covariates = covariates,
  balance_predictions = FALSE,
  model_name = "no_balancing"
)

res_pipeline <- calculate_approved(data = mod_pipeline$data, data_manual = aadt2024, model_name = "inla")
res_pipeline$approved


