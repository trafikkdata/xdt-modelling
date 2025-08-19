# Testing model in various sub-areas (Trondheim, Ålesund and Finnmark)

library(sf)
library(dplyr)
library(INLA)

config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/model_fitting.R")
source("R/model_validation.R")

trondheim_data <- read_sf("data/processed/trondheim_data.geojson")

trondheim_data$aadt_without_bus <- trondheim_data$aadt
trondheim_data$aadt_without_bus[!is.na(trondheim_data$bus_aadt)] <- NA
trondheim_data$aadt_without_bus_sd <- trondheim_data$aadt_sd
trondheim_data$aadt_without_bus_sd[!is.na(trondheim_data$bus_aadt)] <- NA

# Create spatial index - this is simply the row number for each traffic link
trondheim_data$spatial.idx <- 1:nrow(trondheim_data)

adj_sparse <- build_adjacency_matrix(trondheim_data)
constraint_matrix <- build_flow_constraints(trondheim_data)


# Model without bus data ----
formula_nobus <- aadt_without_bus ~ minLanes +
  f(spatial.idx, model = "besag", graph = adj_sparse, 
    adjust.for.con.comp = FALSE, scale.model = FALSE, constr = TRUE) +
  f(roadSystem, model="iid")

mod_nobus <- inla(formula_nobus, 
                  family = "poisson",
                  data = trondheim_data,
                  control.predictor=list(link=1))

summary(mod_nobus)

# Model with bus data ----
formula_bus <- update(formula_nobus, aadt ~ . + hasOnlyPublicTransportLanes)
formula_bus

mod_bus <- inla(formula_bus, 
                family = "poisson",
                data = trondheim_data,
                control.predictor=list(link=1))
summary(mod_bus)


trd_relevant <- dplyr::select(trondheim_data, id, aadt, aadt_sd)
balanced_trondheim <- balance_predictions(data = trondheim_data, model = mod_bus, 
                                      #colname_aadt = "aadt_without_bus", 
                                      #colname_sd = "aadt_without_bus_sd", 
                                      constraint_matrix = constraint_matrix)

print(balanced_trondheim$diagnostics)
