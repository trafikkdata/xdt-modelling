library(sf)
library(dplyr)
library(INLA)

source("R/model_fitting.R")

trondheim_data <- read_sf("data/processed/trondheim_data.geojson")

trondheim_data$aadt_without_bus <- trondheim_data$aadt
trondheim_data$aadt_without_bus[!is.na(trondheim_data$bus_aadt)] <- NA

# Create spatial index - this is simply the row number for each traffic link
trondheim_data$spatial.idx <- 1:nrow(trondheim_data)

adj_sparse <- build_adjacency_matrix(trondheim_data)
constraint_matrix <- build_flow_constraints(trondheim_data)

#trd <- select(trondheim_data, aadt, minLanes, spatial.idx, roadSystem)

# Model without bus data
formula_nobus <- aadt_without_bus ~ minLanes +
  f(spatial.idx, model = "besag", graph = adj_sparse, 
    adjust.for.con.comp = FALSE, scale.model = FALSE, constr = TRUE) +
  f(roadSystem, model="iid")

mod_nobus <- inla(formula_nobus, 
                 family = "poisson",
                 data = trondheim_data,
                 control.predictor=list(link=1))

summary(mod_nobus)

formula_bus <- update(formula_nobus, aadt ~ . + hasOnlyPublicTransportLanes)
formula_bus

mod_bus <- inla(formula_bus, 
                family = "poisson",
                data = trondheim_data,
                control.predictor=list(link=1))
summary(mod_bus)
