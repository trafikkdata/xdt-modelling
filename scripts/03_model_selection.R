library(sf)
library(dplyr)
library(INLA)

config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/model_fitting.R")
source("R/model_validation.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Load data and matrices ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

data <- readRDS("data/processed/preprocessed_data.rds")
aadt2024 <- load_data(config$data_paths$raw$aadt_results)


data$aadt_without_bus <- data$aadt
data$aadt_without_bus[!is.na(data$bus_aadt)] <- NA

# Create spatial index - this is simply the row number for each traffic link
data$spatial.idx <- 1:nrow(data)

adj_sparse <- readRDS("data/processed/adjacency_matrix_2024.rds")
constraint_matrix <- readRDS("data/processed/constraint_matrix_2024.rds")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Define candidate models ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# Here I run the models and check the auto-approval percentages, but I should 
# run the actual cross-validation here.


## Model without bus data ----
formula_nobus <- aadt_without_bus ~ minLanes +
  f(spatial.idx, model = "besag", graph = adj_sparse, 
    adjust.for.con.comp = FALSE, scale.model = FALSE, constr = TRUE) +
  f(roadSystem, model="iid")

mod_nobus <- inla(formula_nobus, 
                  family = "poisson",
                  data = data,
                  control.predictor=list(link=1))

summary(mod_nobus)

## Model with bus data ----
formula_bus <- update(formula_nobus, aadt ~ . + hasOnlyPublicTransportLanes)
formula_bus

mod_bus <- inla(formula_bus, 
                family = "poisson",
                data = data,
                control.predictor=list(link=1))
summary(mod_bus)


## Comparing auto-approval percentages ----
approved_nobus <- calculate_approved(model = mod_nobus, data = data, 
                                     data_manual = aadt2024,
                                     truth_name = "ÅDT.offisiell",
                                     model_name = "no_bus_data")

approved_bus <- calculate_approved(model = mod_bus, data = data, 
                                   data_manual = aadt2024,
                                   truth_name = "ÅDT.offisiell",
                                   model_name = "bus_data")

rbind(approved_bus$approved, approved_nobus$approved)


# Balancing
data <- balance_predictions(data = data, model = mod_bus, 
                            constraint_matrix = constraint_matrix)

