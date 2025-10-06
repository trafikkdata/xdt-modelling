# Adjacency matrix differences


library(dplyr)
library(ggplot2)
library(INLA)

source("R/model_validation.R")

data <- readRDS("data/processed/engineered_data.rds")
data_old <- readRDS("data/processed/data_old.rds")
aadt2024 <- read.csv("data/processed/traffic-links-aadt-data-2024.csv")


inla_graph <- readRDS("data/processed/inla_adj_mat.rds")

data$spatial.idx <- 1:nrow(data)
data_old$spatial.idx <- 1:nrow(data_old)

# Using Johannes' adjacency matrix
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


results <- calculate_approved(model = model, data = data, data_manual = aadt2024, model_name = "inla")
results$approved



# Using new adjacency matrix
adjacency_matrix <- readRDS("data/processed/adjacency_matrix_2024.rds")

data$aadt <- round(data_old$prelimAadt)
factor_columns <- c('roadCategory', 'highestSpeedLimit', 'functionalRoadClass',
                    'isFerryRoute', 'isNorwegianScenicRoute', 'maxLanes', 'minLanes',
                    'functionClass', 'roadSystem')
data[factor_columns] <- lapply(data[factor_columns], as.factor)

data$logLength <- log(data$length + 1)

numeric_columns <- c('logLength', 'numberOfEstablishments',
                     'numberOfEmployees', 'urbanRatio', 'numberOfInhabitants')
data[numeric_columns] <- scale(data[numeric_columns])


formula <- aadt ~ 
  f(spatial.idx, model = "besagproper", graph = adjacency_matrix,
    adjust.for.con.comp = FALSE, constr=T) + 
  f(roadSystem, model = "iid") + 
  functionalRoadClass:maxLanes +
  minLanes:roadCategory +
  functionalRoadClass +
  maxLanes +
  roadCategory

model2 <- inla(formula, family="poisson", data=data,
              control.predictor=list(link=1))


results2 <- calculate_approved(model = model2, data = data, data_manual = aadt2024, model_name = "inla")
results2$approved








data_old_sub <- select(data_old, spatial.idx, 
                       roadSystem = roadSystemReference, functionalRoadClass, 
                       maxLanes, minLanes, roadCategory)

data_new_sub <- select(data, spatial.idx, 
                       roadSystem, functionalRoadClass, 
                       maxLanes, minLanes, roadCategory)

library(compareDF)
comparedf(data_old_sub, data_new_sub) %>% summary() 

