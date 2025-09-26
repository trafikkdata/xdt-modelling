
library(sf)
library(dplyr)
library(INLA)

config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/model_fitting.R")
source("R/model_validation.R")


data <- readRDS("data/processed/engineered_data.rds")
aadt2024 <- load_data(config$data_paths$raw$aadt_results)
nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")


# Create spatial index - this is simply the row number for each traffic link
data$spatial.idx <- 1:nrow(data)

# Create a unique nested identifier
data <- data %>%
  mutate(roadCategory_in_county = paste(roadCategory, county, sep="_"))

length(unique(data$roadCategory_in_county))

adjacency_matrix <- readRDS("data/processed/adjacency_matrix_2024.rds")

formula <- aadt ~ functionalRoadClass_merged:maxLanes_merged + 
  minLanes_merged:roadCategory + functionalRoadClass_merged +
  maxLanes_merged + minLanes_merged + roadCategory + 
  hasOnlyPublicTransportLanes + isFerryRoute + isNorwegianScenicRoute +
  f(spatial.idx, model = "besagproper", 
    graph = adjacency_matrix, 
    adjust.for.con.comp = FALSE, constr = TRUE) +
  f(roadSystem, model = "iid") +
  #f(roadSystem, model="iid", group = countyIds)
  f(roadCategory_in_county, model="iid")

  

model <- inla(formula, 
              family = "poisson",
              data = data,
              control.predictor=list(link=1))

summary(model)

inla_res <- calculate_approved(
  model = model,
  data = data, 
  data_manual = aadt2024,
  model_name = "inla")

inla_res$approved
