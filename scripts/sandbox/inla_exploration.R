# Basic INLA implementation

library(sf)
library(dplyr)
library(INLA)

# Load functions
files.sources = list.files("R/", full.names = TRUE)
sapply(files.sources, source)

data <- readRDS("data/processed/engineered_data.rds")
aadt2024 <- read.csv("data/raw/traffic-links-aadt-data-2024.csv")
nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")


# Create spatial index - this is simply the row number for each traffic link
data$spatial.idx <- 1:nrow(data)

# Create a unique nested identifier
data <- data %>%
  mutate(roadCategory_in_county = paste(roadCategory, county, sep="_"),
         log_last_year_aadt = log(lastYearAadt_trafficVolumeValue + 1))

length(unique(data$roadCategory_in_county))

adjacency_matrix <- readRDS("data/processed/adjacency_matrix_2024.rds")

data$log_last_year_aadt <- data$log_last_year_aadt + 0.1

formula <- aadt ~ functionalRoadClass:maxLanes + 
  minLanes:roadCategory + functionalRoadClass +
  maxLanes + roadCategory + 
  hasOnlyPublicTransportLanes + 
  log_last_year_aadt +
  isFerryRoute + isNorwegianScenicRoute +
  f(spatial.idx, model = "besagproper", 
    graph = adjacency_matrix, 
    adjust.for.con.comp = FALSE, constr = TRUE) +
  f(roadSystem, model = "iid") #+
  #f(roadSystem, model="iid", group = countyIds)
  #f(roadCategory_in_county, model="iid")

data$aadt <- data$aadt + 1
formula <- aadt ~ log_last_year_aadt + f(id, model = "iid") #+ f(spatial.idx, model = "besagproper", 
                                                               #graph = adjacency_matrix, 
                                                               #adjust.for.con.comp = FALSE, constr = TRUE)

model <- inla(formula, 
              family = "poisson",
              data = data,
              control.predictor=list(link=1))
model <- inla(formula, 
              family = "nbinomial",
              data = data,
              control.predictor=list(link=1))
model <- inla(formula, 
              family = "gamma",
              data = data, 
              control.predictor=list(link=1))

summary(model)

inla_res <- calculate_approved(
  model = model,
  data = data, 
  data_manual = aadt2024,
  model_name = "inla")

inla_res$approved

head(select(data, log_last_year_aadt, aadt), 100)

sum(is.na(data$log_last_year_aadt))

summary(data$aadt)
summary(data$log_last_year_aadt)
summary(model$summary.fitted.values$mean)
