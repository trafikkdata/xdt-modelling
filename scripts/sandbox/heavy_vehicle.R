# Heavy vehicle model

# Testing modelling the heavy vehicle volume directly, rather than using the percentage

# Load functions
files.sources = list.files("R/", full.names = TRUE)
sapply(files.sources, source)

# Load packages
library(sf)
library(dplyr)
library(INLA)

# Data
data <- readRDS("data/processed/engineered_data.rds")
aadt2024 <- read.csv("data/raw/traffic-links-aadt-data-2024.csv")
nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")

nvdb <- nvdb_objects()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preparing heavy vehicle volumes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data <- data %>% 
  mutate(
    heavy_aadt = case_when(traffic_volume_source == "Bus" ~ aadt,
                           TRUE ~ round(bestDataSourceAadt_heavyRatio*bestDataSourceAadt_trafficVolumeValue)),
    heavyRatio = case_when(bestDataSourceAadt_heavyRatio == 0 ~ 0.0001,
                           bestDataSourceAadt_heavyRatio == 1 ~ 0.9999,
                           TRUE ~ bestDataSourceAadt_heavyRatio)
    )


# Create spatial index - this is simply the row number for each traffic link
data$spatial.idx <- 1:nrow(data)

adjacency_matrix <- readRDS("data/processed/adjacency_matrix_2024.rds")

formula <- heavy_aadt ~ functionalRoadClass:maxLanes + 
  minLanes:roadCategory + functionalRoadClass +
  maxLanes + roadCategory + 
  hasOnlyPublicTransportLanes + 
  f(spatial.idx, model = "besagproper", 
    graph = adjacency_matrix, 
    adjust.for.con.comp = FALSE, constr = TRUE) +
  f(roadSystem, model = "iid")



model <- inla(formula, 
              family = "poisson",
              data = data,
              control.predictor=list(link=1))

summary(model)

results_heavy <- calculate_approval_metrics(
  model = model,
  data = data,
  data_manual = aadt2024,
  truth_col = "TBA.offisiell",  # The ratio column in manual data
  aadt_col = "ÅDT.offisiell",          # Total AADT for conversion
  model_name = "heavy_inla",
  is_ratio_model = TRUE
)

results_heavy$summary

saveRDS(results_heavy, "results/heavy_vehicle.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Heavy ratio model ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

formula_old <- heavyRatio ~ 
  functionalRoadClass * maxLanes + 
  minLanes * highestSpeedLimit +
  minLanes * functionClass +
  roadCategory

old_model <- inla(
  formula_old,
  family = "beta",
  data   = data,
  control.predictor = list(link = 1, compute = TRUE)
)



# How should we validate these predictions? The manual data has the ratios. 
# So for the volume predictions, convert to ratio using what as the total AADT? 
# The predicted AADT or the manual AADT?



