
library(sf)
library(dplyr)
library(INLA)

config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/model_fitting.R")
source("R/build_matrices.R")
source("R/balancing_clusters.R")
source("R/model_validation.R")
source("R/visualization.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Load data and matrices ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
data_new <- jsonlite::fromJSON("data/raw/directed-traffic-links-2024_new.json")
data_old <- jsonlite::fromJSON("data/raw/directed-traffic-links-2024.json")


data <- readRDS("data/processed/engineered_data.rds")
aadt2024 <- load_data(config$data_paths$raw$aadt_results)
nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Test "fit_group_model" function and "fit_national_model" ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# These are internal functions called by "run_modeling_pipeline".

# model <- fit_model(data, c("minLanes", "functionalRoadClass")) # This takes a long time to run
model_county <- fit_group_model(county_data, c("minLanes", "functionalRoadClass"))

national_res <- fit_national_model(
  data, 
  formula_covariates = c("minLanes", "functionalRoadClass"),
  grouping_variable = "county", 
  groups = unique(data$county),
  inla_scope = "local")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Test "run_modeling_pipeline" function for one county ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

res <- run_modeling_pipeline(inla_groups_to_process = c("Telemark"), 
                             inla_grouping_variable = "county")


retta <- res$data %>% 
  add_geometry_to_traffic_links() %>% 
  #st_as_sf() %>% 
  mutate(text = paste0("INLA: ", pred, 
                       "<br>Balanced: ", balanced_pred,
                       #"<br>ÅDT 2023: ", ÅDT.fjorårets,
                       "<br>Measured: ", aadt,
                       "<br>ID: ", id))
nvdb <- nvdb_objects()

pal <- leaflet::colorNumeric(
  palette = "viridis",
  reverse = TRUE,
  na.color = "#88807b",
  domain = retta$balanced_pred
)

leaflet::leaflet(retta, 
                 options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    color = ~ pal(balanced_pred),
    popup = ~ text,
    opacity = 1) |>
  leaflet::addLegend("bottomright", pal = pal, values = ~ balanced_pred, title = "ÅDT", opacity = 1)


pal <- leaflet::colorNumeric(
  palette = "viridis",
  reverse = TRUE,
  na.color = "#88807b",
  domain = retta$aadt
)
leaflet::leaflet(retta, 
                 options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    color = ~ pal(aadt),
    popup = ~ text,
    opacity = 1) |>
  leaflet::addLegend("bottomright", pal = pal, values = ~ aadt, title = "ÅDT", opacity = 1)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Data for Merijn ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

results <- readRDS("results/unbalanced_i_intersections.rds")
road_placements <- readRDS("data/processed/traffic_link_roadPlacements.rds")

df <- results$data %>% 
  split_traffic_link_id() %>% 
  dplyr::select(id, parentTrafficLinkId, 
                startPosition, endPosition, 
                isTrafficWithMetering, 
                registeredAadt = aadt, registeredSd = aadt_sd, 
                startTrafficNodeId, endTrafficNodeId,
                pred, sd) %>% 
  full_join(road_placements)
  


# write.csv(df, "data/processed/predictions_for_merijn.csv", row.names = FALSE)

json_df <- jsonlite::toJSON(df)
write(json_df, "data/processed/predictions_for_merijn.json")

test <- jsonlite::fromJSON("data/processed/predictions_for_merijn.json")


