
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

data <- readRDS("data/processed/engineered_data.rds")
aadt2024 <- load_data(config$data_paths$raw$aadt_results)
nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")


# Testing functions

print_turning_movements_for_link_at_node(node_id = "347086", 
                                         link_id = "0.0-1.0@319629-WITH", 
                                         nodes)

turns <- get_turning_movements(nodes, node_id = "271925")



# model <- fit_model(data, c("minLanes", "functionalRoadClass")) # This takes a long time to run
model_county <- fit_group_model(county_data, c("minLanes", "functionalRoadClass"))

national_res <- fit_national_model(
  data, 
  formula_covariates = c("minLanes", "functionalRoadClass"),
  grouping_variable = "county", 
  groups = unique(data$county),
  inla_scope = "local")


# ----

res <- run_modeling_pipeline(groups_to_process = c("Trøndelag"))


retta <- res$data %>% 
  add_geometry_to_traffic_links() %>% 
  #st_as_sf() %>% 
  mutate(text = paste0("INLA: ", pred, 
                       "<br>Balanced: ", balanced_pred,
                       #"<br>ÅDT 2023: ", ÅDT.fjorårets,
                       "<br>Målt eller utleda ÅDT: ", aadt,
                       "<br>ID: ", id))
nvdb <- nvdb_objects()

pal <- leaflet::colorNumeric(
  palette = "viridis",
  reverse = TRUE,
  na.color = "#88807b",
  domain = retta$balanced_pred
)

leaflet::leaflet(retta, options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    color = ~ pal(balanced_pred),
    popup = ~ text,
    opacity = 1) |>
  leaflet::addLegend("bottomright", pal = pal, values = ~ balanced_pred, title = "ÅDT", opacity = 1)



# Balancing I-intersections or not ---------------------------------------------
balanced_i_intersections <- run_modeling_pipeline(groups_to_process = "all", 
                                                  balance_i_intersections = TRUE)
saveRDS(balanced_i_intersections, "results/balanced_i_intersections.rds")
balanced_i_intersections <- readRDS("results/balanced_i_intersections.rds")

unbalanced_i_intersections <- run_modeling_pipeline(groups_to_process = "all", 
                                                    balance_i_intersections = FALSE)
saveRDS(balanced_i_intersections, "results/unbalanced_i_intersections.rds")


approved_bali <- calculate_approved(data = balanced_i_intersections$data, 
                                    data_manual = aadt2024,
                                    model_name = "balanced_i_intersections")
approved_unbali <- calculate_approved(data = unbalanced_i_intersections$data, 
                                    data_manual = aadt2024,
                                    model_name = "unbalanced_i_intersections")
approved_bali$approved
approved_unbali$approved



# Merged vs not merged sparse covariate categories -----------------------------
good_formula <- c("functionalRoadClass:maxLanes", 
                  "minLanes:roadCategory", 
                  #"functionalRoadClass:roadCategory", 
                  "functionalRoadClass", 
                  "maxLanes", 
                  "minLanes", 
                  "roadCategory", 
                  "hasOnlyPublicTransportLanes", 
                  "isFerryRoute", 
                  "isNorwegianScenicRoute")

good_formula_merged <- c("functionalRoadClass_merged:maxLanes_merged", 
                         "minLanes_merged:roadCategory", 
                         #"functionalRoadClass_merged:roadCategory", 
                         "functionalRoadClass_merged", 
                         "maxLanes_merged", 
                         "minLanes_merged", 
                         "roadCategory", 
                         "hasOnlyPublicTransportLanes", 
                         "isFerryRoute", 
                         "isNorwegianScenicRoute")

merged_covariates <- run_modeling_pipeline(groups_to_process = "Trøndelag", 
                                           covariates = good_formula_merged)
original_covariates <- run_modeling_pipeline(groups_to_process = "Trøndelag",
                                             covariates = good_formula)

merged_approved <- calculate_approved(data = merged_covariates$data,
                                      data_manual = aadt2024,
                                      model_name = "merged_covariates")
original_approved <- calculate_approved(data = original_covariates$data,
                                      data_manual = aadt2024,
                                      model_name = "original_covariates")
rbind(merged_approved$approved, original_approved$approved)



# Running the model separately vs jointly for all of Norway
separate_model <- run_modeling_pipeline(groups_to_process = "all", 
                                        inla_scope = "local",
                                        covariates = good_formula_merged,
                                        balance_predictions = FALSE)
joint_model <- run_modeling_pipeline(groups_to_process = "all", 
                                     inla_scope = "national",
                                     covariates = good_formula_merged,
                                     balance_predictions = FALSE)

separate <- calculate_approved(data = separate_model$data,
                                      pred = separate_model$data$pred,
                                      sd = separate_model$data$sd,
                                      data_manual = aadt2024,
                                      model_name = "separate")
joint <- calculate_approved(data = joint_model$data,
                                        pred = joint_model$data$pred,
                                        sd = joint_model$data$sd,
                                        data_manual = aadt2024,
                                        model_name = "joint")
rbind(separate$approved, joint$approved)

sample_data <- data[1:5, c("id", "startTrafficNodeId", "endTrafficNodeId", "aadt")]

