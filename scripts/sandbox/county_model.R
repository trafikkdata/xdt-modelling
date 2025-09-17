
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

one_county <- "Innlandet"

county_data <- filter(data, county == one_county)

# Create spatial index - this is simply the row number for each traffic link
county_data$spatial.idx <- 1:nrow(county_data)

adj_sparse <- build_adjacency_matrix(county_data)
constraint_matrix <- build_incidence_matrix(nodes, county_data)


formula <- aadt ~ minLanes +
  f(spatial.idx, model = "besag", graph = adj_sparse, 
    adjust.for.con.comp = FALSE, scale.model = FALSE, constr = TRUE) +
  f(roadSystem, model="iid")

model <- inla(formula, 
              family = "poisson",
              data = county_data,
              control.predictor=list(link=1))

summary(model)

balanced_county <- balance_predictions(data = county_data, model = model, 
                                         constraint_matrix = constraint_matrix)

print(balanced_county$diagnostics)

inla_res <- calculate_approved(
  model = model,
  data = county_data, 
  data_manual = aadt2024,
  model_name = "inla")

balanced_res <- calculate_approved(
  pred = balanced_county$results$balanced_pred, 
  sd = balanced_county$results$balanced_pred,
  data = county_data, 
  data_manual = aadt2024, 
  model_name = "balansert")

rbind(inla_res$approved, balanced_res$approved)


retta <- inla_res$retta %>% 
  add_geometry_to_traffic_links() %>% 
  st_as_sf() %>% 
  mutate(balansert_pred = balanced_res$retta$balansert_pred,
         balansert_sd = balanced_res$retta$balansert_sd) %>% 
  mutate(text = paste0("INLA: ", inla_pred, 
                       "<br>Balanced: ", balansert_pred,
                       #"<br>ÅDT 2023: ", ÅDT.fjorårets,
                       "<br>Målt eller utleda ÅDT: ", aadt,
                       "<br>ID: ", id))


nvdb <- nvdb_objects()

pal <- leaflet::colorBin(
  palette = "viridis",
  domain = NULL, 
  reverse = TRUE,
  na.color = "#88807b"
)

leaflet::leaflet(retta, options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    color = ~ pal(retta[["balansert_pred"]]),
    popup = ~ text,
    opacity = 1)


