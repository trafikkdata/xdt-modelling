# Testing model on Finnmark

library(sf)
library(dplyr)
library(INLA)

config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/model_fitting.R")
source("R/model_validation.R")

finnmark_data <- read_sf("data/processed/finnmark_data.geojson")
aadt2024 <- read.csv("data/raw/traffic-links-aadt-data-2024.csv")
nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")

# Create spatial index - this is simply the row number for each traffic link
finnmark_data$spatial.idx <- 1:nrow(finnmark_data)

adj_sparse <- build_adjacency_matrix(finnmark_data)
constraint_matrix <- build_incidence_matrix(nodes, finnmark_data)


formula <- aadt ~ minLanes +
  f(spatial.idx, model = "besag", graph = adj_sparse, 
    adjust.for.con.comp = FALSE, scale.model = FALSE, constr = TRUE) +
  f(roadSystem, model="iid")

model <- inla(formula, 
              family = "poisson",
              data = finnmark_data,
              control.predictor=list(link=1))

summary(model)

balanced_finnmark <- balance_predictions(data = finnmark_data, model = model, 
                                         constraint_matrix = constraint_matrix)

print(balanced_finnmark$diagnostics)

inla_res <- calculate_approved(
  model = model,
  data = finnmark_data, 
  data_manual = aadt2024,
  model_name = "inla")

balanced_res <- calculate_approved(
  pred = balanced_finnmark$results$balanced_pred, 
  sd = balanced_finnmark$results$balanced_pred,
  data = finnmark_data, 
  data_manual = aadt2024, 
  model_name = "balansert")

rbind(inla_res$approved, balanced_res$approved)



retta <- inla_res$retta %>% 
  st_as_sf() %>% 
  mutate(balansert_pred = balanced_res$retta$balansert_pred,
         balansert_sd = balanced_res$retta$balansert_sd) %>% 
  mutate(text = paste0("INLA: ", inla_pred, 
                       "<br>Balanced: ", balansert_pred,
                       #"<br>ÅDT 2023: ", ÅDT.fjorårets,
                       "<br>Målt eller utleda ÅDT: ", aadt,
                       "<br>ID: ", id))

leaflet::leaflet(retta, options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    color = ~ pal(retta[["balansert_pred"]]),
    popup = ~ text,
    opacity = 1)
