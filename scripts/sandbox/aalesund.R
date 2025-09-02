# Testing model on Ålesund

library(sf)
library(dplyr)
library(INLA)

config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/model_fitting.R")
source("R/model_validation.R")

aalesund_data <- read_sf("data/processed/aalesund_data.geojson")
aadt2024 <- read.csv("data/raw/traffic-links-aadt-data-2024.csv")
nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")

# Create spatial index - this is simply the row number for each traffic link
aalesund_data$spatial.idx <- 1:nrow(aalesund_data)

adj_sparse <- build_adjacency_matrix(aalesund_data)
constraint_matrix <- build_incidence_matrix(nodes, link_ids= aalesund_data)


formula <- aadt ~ minLanes +
  f(spatial.idx, model = "besag", graph = adj_sparse, 
    adjust.for.con.comp = FALSE, scale.model = FALSE, constr = TRUE) +
  f(roadSystem, model="iid")

model <- inla(formula, 
                  family = "poisson",
                  data = aalesund_data,
                  control.predictor=list(link=1))

summary(model)

balanced_aalesund <- balance_predictions(data = aalesund_data, model = model, 
                                          constraint_matrix = constraint_matrix)

print(balanced_aalesund$diagnostics)

inla_res <- calculate_approved(
  model = model,
  data = aalesund_data, 
  data_manual = aadt2024,
  model_name = "inla")

balanced_res <- calculate_approved(
  pred = balanced_aalesund$results$balanced_pred, 
  sd = balanced_aalesund$results$balanced_pred,
  data = aalesund_data, 
  data_manual = aadt2024, 
  model_name = "balansert")

rbind(inla_res$approved, balanced_res$approved)



nvdb <- nvdb_objects()

pal <- leaflet::colorBin(
  palette = "viridis",
  domain = NULL, 
  reverse = TRUE,
  na.color = "#88807b"
)

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



examine_node_flow(A1 = constraint_matrix, node_id = "271925")
turns_df <- get_turning_movements(nodes = nodes, node_id = "271925")
turns_df[1,2]

print_turning_movements_for_link_at_node(node_id = "271925", link_id = "0.0-1.0@248954-WITH", nodes)
print_turning_movements_for_link_at_node(node_id = "271123", link_id = "0.08626678-1.0@248963-WITH", nodes)
print_turning_movements_for_link_at_node(node_id = "271111", link_id = "0.85695107-0.78147169@248950-WITH", nodes)

get_turning_movements(nodes, "271111")
