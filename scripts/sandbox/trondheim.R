# Testing model in various sub-areas (Trondheim)

library(sf)
library(dplyr)
library(INLA)

config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/model_fitting.R")
source("R/model_validation.R")

trondheim_data <- read_sf("data/processed/trondheim_data.geojson")
aadt2024 <- read.csv("data/raw/traffic-links-aadt-data-2024.csv")
nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")


trondheim_data$aadt_without_bus <- trondheim_data$aadt
trondheim_data$aadt_without_bus[!is.na(trondheim_data$bus_aadt)] <- NA
trondheim_data$aadt_without_bus_sd <- trondheim_data$aadt_sd
trondheim_data$aadt_without_bus_sd[!is.na(trondheim_data$bus_aadt)] <- NA

# Create spatial index - this is simply the row number for each traffic link
trondheim_data$spatial.idx <- 1:nrow(trondheim_data)

adj_sparse <- build_adjacency_matrix(trondheim_data)
constraint_matrix <- build_incidence_matrix(nodes = nodes, trondheim_data)


# Model without bus data ----
formula_nobus <- aadt_without_bus ~ minLanes +
  f(spatial.idx, model = "besag", graph = adj_sparse, 
    adjust.for.con.comp = FALSE, scale.model = FALSE, constr = TRUE) +
  f(roadSystem, model="iid")

mod_nobus <- inla(formula_nobus, 
                  family = "poisson",
                  data = trondheim_data,
                  control.predictor=list(link=1))

summary(mod_nobus)

# Model with bus data ----
formula_bus <- update(formula_nobus, aadt ~ . + hasOnlyPublicTransportLanes)
formula_bus

mod_bus <- inla(formula_bus, 
                family = "poisson",
                data = trondheim_data,
                control.predictor=list(link=1))
summary(mod_bus)


trd_relevant <- dplyr::select(trondheim_data, id, aadt, aadt_sd)
balanced_trondheim <- balance_predictions(data = trondheim_data, model = mod_bus, 
                                      #colname_aadt = "aadt_without_bus", 
                                      #colname_sd = "aadt_without_bus_sd", 
                                      constraint_matrix = constraint_matrix)

print(balanced_trondheim$diagnostics)

inla_res <- calculate_approved(
  model = mod_bus, 
  data = trondheim_data, 
  data_manual = aadt2024,
  model_name = "inla")

balanced_res <- calculate_approved(
  pred = balanced_trondheim$results$balanced_pred, 
  sd = balanced_trondheim$results$balanced_pred,
  data = trondheim_data, 
  data_manual = aadt2024, 
  model_name = "balansert")

rbind(inla_res$approved, balanced_res$approved)




# Plotting
nvdb <- nvdb_objects()

pal <- leaflet::colorBin(
  palette = "viridis",
  domain = NULL, 
  reverse = TRUE,
  na.color = "#88807b"
)
pal_bin <- leaflet::colorFactor(
  palette = "viridis",
  domain = NULL, 
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


uretta <- inla_res$uretta %>% 
  st_as_sf() %>% 
  mutate(balansert_pred = balanced_res$uretta$balansert_pred,
         balansert_sd = balanced_res$uretta$balansert_sd) %>% 
  mutate(text = paste0("INLA: ", inla_pred, 
                       "<br>Balanced: ", balansert_pred,
                       "<br>ÅDT 2023: ", ÅDT.fjorårets,
                       "<br>ÅDT 2024: ", ÅDT.offisiell,
                       "<br>Målt eller utleda ÅDT: ", ÅDT.fra.datagrunnlag,
                       "<br>ID: ", ID))


leaflet::leaflet(uretta, options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    color = ~ pal(uretta[["balansert_pred"]]),
    popup = ~ text,
    opacity = 1)

leaflet::leaflet(uretta, options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    color = ~ pal_bin(uretta[["approved"]]),
    popup = ~ text,
    opacity = 1)



# Undersøker A1 for node 3508236 ----
okstadbakken_problem <- constraint_matrix["3508236",]
okstadbakken_links <- okstadbakken_problem[okstadbakken_problem != 0]
okstadbakken_node <- nodes %>% filter(id == "3508236")

nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")
traffic_node_network <- nodes
link_ids <- trondheim_data$id %>% as.vector()

turning_movements <- nodes %>% filter(id == "3508236")
