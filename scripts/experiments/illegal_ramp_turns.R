# I intersections

library(sf)
library(dplyr)
library(INLA)

#config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/model_fitting.R")


nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")
traffic_links <- readRDS("data/processed/engineered_data.rds")


ramps <- dplyr::filter(traffic_links, isRamp) %>% 
  mutate(
    kd_number = stringr::str_extract(roadSystemReferences, "(?<=KD)\\d+"),   # extract digits after KD
    kd_number = as.integer(kd_number),             # convert to number
    ramp_type = if_else(is.na(kd_number), NA_character_,
                        if_else(kd_number %% 2 == 0, "pakjoring", "avkjoring"))
  ) %>% add_geometry_to_traffic_links()

#ramp_nodes <- unique(c(ramps$startTrafficNodeId, ramps$endTrafficNodeId))

leaflet::leaflet(ramps, options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    popup = ~ ramp_type,
    opacity = 1)

ramps_long <- tidyr::pivot_longer(ramps, cols = c("startTrafficNodeId", "endTrafficNodeId"), 
                                  names_to = "node_type", values_to = "node_id") #%>% 

node_geometry <- select(nodes, id, geometry)

suspect_ramp_nodes <- ramps_long %>% 
  st_drop_geometry() %>% 
  dplyr::left_join(nodes, join_by(node_id == id)) %>% 
  select(id, node_id, ramp_type, numberOfIncomingLinks, numberOfOutgoingLinks, node_type, geometry) %>% 
  mutate(illegal = case_when(
    ramp_type == "pakjoring" & node_type == "endTrafficNodeId" & numberOfOutgoingLinks > 1 ~ "illegal turn onto main road",
    ramp_type == "avkjoring" & node_type == "startTrafficNodeId" & numberOfIncomingLinks > 1 ~ "illegal turn onto ramp",
    .default = "nothing detected"
  )) %>% 
  dplyr::filter(illegal != "nothing detected") %>% 
  st_as_sf()
  
  #tidyr::pivot_wider(id_cols = id, names_from = ramp_type, values_from = node_id)

nvdb <- nvdb_objects()

leaflet::leaflet(suspect_ramp_nodes, options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addCircleMarkers(popup = ~ illegal)


# Jeg tror dette plukker opp de ulovlige svingebevegelsene, men det plukker opp 
# veldig mye annet også, og her er det mye som ikke gir mening.