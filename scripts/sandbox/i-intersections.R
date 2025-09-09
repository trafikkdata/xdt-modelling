# I-intersections

library(sf)
library(dplyr)
library(INLA)
nvdb <- nvdb_objects()

#config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/model_fitting.R")


nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")


two_in_two_out <- dplyr::filter(nodes, numberOfIncomingLinks == 2 & numberOfOutgoingLinks == 2)
two_parent_links <- dplyr::filter(two_in_two_out, numberOfUndirectedLinks == 2)


leaflet::leaflet(two_parent_links, options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addCircleMarkers()


ende_noder <- dplyr::filter(nodes, numberOfIncomingLinks == 1 & numberOfOutgoingLinks == 1)

leaflet::leaflet(ende_noder, options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addCircleMarkers()
