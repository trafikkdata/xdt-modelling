library(sf)
library(dplyr)

source("R/utilities.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load preprocessed data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

preprocessed_data <- readRDS("data/processed/preprocessed_data.rds") %>% 
  add_geometry_to_traffic_links()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Filter out Trondheim data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

point <- c(63.41, 10.31)
bbox <- sf::st_bbox(
  c(xmin = point[2] - 0.25,
    xmax = point[2] + 0.25,
    ymin = point[1] - 0.07,
    ymax = point[1] + 0.07),
  crs = sf::st_crs(nvdb_objects()$nvdb_crs)
)

trondheim_geom <- get_traffic_links_in_bbox(preprocessed_data, bbox = bbox) %>% 
  dplyr::select(id, geometry, startTrafficNodeId, endTrafficNodeId)

trondheim_data <- preprocessed_data %>% 
  dplyr::filter(id %in% trondheim_geom$id) 

st_write(trondheim_data, "data/processed/trondheim_data.geojson", append = FALSE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Filter out Aalesund data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

point <- c(62.474378, 6.196128)
bbox <- sf::st_bbox(
  c(xmin = point[2] - 0.15,
    xmax = point[2] + 0.15,
    ymin = point[1] - 0.015,
    ymax = point[1] + 0.01),
  crs = sf::st_crs(nvdb_objects()$nvdb_crs)
)


aalesund_geom <- get_traffic_links_in_bbox(preprocessed_data, bbox = bbox) %>% 
  dplyr::select(id, geometry, startTrafficNodeId, endTrafficNodeId)

aalesund_data <- preprocessed_data %>% 
  dplyr::filter(id %in% aalesund_geom$id) 

st_write(aalesund_data, "data/processed/aalesund_data.geojson", append = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Filter out Finnmark data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

point <- c(69.877413, 24.454276)
bbox <- sf::st_bbox(
  c(xmin = point[2] - 2.9,
    xmax = point[2] + 6,
    ymin = point[1] - 2,
    ymax = point[1] + 2),
  crs = sf::st_crs(nvdb_objects()$nvdb_crs)
)


finnmark_geom <- get_traffic_links_in_bbox(preprocessed_data, bbox = bbox) %>% 
  dplyr::select(id, geometry, startTrafficNodeId, endTrafficNodeId)

finnmark_data <- preprocessed_data %>% 
  dplyr::filter(id %in% finnmark_geom$id) 

st_write(finnmark_data, "data/processed/finnmark_data.geojson", append = FALSE)

leaflet::leaflet(finnmark_data, options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    color = "black",
    opacity = 1)




