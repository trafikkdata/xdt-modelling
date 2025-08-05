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
