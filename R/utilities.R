#' Add geometry to traffic links
#'
#' @param df Data frame without geometry
#' @param id_name The name of the id column in df.
#'
#' @returns The original data frame df, with and added geometry column. The return object has class sf.
#' @export
#'
#' @examples
add_geometry_to_traffic_links <- function(df, id_name = "id", directed = TRUE){
  # Input: dataframe with id column.
  # Output: the same data frame, now with geometry column. 
  
  if("sf" %in% class(df)){
    return(df)
  }
  
  # If id does not contain "with" or "against", rename it to parentTrafficLinkId
  if(!grepl("(WITH|AGAINST)$", df[1, id_name])){
    directed <- FALSE
  }
  
  geom <- readRDS("data/processed/traffic_link_geometries.rds")
  
  id_name_geom <- ifelse(directed, "id", "parentTrafficLinkId")
  
  # Leave out the non-relevant id column
  geom <- dplyr::select(geom, all_of(c(id_name_geom, "geometry")))
    
  # Create a named vector for the join
  join_vector <- setNames(id_name_geom, id_name)
  
  df_geom <- dplyr::left_join(df, geom, by = join_vector) |> sf::st_as_sf()
  
  return(df_geom)
}

load_data <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("Data file not found: ", file_path)
  }
  
  if (tools::file_ext(file_path) == "json") {
    data <- jsonlite::fromJSON(file_path)
  } else if (tools::file_ext(file_path) == "rds") {
    data <- readRDS(file_path)
  } else if (tools::file_ext(file_path) == "csv") {
    data <- read.csv(file_path)
  } else if (tools::file_ext(file_path) == "geojson") {
    data <- sf::read_sf(file_path)
  } else {
    stop("File type is not csv, rds, json or geojson. Method for loading this file could not be found.")
  }
  
  return(data)
}


#' NVDB objects for Leaflet maps
#'
#' @returns A list of nvdb_url, nvdb_attribution and nvdb_crs that are needed for using the NVDB map in Leaflet.
#' @export
#'
#' @examples
#' nvdb_objects()
nvdb_objects <- function(){
  # Use NVDB map in leaflet
  nvdb_map_url <-
    "https://nvdbcache.geodataonline.no/arcgis/rest/services/Trafikkportalen/GeocacheTrafikkJPG/MapServer/tile/{z}/{y}/{x}"
  
  nvdb_map_attribution <-
    "NVDB, Geovekst, kommunene og Open Street Map contributors (utenfor Norge)"
  
  nvdb_crs <- leaflet::leafletCRS(
    crsClass = "L.Proj.CRS", code = "EPSG:25833",
    proj4def = "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs",
    resolutions = c(
      21674.7100160867,
      10837.35500804335,
      5418.677504021675,
      2709.3387520108377,
      1354.6693760054188,
      677.3346880027094,
      338.6673440013547,
      169.33367200067735,
      84.66683600033868,
      42.33341800016934,
      21.16670900008467,
      10.583354500042335,
      5.291677250021167,
      2.6458386250105836,
      1.3229193125052918,
      0.6614596562526459,
      0.33072982812632296,
      0.16536491406316148
    ),
    origin = c(-2500000.0, 9045984.0))
  
  return(list(nvdb_url = nvdb_map_url,
              nvdb_attribution = nvdb_map_attribution,
              nvdb_crs = nvdb_crs))
}

#' Return complete traffic links within a given bounding box
#'
#' @inheritParams plot_with_bbox
#'
#' @returns A subset of the data frame given in `data`, containing the complete traffic links that overlap with the bounding box.
#' @export
#'
get_traffic_links_in_bbox <- function(data, bbox = NULL, point = NULL){
  if(is.null(bbox)){
    bbox <- bbox_around_point(point = point, dist = dist, crs = nvdb$nvdb_crs)
  }
  
  # Crop data only in bbox. This cuts across traffic links.
  cropped <- sf::st_crop(data, bbox)
  
  # Select the ids of all traffic links in area.
  link_ids <- cropped$parentTrafficLinkId
  
  # Filter out data for all such links
  subset_data <- dplyr::filter(data, parentTrafficLinkId %in% link_ids)
  
  return(subset_data)
}


split_traffic_link_id <- function(df){
  df_parsed <- df |>
    tidyr::extract(
      id,
      into = c("startPosition", "endPosition"),
      regex = "([^@-]+@?\\d*)-([^@-]+@\\d*)-.*",
      remove = FALSE
    ) %>%
    dplyr::mutate(
      startPosition = dplyr::if_else(
        !grepl("@", startPosition),
        paste0(startPosition, "@", sub(".*@", "", endPosition)),
        startPosition)
    )
  return(df_parsed)
}
