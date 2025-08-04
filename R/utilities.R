#' Add geometry to traffic links
#'
#' @param df Data frame without geometry
#' @param id_name The name of the id column in df.
#'
#' @returns The original data frame df, with and added geometry column. The return object has class sf.
#' @export
#'
#' @examples
add_geometry_to_traffic_links <- function(df, id_name = "id"){
  # Input: dataframe with id column.
  # Output: the same data frame, now with geometry column. 
  
  geom <- readRDS("data/processed/traffic_link_geometries.rds")
  
  # Create a named vector for the join
  join_vector <- setNames("id", id_name)
  
  df_geom <- dplyr::full_join(df, geom, by = join_vector) |> sf::st_as_sf()
  
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