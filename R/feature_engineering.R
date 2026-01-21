# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Main feature engineering function ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

engineer_features <- function(df, columns_to_fill = NULL, scale_cols){
  df <- df |> 
    fill_missing_entries(columns_to_fill = columns_to_fill) |> 
    merge_sparse_categories() |> 
    add_county() |> 
    add_roadSystem() |> 
    add_logLength() |> 
    scale_numeric_features(scale_cols = scale_cols)
  
  return(df)
    
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fill missing values ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fill_missing_entries <- function(df, columns_to_fill = NULL, method = "unknown") {
  # Identify columns to fill
  if (is.null(columns_to_fill)) {
    cols_with_missing <- names(which(colSums(is.na(df)) > 0))
    columns_to_fill <- cols_with_missing[
      !stringr::str_starts(cols_with_missing, "bestDataSourceAadt_")
    ]
  }
  
  # Fill missing values based on method
  if (method == "unknown") {
    # Simple replacement 
    df <- df |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(columns_to_fill),
        ~ tidyr::replace_na(.x, "unknown")
      ))
    
  } else if (method == "mode") {
    # Mode imputation
    Mode <- function(x) {
      ux <- unique(x[!is.na(x)])  # Exclude NAs when finding mode
      if (length(ux) == 0) return(NA)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    mode_values <- df |>
      dplyr::select(dplyr::all_of(columns_to_fill)) |>
      dplyr::summarise(dplyr::across(dplyr::everything(), Mode))
    
    df <- df |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(columns_to_fill),
        ~ dplyr::coalesce(.x, mode_values[[dplyr::cur_column()]])
      ))
    
  } else {
    stop("method must be either 'mode' or 'unknown'")
  }
  
  return(df)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge sparse categories ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

merge_sparse_categories <- function(df){
  df <- df |>
    dplyr::mutate(
      functionalRoadClass_merged = as.factor(dplyr::case_when(
        functionalRoadClass %in% c("7", "8", "9") ~ "7+",
        TRUE ~ functionalRoadClass)),
      
      highestSpeedLimit_merged = as.factor(dplyr::case_when(
        highestSpeedLimit %in% c("100", "110") ~ "100+",
        TRUE ~ highestSpeedLimit)),
      
      lowestSpeedLimit_merged = as.factor(dplyr::case_when(
        lowestSpeedLimit %in% c("90", "100", "110") ~ "90+",
        TRUE ~ lowestSpeedLimit)),
      
      maxLanes_merged = as.factor(dplyr::case_when(
        maxLanes %in% c("4", "5", "6", "7") ~ "4+",
        TRUE ~ maxLanes)),
      
      minLanes_merged = as.factor(dplyr::case_when(
        minLanes %in% c("3","4", "5", "6", "7") ~ "3+",
        TRUE ~ minLanes))
    )
  
  return(df)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add county variable ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

add_county <- function(df){
  # County names from codes
  county_mapping <- c(
    "3" = 'Oslo',
    "11" = 'Rogaland',
    "15" = 'Møre og Romsdal',
    "18" = 'Nordland',
    "31" = 'Østfold',
    "32" = 'Akershus',
    "33" = 'Buskerud',
    "34" = 'Innlandet',
    "39" = 'Vestfold',
    "40" = 'Telemark',
    "42" = 'Agder',
    "46" = 'Vestland',
    "50" = 'Trøndelag',
    "55" = 'Troms',
    "56" = 'Finnmark'
  )
  
  df |>
    dplyr::mutate(county = as.factor(county_mapping[as.character(countyIds)]))
}

add_county <- function(df, county_number_col) {
  # Named vector for mapping
  county_map <- c(
    "3" = 'Oslo',
    "11" = 'Rogaland',
    "15" = 'Møre og Romsdal',
    "18" = 'Nordland',
    "31" = 'Østfold',
    "32" = 'Akershus',
    "33" = 'Buskerud',
    "34" = 'Innlandet',
    "39" = 'Vestfold',
    "40" = 'Telemark',
    "42" = 'Agder',
    "46" = 'Vestland',
    "50" = 'Trøndelag',
    "55" = 'Troms',
    "56" = 'Finnmark'
  )
  
  df$county_name <- county_map[as.character(df[[county_number_col]])]
  return(df)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add roadSystem variable ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

add_roadSystem <- function(df){
  df |>
    dplyr::mutate(roadSystem = as.factor(gsub(" .*$", "", roadSystemReferences)))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add logLength variable ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

add_logLength <- function(df){
  df |>
    dplyr::mutate(logLength = log(length))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Scale numeric features ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

scale_numeric_features <- function(df, scale_cols){
  df[scale_cols] <- scale(df[scale_cols])
  return(df)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Identify unbalancable nodes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

identify_unbalancable_nodes <- function(){
  data <- readRDS("data/processed/engineered_data.rds")
  nodes_raw <- sf::read_sf("data/raw/traffic-nodes-2024.geojson")
  
  # Unnest to get one row per node-traffic link combination
  node_traffic_links <- nodes_raw |> sf::st_drop_geometry() |> 
    dplyr::select(id, connectedTrafficLinkIds, roadSystems) |>
    tidyr::unnest(connectedTrafficLinkIds)
  
  # Join with data to get road systems from traffic links
  node_covered_systems <- node_traffic_links |>
    dplyr::left_join(data |> dplyr::select(parentTrafficLinkId, roadSystem) |> 
                       dplyr::distinct(), 
              by = c("connectedTrafficLinkIds" = "parentTrafficLinkId")) |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      roadSystems = list(first(roadSystems)),
      n_roadSystems = length(first(roadSystems)),
      trafficLinkRoadSystems = list(unique(roadSystem)),
      n_trafficLinkRoadSystems = length(unique(roadSystem))
    )
  
  nodes <- nodes_raw |> dplyr::left_join(node_covered_systems)

  nodes <- nodes |> 
    dplyr::mutate(
      number_of_traffic_links = lengths(connectedTrafficLinkIds),
      number_of_candidate_links = lengths(connectedTrafficLinkCandidateIds),
      # I intersections
      i_intersection = numberOfIncomingLinks == 2 & 
        numberOfOutgoingLinks == 2 & 
        numberOfUndirectedLinks == 2 & 
        n_roadSystems > n_trafficLinkRoadSystems,
      # Incomplete nodes
      unbalancable_node = number_of_candidate_links > number_of_traffic_links)
  
  return(nodes)
}
