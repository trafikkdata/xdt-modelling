# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Main feature engineering function ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

engineer_features <- function(df, columns_to_fill = NULL, scale_cols){
  df <- df %>% 
    fill_missing_entries(columns_to_fill = columns_to_fill) %>% 
    merge_sparse_categories() %>% 
    add_county() %>% 
    add_roadSystem() %>% 
    add_logLength() %>% 
    scale_numeric_features(scale_cols = scale_cols)
  
  return(df)
    
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fill missing values ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fill_missing_entries <- function(df, columns_to_fill = NULL){
  if(is.null(columns_to_fill)){
    cols_with_missing <- names(which(colSums(is.na(df)) > 0))
    columns_to_fill <- cols_with_missing[
      !stringr::str_starts(cols_with_missing, "bestDataSourceAadt_")]
  }
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Handle NA's
  mode_df <- df %>% dplyr::select(dplyr::all_of(columns_to_fill)) %>% 
    dplyr::summarise(dplyr::across(dplyr::everything(), Mode)) %>% 
    as.list()
  df <- df %>% tidyr::replace_na(mode_df)
  return(df)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
