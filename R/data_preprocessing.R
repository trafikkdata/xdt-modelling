#' Preprocess data
#'
#' @param raw_data Raw, unprocessed traffic count data
#' @param year 
#' @param scale_cols 
#' @param stops_on_traffic_links_data 
#' @param bus_counts_data 
#' @param lowest_certainty 
#' @param no_of_days 
#' @param location_uncertainties 
#' @param verb 
#'
#' @returns
#' @export
#'
#' @examples
preprocess_traffic_data <- function(raw_data, 
                                    year, 
                                    scale_cols = NULL, 
                                    stops_on_traffic_links_data,
                                    bus_counts_data, 
                                    lowest_certainty = "Medium",
                                    no_of_days = 365,
                                    location_uncertainties = c(0, 0.5, 1.5),
                                    verb = FALSE){
  
  check_raw_data(raw_data = raw_data)
  
  df <- raw_data %>% 
    process_traffic_volume(year = year) %>% 
    fill_missing_entries() %>% 
    process_list_columns() %>% 
    standardize_data_types() %>% 
    engineer_features() %>% 
    scale_numeric_features(scale_cols = scale_cols) %>% 
    add_busstop_counts(stops_on_traffic_links_data = stops_on_traffic_links, 
                       bus_counts_data = bus_counts, 
                       lowest_certainty = lowest_certainty, 
                       no_of_days = no_of_days,
                       location_uncertainties = location_uncertainties)

  return(df)
}




process_traffic_volume <- function(df, year){
  # If data is from 2024 or later, it contains the variable "bestDataSourceAadt", which is a nested data frame column.
  # This column is derived from the "trafficVolumes" column.
  if(year < 2024){
    df <- get_best_traffic_volume(df)
  }
  if(year >= 2024){
    df <- flatten_df(df)
  }
  return(df)
}

get_best_traffic_volume <- function(){
  # Handle the nested dataframes containing traffic volumes
  # (only for data before 2024)
}

flatten_df <- function(df){
  df_flattened <- df %>%
    tidyr::unnest_wider(bestDataSourceAadt, names_sep = "_")
  
  return(df_flattened)
}




fill_missing_entries <- function(df, columns_to_fill = NULL){
  if(is.null(columns_to_fill)){
    cols_with_missing <- names(which(colSums(is.na(df)) > 0))
    columns_to_fill <- cols_with_missing[!stringr::str_starts(cols_with_missing, "bestDataSourceAadt_")]
  }
  
  # Handle NA's
  mode_df <- df %>% select(all_of(columns_to_fill)) %>% 
    summarise(across(everything(), Mode)) %>% 
    as.list()
  df <- df %>% tidyr::replace_na(mode_df)
  return(df)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}




process_list_columns <- function(df){
  # All list extraction logic
  names(df)[sapply(df, is.list)]
  
  df <- mutate(df, 
               across(c(functionalRoadClass, functionClass), 
                      extract_smallest_element),
               across(c(municipalityIds, countyIds, roadCategory, roadSystemReferences), 
                      safely_extract_first_element))
  return(df)
}

extract_smallest_element <- Vectorize(function(x){
  if(is.vector(x)){
    return(min(x))
  }else{
    return(x)
  }
})

safely_extract_first_element <- Vectorize(function(x){
  if(is.vector(x)|is.data.frame(x)){return(x[1])}
  if(is.character(x)|is.numeric(x)|is.null(x)){return(x)}
  warning("Entry was not vector, character, numeric or null.")
})




standardize_data_types <- function(df){
  # Final type conversions
  numeric_cols <- c("bestDataSourceAadt_trafficVolumeValue", 'numberOfEstablishments',
                    'numberOfEmployees', 'urbanRatio', 'numberOfInhabitants')
  integer_cols <- c("bestDataSourceAadt_year")
  factor_cols <- c('roadCategory', 'highestSpeedLimit', 
                   'functionalRoadClass', 'functionClass',
                   'maxLanes', 'minLanes', 'roadSystemReferences')
  logical_cols <- c('isNorwegianScenicRoute', 'isFerryRoute')
  
  df <- df %>% 
    mutate(across(all_of(numeric_cols), as.numeric),
           across(all_of(integer_cols), as.integer),
           across(all_of(factor_cols), as.factor),
           across(all_of(logical_cols), as.logical))
  # 
  # # This raises a warning related to the conversion of prelimAadt to numeric. 
  # # The following displays the before/after values that raised this error.
  # ind1 <- is.na(df$prelimAadt)
  # ind2 <- is.na(df_correct_types$prelimAadt)
  # differing <- which(ind1 != ind2)
  # if(length(differing) != 0){
  #   cat("\nThe conversion of prelimAadt caused the warning 'NAs introduced by coercion'.\n") 
  #   cat("Here are the values that caused the warning:\n")
  # }
  # for(different in differing){
  #   cat("Value in original df:\n")
  #   print(df[different, "prelimAadt"])
  #   cat("Value in converted df:\n")
  #   print(df_correct_types[different, "prelimAadt"])
  # }
  return(df)
}




engineer_features <- function(df){
  # logLength, etc.
  
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
  
  df <- df %>%
    mutate(county = county_mapping[as.character("countyIds")],
           roadSystem = gsub(" .*$", "", roadSystemReferences))
  
  return(df)
}




scale_numeric_features <- function(df, scale_cols){
  df[scale_cols] <- scale(df[scale_cols])
  return(df)
}




add_busstop_counts <- function(df, 
                               stops_on_traffic_links_data, 
                               bus_counts_data,
                               lowest_certainty, 
                               no_of_days, 
                               location_uncertainties){
  bus_counts_on_traffic_links <- connect_busstop_counts_to_traffic_links(
    stops_on_traffic_links_data, 
    bus_counts, 
    no_of_days = no_of_days,
    location_uncertainties = location_uncertainties)
  
  # "lowest_certainty" can be low, medium, high, or only_on_link.
  possible_certainties <- factor(c("Low", "Medium", "High", "Only_on_link"), 
                                 levels = c("Low", "Medium", "High", "Only_on_link"),
                                 ordered = TRUE)
  certainties_to_include <- possible_certainties[possible_certainties >= lowest_certainty]
  
  certain_counts <- bus_counts_on_traffic_links %>% 
    dplyr::filter(is.na(stopCertainty) | stopCertainty == "" | 
                    stopCertainty %in% certainties_to_include)
  
  df <- df %>% left_join(certain_counts, by = c("id", "parentTrafficLinkId"))
  
  bus_ids <- which(!is.na(df$bus_aadt))
  
  # Initialize the final column to be the traffic volume column, and then add 
  # bus counts for the missing values.
  df$aadt <- df$bestDataSourceAadt_trafficVolumeValue
  df$aadt[is.na(df$aadt)] <- df$bus_aadt[is.na(df$aadt)]
  
  df$aadt_sd <- df$bestDataSourceAadt_correctedStandardError
  df$aadt_sd[is.na(df$aadt_sd)] <- df$bus_sd[is.na(df$aadt_sd)]
  #TODO: Need to do some more stuff with the sd here. 
  
  return(df)

}

connect_busstop_counts_to_traffic_links <- function(
    stops_on_traffic_links_data, bus_counts_data,
    no_of_days,
    location_uncertainties){
  
  # This data is downloaded from EnTur and contains two columns, 
  # the bus stop id's (quay id) and the count.
  bus_counts <- bus_counts_data
  
  # This data is manually created by visually connecting the traffic links that
  # only have public transport, to the bus stops on the given traffic link.
  stops_on_traffic_links <- stops_on_traffic_links_data %>% 
    dplyr::select(id, parentTrafficLinkId, stopPointRef, stopOnTrafficLink, 
                  stopCertainty, stopsServeDifferentBuses, trikkestopp, comment) 
  
  # Some traffic links have multiple bus stops.
  stops_on_traffic_links_long <- stops_on_traffic_links %>% 
    dplyr::mutate(stopPointRef = strsplit(stopPointRef, ", ")) %>% 
    tidyr::unnest(cols = stopPointRef) %>% 
    dplyr::mutate(stopPointRef = trimws(stopPointRef))
  
  # Set the coefficient of variation (CV):
  cv_uncertainty <- 1.06
  
  # We join the stops on the traffic links to the count data by stop point id, 
  # and then summarize across multiple bus stops by mean and sum.
  # Some places it makes sense to take the sum, while other places it makes 
  # sense to take the sum, depending on whether the same buses stop on all the 
  # stops along the link or not. 
  bus_counts_on_traffic_links <- stops_on_traffic_links_long %>% 
    dplyr::full_join(bus_counts, by = "stopPointRef") %>% 
    dplyr::group_by(id) %>% 
    dplyr::summarise(
      stopsServeDifferentBuses = first(stopsServeDifferentBuses), # Assuming consistent within group
      n_stops = n(),
      mean_count = mean(no_of_buses, na.rm = TRUE),
      sum_count = sum(no_of_buses, na.rm = TRUE),
      sd_count = if_else(n() > 1, sd(no_of_buses, na.rm = TRUE), NA_real_),
      
      # Apply the appropriate method based on segment type
      bus_aadt = case_when(
        is.na(stopsServeDifferentBuses) ~ mean_count/no_of_days,           # Single stop: use the value
        stopsServeDifferentBuses == "FALSE" ~ mean_count/no_of_days,        # Rural: average
        stopsServeDifferentBuses == "TRUE" ~ sum_count/no_of_days           # Terminal: sum
      ),
      
      # Calculate uncertainty based on method
      bus_sd = case_when(
        is.na(stopsServeDifferentBuses) ~ cv_uncertainty * bus_aadt/no_of_days,                    # Single: CV method
        stopsServeDifferentBuses == "FALSE" ~ coalesce(sd_count, cv_uncertainty * bus_aadt)/no_of_days, # Rural: use SD, fallback to CV
        stopsServeDifferentBuses == "TRUE" ~ cv_uncertainty * sqrt(n_stops) * bus_aadt/no_of_days   # Terminal: error propagation
      ),
      
      .groups = "drop"
      ) %>% 
    dplyr::full_join(stops_on_traffic_links, by = c("id", "stopsServeDifferentBuses")) %>% 
    add_stop_location_uncertainty(location_uncertainties = location_uncertainties)
  
  return(bus_counts_on_traffic_links)
}

add_stop_location_uncertainty <- function(bus_counts_on_traffic_links, 
                                          location_uncertainties = location_uncertainties){
  bus_counts_with_scaled_uncertainty <- bus_counts_on_traffic_links %>% 
    mutate(bus_sd = case_when(
      stopCertainty == "High" ~ bus_sd + location_uncertainties[1]*bus_aadt,
      stopCertainty == "Medium" ~ bus_sd + location_uncertainties[2]*bus_aadt,
      stopCertainty == "Low" ~ bus_sd + location_uncertainties[3]*bus_aadt
    ))
  return(bus_counts_with_scaled_uncertainty)
}


# Tester


check_raw_data <- function(raw_data){
  cat("Checking column names... ")
  column_names <- c("id", "parentTrafficLinkId", "isTrafficWithMetering", 
                    "functionalRoadClass", "functionClass", "highestSpeedLimit", 
                    "lowestSpeedLimit", "isNorwegianScenicRoute", 
                    "isFerryRoute", "isRamp", "isBlocked", "tollStationIds",
                    "isInvalid", "yearAppliesTo", "startTrafficNodeId",
                    "endTrafficNodeId", "municipalityIds", "countyIds",
                    "roadSystemReferences", "roadCategory", "roadLinkIds", 
                    "roadNodeIds", "length", "maxLanes", "minLanes", 
                    "hasOnlyPublicTransportLanes", "urbanRatio", 
                    "numberOfEstablishments", "numberOfEmployees", 
                    "numberOfInhabitants", "associatedTrpIds", "lastYearAadt",
                    "bestDataSourceAadt", "trafficVolumes")
  if(all(colnames(raw_data) == column_names)){
    cat("All column names are as expected.")
  }else{
    warning("Unexpected column names.")
    print(cbind("Expected column names" = column_names, 
                "colnames(raw_data)" = colnames(raw_data)))
  }
  
  
}

validate_processed_data <- function(){
  # Quality checks
}

check_required_columns <- function(){
  # Quality checks
}

validate_data_ranges <- function(){
  # AADT > 0, etc.
}

check_data_completeness <- function(){
  # Missing values?
}

validate_data_types <- function(){
  # Check that columns have the right type.
}
