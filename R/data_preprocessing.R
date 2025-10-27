# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Main preprocessing function ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
    # fill_missing_entries() %>% 
    process_list_columns() %>% 
    standardize_data_types() %>% 
    # engineer_features() %>% 
    # scale_numeric_features(scale_cols = scale_cols) %>% 
    add_busstop_counts(stops_on_traffic_links_data = stops_on_traffic_links, 
                       bus_counts_data = bus_counts, 
                       lowest_certainty = lowest_certainty, 
                       no_of_days = no_of_days,
                       location_uncertainties = location_uncertainties) %>% 
    round_and_check_aadt() %>% 
    assign_traffic_volume_source(current_year = 2024)
  
  check_data_completeness(df)

  return(df)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Process traffic volume ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
    tidyr::unnest_wider(bestDataSourceAadt, names_sep = "_") %>% 
    tidyr::unnest_wider(lastYearAadt, names_sep = "_")
  
  return(df_flattened)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Process_list_columns ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

process_list_columns <- function(df){
  # All list extraction logic
  names(df)[sapply(df, is.list)]
  
  df <- dplyr::mutate(df, 
               dplyr::across(c(functionalRoadClass, functionClass), 
                      extract_smallest_element),
               dplyr::across(c(municipalityIds, countyIds, 
                               roadCategory, roadSystemReferences), 
                      safely_extract_first_element))
  
  df <- remove_list_columns(df)
  
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

remove_list_columns <- function(df){
  list_cols <- sapply(df, is.list)
  df_filtered <- df[, !list_cols]
  return(df_filtered)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Standardize data types ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

standardize_data_types <- function(df){
  # Final type conversions
  numeric_cols <- c("bestDataSourceAadt_trafficVolumeValue", "length",
                    "lastYearAadt_trafficVolumeValue")
  integer_cols <- c("yearAppliesTo", "bestDataSourceAadt_year",
                    "lastYearAadt_year")
  factor_cols <- c('functionalRoadClass', 'functionClass',
                   'highestSpeedLimit', "lowestSpeedLimit", 
                   "municipalityIds", "countyIds",'roadCategory', 
                   'maxLanes', 'minLanes')
  logical_cols <- c('isNorwegianScenicRoute', 'isFerryRoute', "isRamp", 
                    "isBlocked", "isInvalid", "hasOnlyPublicTransportLanes")
  
  df <- df %>% 
    dplyr::mutate(dplyr::across(dplyr::all_of(numeric_cols), as.numeric),
                  dplyr::across(dplyr::all_of(integer_cols), as.integer),
                  dplyr::across(dplyr::all_of(factor_cols), as.factor),
                  dplyr::across(dplyr::all_of(logical_cols), as.logical))

  return(df)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add busstop counts ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
  
  df <- df %>% dplyr::left_join(certain_counts, 
                                by = c("id", "parentTrafficLinkId"))
  
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
      stopsServeDifferentBuses = dplyr::first(stopsServeDifferentBuses), # Assuming consistent within group
      n_stops = dplyr::n(),
      mean_count = mean(no_of_buses, na.rm = TRUE),
      sum_count = sum(no_of_buses, na.rm = TRUE),
      sd_count = dplyr::if_else(dplyr::n() > 1, sd(no_of_buses, na.rm = TRUE), NA_real_),
      
      # Apply the appropriate method based on segment type
      bus_aadt = dplyr::case_when(
        is.na(stopsServeDifferentBuses) ~ mean_count/no_of_days,           # Single stop: use the value
        stopsServeDifferentBuses == "FALSE" ~ mean_count/no_of_days,        # Rural: average
        stopsServeDifferentBuses == "TRUE" ~ sum_count/no_of_days           # Terminal: sum
      ),
      
      # Calculate uncertainty based on method
      bus_sd = dplyr::case_when(
        is.na(stopsServeDifferentBuses) ~ cv_uncertainty * bus_aadt/no_of_days,                    # Single: CV method
        stopsServeDifferentBuses == "FALSE" ~ dplyr::coalesce(sd_count, cv_uncertainty * bus_aadt)/no_of_days, # Rural: use SD, fallback to CV
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
    dplyr::mutate(
      bus_sd = dplyr::case_when(
        is.na(stopCertainty) | stopCertainty == "" ~ bus_sd, 
        stopCertainty == "High" ~ bus_sd + location_uncertainties[1]*bus_aadt,
        stopCertainty == "Medium" ~ bus_sd + location_uncertainties[2]*bus_aadt,
        stopCertainty == "Low" ~ bus_sd + location_uncertainties[3]*bus_aadt
    ))
  return(bus_counts_with_scaled_uncertainty)
}


quality_check_bus <- function(){
  # Remove data for bus stops where the counts are less than 10 or more than 1000. 
  # These are both very unlikely.
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Round and check AADT ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

round_and_check_aadt <- function(df){
  df$aadt <- round(df$aadt)
  df$aadt[df$aadt < 0] <- 0
  
  return(df)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Assign traffic volume source ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

assign_traffic_volume_source <- function(df, current_year){
  df <- df %>% 
    dplyr::mutate(
      traffic_volume_source = dplyr::case_when(
        bestDataSourceAadt_trafficVolumeType == "DERIVED" ~ "Derived",
        bestDataSourceAadt_sourceType == "EXTERNAL" & isFerryRoute ~ "Ferry",
        bestDataSourceAadt_sourceType == "EXTERNAL" ~ "External_municipal",
        bestDataSourceAadt_sourceType == "TOLL_STATION_AUTOPASS" ~ "AutoPASS",
        bestDataSourceAadt_sourceType == "TRAFIKKDATA" & 
          bestDataSourceAadt_registrationFrequency == "PERIODIC" ~ "Trafikkdata_periodic",
        bestDataSourceAadt_sourceType == "TRAFIKKDATA" & 
          bestDataSourceAadt_registrationFrequency == "CONTINUOUS" ~ "Trafikkdata_continuous",
        is.na(bestDataSourceAadt_trafficVolumeValue) & !is.na(aadt) ~ "Bus"
      ),
      traffic_volume_year = dplyr::case_when(
        traffic_volume_source == "Bus" ~ current_year,
        .default = bestDataSourceAadt_year)
      )
  return(df)
}



# Tests ----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check raw data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

check_raw_data <- function(raw_data){
  cat("Checking column names... \n")
  column_names <- c("id", "parentTrafficLinkId", "isTrafficWithMetering", 
                    "functionalRoadClass", "functionClass", "highestSpeedLimit", 
                    "lowestSpeedLimit", "isNorwegianScenicRoute", 
                    "isFerryRoute", "isRamp", "isBlocked", "tollStationIds",
                    "isInvalid", "yearAppliesTo", "startTrafficNodeId",
                    "endTrafficNodeId", "municipalityIds", "countyIds",
                    "roadSystemReferences", "roadCategory", "roadLinkIds", 
                    "roadNodeIds", "length", "roadPlacements", "maxLanes", "minLanes", 
                    "hasOnlyPublicTransportLanes", "associatedTrpIds",
                    "lastYearAadt", "bestDataSourceAadt", "trafficVolumes")
  if(setequal(colnames(raw_data), column_names)){
    cat("All column names are as expected.")
    }else{
      warning("Unexpected column names.")
      unexpected <- setdiff(names(raw_data), column_names)
      missing <- setdiff(column_names, names(raw_data))
      
      if (length(unexpected) > 0) {
        cat("Columns in raw data that are not expected:\n")
        print(unexpected)
      }
      
      if (length(missing) > 0) {
        cat("Expected columns not found in raw data:\n")
        print(missing)
      }
  }
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validate processed data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

validate_processed_data <- function(){
  # Quality checks
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check required columns ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

check_required_columns <- function(){
  # Quality checks
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validate data ranges ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

validate_data_ranges <- function(){
  # AADT > 0, etc.
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check data completeness ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

check_data_completeness <- function(df){
  # Missing values?
  # Check that the number of missing values is the same for predictions and uncertainties.
  missing_preds <- sum(is.na(df$aadt))
  missing_sds <- sum(is.na(df$aadt_sd))
  if(missing_sds != missing_preds){
    warning("The number of missing values in 'aadt' is not the same as the number of missing values in 'aadt_sd'. This will lead to problems in the balancing procedure.")
  }
  # Check for missing values in relevant columns
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Validate data types ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

validate_data_types <- function(){
  # Check that columns have the right type.
}
