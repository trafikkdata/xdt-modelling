
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate Measurement Error Variances for Traffic Data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Calculate measurement error variance for traffic volume data
#'
#' This function computes measurement error variances based on:
#' 1. Data source type (continuous, periodic, ferry, etc.)
#' 2. Temporal coverage (for continuous and periodic measurements)
#' 3. Age of data (temporal decay)
#' 4. For derived values: error propagation from contributing segments
#'
#' @param data Data frame containing traffic measurements
#' @param aadt_col Name of column containing AADT values
#' @param source_col Name of column containing source type
#' @param year_col Name of column containing measurement year
#' @param coverage_col Name of column containing temporal coverage (0-1), 
#'        required for "Trafikkdata_continuous", "Trafikkdata_periodic", and "AutoPASS"
#' @param current_year Current year for temporal decay calculation (default: 2024)
#' @param params List of parameters (see details)
#' 
#' @details
#' Parameters list should contain:
#' - cv_base_continuous: Base CV for continuous measurements (default: 0.015)
#' - cv_base_autopass: Base CV for AutoPASS (default: 0.02)
#' - k_missing: Factor for missing data uncertainty (default: 0.5)
#' - cv_ferry: CV for ferry data (default: 0.075)
#' - cv_external: CV for external municipal data (default: 0.20)
#' - cv_bus: CV for bus data (default: 0.15)
#' - cv_annual: Annual CV for temporal decay (default: 0.02)
#' - cv_max_periodic: Maximum CV for periodic measurements (default: NULL for no cap)
#'
#' @return Vector of measurement error standard deviations (same length as input data)
#'
calculate_measurement_error <- function(
    data,
    aadt_col = "aadt",
    source_col = "traffic_volume_source",
    year_col = "traffic_volume_year",
    coverage_col = "bestDataSourceAadt_coverage",
    current_year = 2024,
    params = list()
) {
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set default parameters ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  default_params <- list(
    cv_base_continuous = 0.005,    # Base error for continuous TRP
    cv_base_periodic = 0.01,       # Base error for continuous TRP
    cv_base_autopass = 0.005,      # Base error for AutoPASS
    k_missing = 0.5,               # Factor for missing data (0.5 = trust factor curves moderately)
    cv_ferry = 0.01,               # Base error for ferry data
    cv_external = 0.1,             # 20% for external municipal data
    cv_bus = 0.3,                 # 15% for bus data
    cv_annual = 0.02,              # 2% annual temporal decay
    cv_max_periodic = NULL         # No cap on periodic CV by default
  )
  
  # Merge user params with defaults
  params <- modifyList(default_params, params)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract data columns ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  aadt <- data[[aadt_col]]
  source <- data[[source_col]]
  year <- data[[year_col]]
  coverage <- data[[coverage_col]]
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate temporal uncertainty component ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  delta_t <- pmax(0, current_year - year)  # Years since measurement
  sigma_temporal <- aadt * params$cv_annual * delta_t
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate measurement uncertainty by source type ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  sigma_measurement <- numeric(nrow(data))
  
  for (i in seq_len(nrow(data))) {
    
    src <- source[i]
    aadt_val <- aadt[i]
    cov <- coverage[i]
    
    if (is.na(src)){
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # No data ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      sigma_measurement[i] <- NA_real_
      
    } else if (src == "Trafikkdata_continuous") {
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Continuous TRP measurements ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      if (is.na(cov)) {
        stop("Coverage required for Trafikkdata_continuous at row ", i)
      }
      
      cv_base <- params$cv_base_continuous
      cv_missing <- params$k_missing * (1 - cov)
      cv_total <- sqrt(cv_base^2 + cv_missing^2)
      sigma_measurement[i] <- aadt_val * cv_total
      
    } else if (src == "Trafikkdata_periodic") {
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Periodic measurements ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      if (is.na(cov)) {
        stop("Coverage required for Trafikkdata_periodic at row ", i)
      }
      
      # Use same formula as continuous, but coverage is much lower
      cv_base <- params$cv_base_periodic
      cv_missing <- params$k_missing * (1 - cov)
      cv_total <- sqrt(cv_base^2 + cv_missing^2)
      
      # Apply cap if specified
      if (!is.null(params$cv_max_periodic)) {
        cv_total <- min(cv_total, params$cv_max_periodic)
      }
      
      sigma_measurement[i] <- aadt_val * cv_total
      
    } else if (src == "AutoPASS") {
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # AutoPASS toll stations ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      if (is.na(cov)) {
        stop("Coverage required for AutoPASS at row ", i)
      }
      
      cv_base <- params$cv_base_autopass
      cv_missing <- params$k_missing * (1 - cov)
      cv_total <- sqrt(cv_base^2 + cv_missing^2)
      sigma_measurement[i] <- aadt_val * cv_total
      
    } else if (src == "Ferry") {
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Ferry data ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      sigma_measurement[i] <- aadt_val * params$cv_ferry
      
    } else if (src == "External_municipal") {
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # External municipal data ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      sigma_measurement[i] <- aadt_val * params$cv_external
      
    } else if (src == "Bus") {
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Bus data ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      sigma_measurement[i] <- aadt_val * params$cv_bus
      
    } else if (src == "Derived") {
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Derived values ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # For derived values, uncertainty needs to be calculated separately
      # based on the contributing segments. This is handled in a separate step.
      # Set to 0 here as a placeholder
      sigma_measurement[i] <- 0
      
    } else {
      warning("Unknown source type: ", src, " at row ", i)
      sigma_measurement[i] <- NA_real_
    }
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Combine measurement and temporal uncertainty ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # For non-derived values, combine measurement and temporal components
  sigma_total <- sqrt(sigma_measurement^2 + sigma_temporal^2)
  
  # For derived values, temporal uncertainty still applies
  # (measurement uncertainty will be added later based on contributing segments)
  #sigma_total[source == "Derived"] <- sigma_temporal[source == "Derived"]
  
  return(sigma_total)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate derived value uncertainties ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Calculate measurement error for derived traffic volumes
#'
#' Derived values are calculated as combinations of other measurements
#' (e.g., v_after = v_before - v_ramp). Their measurement errors propagate
#' from the contributing segments.
#'
#' @param data Data frame with all traffic measurements
#' @param derived_formula Data frame or list describing how each derived value
#'        is calculated from other segments. Should have columns:
#'        - derived_id: ID of the derived segment
#'        - contributing_id: ID of contributing segment
#'        - sign: +1 or -1 (whether to add or subtract)
#' @param sigma_measurement Vector of measurement errors (from calculate_measurement_error)
#' @param id_col Name of column containing segment IDs
#' @param source_col Name of column containing source type
#'
#' @return Updated vector of measurement errors with derived values filled in
#'
propagate_derived_uncertainty <- function(
    data,
    derived_formula,
    sigma_measurement,
    id_col = "segment_id",
    source_col = "traffic_volume_source"
) {
  
  sigma_updated <- sigma_measurement
  
  # Get indices of derived segments
  derived_idx <- which(data[[source_col]] == "Derived")
  
  for (idx in derived_idx) {
    derived_id <- data[[id_col]][idx]
    
    # Find all contributing segments for this derived value
    contrib <- derived_formula[derived_formula$derived_id == derived_id, ]
    
    if (nrow(contrib) == 0) {
      warning("No formula found for derived segment: ", derived_id)
      next
    }
    
    # Get measurement errors for contributing segments
    contrib_sigma <- numeric(nrow(contrib))
    for (j in seq_len(nrow(contrib))) {
      contrib_id <- contrib$contributing_id[j]
      contrib_row <- which(data[[id_col]] == contrib_id)
      
      if (length(contrib_row) == 0) {
        warning("Contributing segment not found: ", contrib_id)
        contrib_sigma[j] <- NA_real_
      } else {
        contrib_sigma[j] <- sigma_measurement[contrib_row]
      }
    }
    
    # Propagate uncertainty: variances add for independent measurements
    # (sign doesn't matter for variance)
    sigma_derived_measurement <- sqrt(sum(contrib_sigma^2, na.rm = TRUE))
    
    # Combine with temporal uncertainty already in sigma_updated
    sigma_temporal_component <- sigma_updated[idx]  # This was set earlier
    sigma_updated[idx] <- sqrt(sigma_derived_measurement^2 + sigma_temporal_component^2)
  }
  
  return(sigma_updated)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example usage ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (FALSE) {
  
  # Assuming you have a data frame like this:
  # traffic_data <- data.frame(
  #   segment_id = 1:100,
  #   AADT = c(...),
  #   traffic_volume_source = c("Trafikkdata_continuous", "Trafikkdata_periodic", ...),
  #   traffic_volume_year = c(2024, 2023, ...),
  #   coverage = c(0.98, 0.04, ...)  # Fraction of year with data
  # )
  
  # Calculate measurement errors
  sigma_error <- calculate_measurement_error(
    data = data,
    aadt_col = "aadt",
    source_col = "traffic_volume_source",
    year_col = "traffic_volume_year",
    coverage_col = "bestDataSourceAadt_coverage",
    current_year = 2024
  )
  
  data$sigma_error <- sigma_error
  # If you have derived values, propagate their uncertainty
  # derived_formula <- data.frame(
  #   derived_id = c(50, 50, 51),
  #   contributing_id = c(45, 46, 47),
  #   sign = c(1, -1, 1)
  # )
  
  # sigma_error <- propagate_derived_uncertainty(
  #   data = traffic_data,
  #   derived_formula = derived_formula,
  #   sigma_measurement = sigma_error,
  #   id_col = "segment_id",
  #   source_col = "traffic_volume_source"
  # )
  
  # Add to your data
  traffic_data$measurement_error_sd <- sigma_error
  
  # Create diagonal covariance matrix for Bayesian updating
  Sigma_epsilon_prime <- diag(sigma_error^2)
}

