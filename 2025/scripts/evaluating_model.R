# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(xdtkit)

year <- 2025
path <- paste0(year, "/")

predictions_total <- readRDS(paste0(path, "results/predictions_total.rds"))
predictions_heavy <- readRDS(paste0(path, "results/predictions_heavy.rds"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate preapproval metrics
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
preapproval <- calculate_preapproval_metrics(data = predictions_heavy, model_name = "heavy")
print_preapproval_summary(preapproval)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Examining total AADT results
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Predictions compared to last year's predictions
plot_predictions_against_last_year(
  predictions_total, log10_axis = TRUE, color_by = "traffic_volume_source")

# Relative uncertainty from INLA
plot_prediction_relative_uncertainty_histogram(
  predictions_total, cap_cv = 1, balanced = FALSE,
  reference_lines = c(0.125, 0.3))

# Relative uncertainty from balancing
plot_prediction_relative_uncertainty_histogram(
  predictions_total, cap_cv = 1, reference_lines = c(0.1, 0.3))

# Relative uncertainty (from balancing) by data source
plot_prediction_relative_uncertainty_histogram(
  predictions_total, cap_cv = 1, facet_by = "traffic_volume_source",
  reference_lines = c(0.1, 0.3))

# Standard deviation from INLA
plot_prediction_uncertainty(predictions_total, cap_sd = 25000, balanced = FALSE,
                            log10_x = TRUE, log10_y = TRUE)
# Standard deviation from balancing
plot_prediction_uncertainty(predictions_total, cap_sd = 25000000,
                            log10_x = TRUE, log10_y = TRUE,
                            color_by = "functionClass")

predictions_total <- predictions_total |>
  dplyr::mutate(
    uncertainty_total = dplyr::case_when(
      balanced_sd/balanced_pred <= 0.1 ~ "low",
      balanced_sd/balanced_pred <= 0.3 ~ "medium",
      TRUE ~ "high"
    ),
    not_measured_low = is.na(aadt) & uncertainty_total == "low",
    is_zero = balanced_pred == 0
  )

plot_traffic_links_map(
  predictions_total,
  color_by = "not_measured_low"
)

plot_traffic_links_map(
  predictions_total,
  color_by = "uncertainty_total"
)

zero_pred <- dplyr::filter(predictions_total, balanced_pred == 0)

plot_traffic_links_map(
  predictions_total,
  color_by = "is_zero"
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Examining heavy AADT results
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Predictions compared to last year's predictions
plot_predictions_against_last_year(
  predictions_heavy, log10_axis = TRUE, color_by = "traffic_volume_source",
  heavy_vehicle = TRUE)

# Relative uncertainty from INLA
plot_prediction_relative_uncertainty_histogram(
  predictions_heavy, cap_cv = 4, balanced = FALSE, heavy_vehicle = TRUE,
  reference_lines = c(0.1, 0.5))

# Relative uncertainty from balancing
plot_prediction_relative_uncertainty_histogram(
  predictions_heavy, cap_cv = 4, heavy_vehicle = TRUE,
  reference_lines = c(0.1, 0.5))


predictions_heavy <- predictions_heavy |>
  dplyr::mutate(
    uncertainty_heavy = dplyr::case_when(
      balanced_sd_heavy/balanced_pred_heavy <= 0.2 ~ "low",
      balanced_sd_heavy/balanced_pred_heavy <= 0.5 ~ "medium",
      TRUE ~ "high"
    ),
    not_measured_low = is.na(heavyAadt) & uncertainty_heavy == "low",
    is_zero = balanced_pred_heavy == 0
  )


plot_traffic_links_map(
  predictions_heavy,
  color_by = "uncertainty_heavy",
  heavy_vehicle = TRUE
)

library(ggplot2)

ggplot(predictions, aes(x = balanced_sd/balanced_pred, y = balanced_sd_heavy/balanced_pred_heavy)) +
  geom_point() +
  xlim(0,4) +
  ylim(0,4) +
  geom_hline(yintercept = c(0.1, 0.3), color = "red")+
  geom_vline(xintercept = c(0.1, 0.3), color = "red")+
  xlab("Relative uncertainty, AADT") +
  ylab("Relative uncertainty, heavy AADT")
