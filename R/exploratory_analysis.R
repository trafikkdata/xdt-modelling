# Quick function for any two variables
make_crosstab_heatmap <- function(data, var1, var2, log_scale = FALSE) {
  crosstab <- data |>
    dplyr::count(!!sym(var1), !!sym(var2))
  
  fill_var <- if(log_scale) quo(log10(n + 1)) else quo(n)
  
  ggplot(crosstab, aes_string(x = var1, y = var2, fill = fill_var)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    geom_text(aes(label = n), color = "white", size = 3) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Cross-tab:", var1, "vs", var2))
}

# Looking at interactions and their AADT
# Function to create AADT interaction heatmaps
make_aadt_heatmap <- function(data, var1, var2, 
                              stat = "mean",  # "mean", "median", "log_mean"
                              min_n = 5,      # minimum observations to show
                              show_counts = TRUE) {
  
  # Calculate summary statistics
  summary_data <- data |>
    filter(!is.na(aadt)) |>
    group_by(!!sym(var1), !!sym(var2)) |>
    summarise(
      mean_aadt = mean(aadt, na.rm = TRUE),
      median_aadt = median(aadt, na.rm = TRUE),
      log_mean_aadt = log10(mean(aadt, na.rm = TRUE)),
      n = n(),
      .groups = "drop"
    ) |>
    filter(n >= min_n)  # Filter out sparse combinations
  
  # Choose which statistic to plot
  fill_var <- switch(stat,
                     "mean" = quo(mean_aadt),
                     "median" = quo(median_aadt), 
                     "log_mean" = quo(log_mean_aadt))
  
  stat_label <- switch(stat,
                       "mean" = "Mean AADT",
                       "median" = "Median AADT",
                       "log_mean" = "Log10(Mean AADT)")
  
  # Create the plot
  p <- ggplot(summary_data, aes_string(x = var1, y = var2, fill = quo_name(fill_var))) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient(low = "lightblue", high = "darkred", name = stat_label) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("AADT by", var1, ":", var2),
         subtitle = paste("Showing", stat_label, "| Min", min_n, "observations"))
  
  # Add text labels
  if (show_counts) {
    if (stat == "log_mean") {
      p <- p + geom_text(aes(label = paste0(round(mean_aadt), "\n(n=", n, ")")), 
                         size = 2.5, color = "white")
    } else {
      p <- p + geom_text(aes(label = paste0(round(!!fill_var), "\n(n=", n, ")")), 
                         size = 2.5, color = "white")
    }
  } else {
    p <- p + geom_text(aes(label = round(!!fill_var)), size = 3, color = "white")
  }
  
  return(p)
}


plot_all_pairs <- function(data, pairwise_combinations, plot_type, ...){
  plot_list <- list()
  for(pair in 1:ncol(pairwise_combinations)){
    #print(c(pairwise_combinations[1, pair], pairwise_combinations[2, pair]))
    plot_list[[pair]] <- plot_type(data, pairwise_combinations[1, pair], pairwise_combinations[2, pair], ...)
  }
  return(plot_list)
}