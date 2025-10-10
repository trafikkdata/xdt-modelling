# Approval methods


# Testing new approval function -----

uretta <- trondelag_in_one$diagnostics$approval$uretta
plot_traffic_links(trondelag_cluster$diagnostics$approval$uretta)


metrics <- calculate_approval_metrics(data = trondelag_cluster$data, data_manual = aadt2024)

plot_directed_links(trondelag_cluster$data)

trondelag_in_one$data <- trondelag_cluster$data %>% 
  mutate(over_balanced = ifelse(abs(balanced_pred-aadt) > 500, balanced_pred-aadt, 0))

over_balanced <- filter(trondelag_in_one$data, abs(balanced_pred-aadt) > 500)
plot_directed_links(over_balanced)


plot_undirected_links(df = metrics$undirected_data)

plot_undirected_links(df = metrics$segments_without_counters, column_to_plot = "eale")

unreasonable_eale <- filter(metrics$segments_without_counters, eale > 0.5)
plot_undirected_links(df = unreasonable_eale)
