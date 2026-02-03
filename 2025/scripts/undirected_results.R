# Undirected results

library(dplyr)
library(ggplot2)

results2025 <- readRDS(paste0(path, "results/results2025.rds"))
prepared_traffic_links <- readRDS(paste0(path, "data-prepared/prepared_traffic_links", year, ".rds"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Aggregate directed links to undirected ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

aggregate_to_undirected <- function(directed_results, prepared_traffic_links) {
  directed_with_metadata <- dplyr::left_join(directed_results, prepared_traffic_links)
  
  directed_with_metadata |>
    dplyr::group_by(parentTrafficLinkId) |>
    dplyr::summarise(
      # Total AADT is sum of both directions
      estimatedAadt = sum(estimatedAadt, na.rm = TRUE),
      
      # For uncertainty propagation, assuming independence between directions:
      # Var(X + Y) = Var(X) + Var(Y)
      estimatedAadtStandardDeviation = sqrt(sum(estimatedAadtStandardDeviation^2, na.rm = TRUE)),
      
      # Heavy vehicle AADT (sum of both directions)
      estimatedAadtHeavy = sum(estimatedAadtHeavy, na.rm = TRUE),
      
      # Heavy vehicle uncertainty
      estimatedAadtHeavyStandardDeviation = sqrt(sum(estimatedAadtHeavyStandardDeviation^2, na.rm = TRUE)),
      
      # Last year AADT
      lastYearAadt = sum(lastYearAadt_aadt),
      lastYearAadtHeavy = sum(lastYearAadt_heavyAadt),
      
      county = dplyr::first(county),
      roadCategory = dplyr::first(roadCategory),
      functionClass = dplyr::first(functionClass),
      
      .groups = "drop"
    )
}

undirected_results <- aggregate_to_undirected(directed_results = results2025, prepared_traffic_links) |> 
  mutate(relative_uncertainty = estimatedAadtStandardDeviation/estimatedAadt,
         relative_uncertainty_heavy = estimatedAadtHeavyStandardDeviation/estimatedAadtHeavy,
         uncertainty = dplyr::case_when(
           relative_uncertainty <= 0.1 ~ "Lav",
           relative_uncertainty <= 0.3 ~ "Middels",
           TRUE ~ "Høy") |> factor(levels = c("Lav", "Middels", "Høy")),
         uncertainty_heavy = dplyr::case_when(
           relative_uncertainty_heavy <= 0.1 ~ "Lav",
           relative_uncertainty_heavy <= 0.3 ~ "Middels",
           TRUE ~ "Høy")|> factor(levels = c("Lav", "Middels", "Høy")),
         pct_change = (estimatedAadt - lastYearAadt) / lastYearAadt * 100,
         pct_change_heavy = (estimatedAadtHeavy - lastYearAadtHeavy) / lastYearAadtHeavy * 100
         )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Relative uncertainty ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot(undirected_results, aes(x = relative_uncertainty, y = relative_uncertainty_heavy)) +
  geom_point() +
  xlim(0,4) +
  ylim(0,4) +
  geom_hline(yintercept = c(0.1, 0.3), color = "hotpink")+
  geom_vline(xintercept = c(0.1, 0.3), color = "hotpink")+
  xlab("Relative uncertainty, AADT") +
  ylab("Relative uncertainty, heavy AADT")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Uncertainty heatmap: total AADT vs heavy AADT ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Count combinations
heatmap_data <- undirected_results %>%
  count(uncertainty, uncertainty_heavy)

# Create heatmap
ggplot(heatmap_data, aes(x = uncertainty_heavy, y = uncertainty, fill = n)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = n), size = 6, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "steelblue", high = "darkred") +
  labs(
    x = "Usikkerhet, ÅDT andel lange kjøretøy",
    y = "Usikkerhet, ÅDT total",
    title = "Fordeling av trafikklenker i usikkerhetskategorier",
    fill = "Antall trafikklenker"
  ) +
  coord_equal() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Uncertainty by functional road class ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Count combinations
uncertainty_by_class <- undirected_results %>%
  count(functionClass, uncertainty)

# Stacked bar chart
ggplot(uncertainty_by_class, aes(x = functionClass, y = n, fill = uncertainty)) +
  geom_col() +
  scale_fill_manual(
    values = c("Lav" = "green3", "Middels" = "orange", "Høy" = "red")
  ) +
  labs(
    x = "Funksjonsklasse",
    y = "Antall trafikklenker",
    title = "Usikkerhet fordelt på funksjonsklasse",
    fill = "Usikkerhet"
  ) +
  theme_minimal()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Uncertainty by road category ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Count combinations
uncertainty_by_class <- undirected_results %>%
  count(roadCategory, uncertainty)

# Stacked bar chart
ggplot(uncertainty_by_class, aes(x = roadCategory, y = n, fill = uncertainty)) +
  geom_col() +
  scale_fill_manual(
    values = c("Lav" = "green3", "Middels" = "orange", "Høy" = "red")
  ) +
  labs(
    x = "Vegkategori",
    y = "Antall trafikklenker",
    title = "Usikkerhet fordelt på vegkategori",
    fill = "Usikkerhet"
  ) +
  theme_minimal()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Year-over-year change distribution ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Histogram of changes
ggplot(undirected_results, aes(x = pct_change)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  facet_wrap(~functionClass, scales = "free_y") +
  labs(
    x = "Endring fra i fjor (%)",
    y = "Antall trafikklenker",
    title = "Fordeling av endringer fra forrige års ÅDT"
  ) +
  xlim(-100, 100) +
  theme_minimal()
