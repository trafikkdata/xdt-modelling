# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Generate validation reports ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(quarto)
library(dplyr)

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

saveRDS(undirected_results, paste0(path, "results/undirected_results", year, ".rds"))

# Get unique combinations of road category and county
report_combinations <- undirected_results |> 
  #distinct(roadCategory, county) %>%
  #arrange(county, roadCategory)
  distinct(county) |> 
  arrange(county) 

# Function to render one report
render_validation_report <- function(county_name){#, road_cat) {
  output_path <- file.path("output", glue::glue("validation_report_{county_name}.pdf"))
  
  quarto_render(
    input = paste0(year, "/reports/validation_report_template.qmd"),
    execute_params = list(
      county = county_name
    ),
    output_file = glue::glue("validation_report_{county_name}.pdf")
  )
  # Move the file to output directory
  file.rename(
    glue::glue("validation_report_{county_name}.pdf"),
    output_path
  )
}

# Generate all reports
purrr::walk(
  report_combinations$county,
  #report_combinations$roadCategory,
  render_validation_report
)
