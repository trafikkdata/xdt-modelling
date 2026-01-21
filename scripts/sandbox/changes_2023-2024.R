
library(dplyr)
library(ggplot2)
library(leaflet)

aadt2024 <- read.csv("data/raw/traffic-links-aadt-data-2024.csv")
data <- readRDS("data/processed/engineered_data.rds")


aadt2024 <- aadt2024 %>% 
  mutate(ÅDT.2023 = as.numeric(ÅDT.fjorårets),
         ÅDT.2024 = as.numeric(ÅDT.offisiell),
         data.2023 = if_else(Datagrunnlag == "CONTINUOUS_REGISTRATION", ÅDT.2023, NA),
         data.2024 = if_else(Datagrunnlag == "CONTINUOUS_REGISTRATION", ÅDT.2024, NA),
         pred.2023 = if_else(Datagrunnlag == "CONTINUOUS_REGISTRATION", NA, ÅDT.2023),
         pred.2024 = if_else(Datagrunnlag == "CONTINUOUS_REGISTRATION", NA, ÅDT.2024)
         ) %>% 
  add_county(county_number_col = "Fylkesnr")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Population-level summaries: 2023 vs 2024 ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Total network AADT
total_summary <- aadt2024 |>
  summarise(
    total_2023 = sum(ÅDT.2023, na.rm = TRUE),
    total_2024 = sum(ÅDT.2024, na.rm = TRUE),
    difference = total_2024 - total_2023,
    pct_change = 100 * (total_2024 - total_2023) / total_2023
  )

print(total_summary)

# Weighted average by road length
weighted_summary <- aadt2024 |>
  summarise(
    weighted_mean_2023 = weighted.mean(ÅDT.2023, Strekningslengde..m., na.rm = TRUE),
    weighted_mean_2024 = weighted.mean(ÅDT.2024, Strekningslengde..m., na.rm = TRUE),
    difference = weighted_mean_2024 - weighted_mean_2023,
    pct_change = 100 * (weighted_mean_2024 - weighted_mean_2023) / weighted_mean_2023
  )

print(weighted_summary)

# Percentile stability
percentile_summary <- aadt2024 |>
  summarise(
    across(
      c(ÅDT.2023, ÅDT.2024),
      list(
        p10 = ~quantile(., 0.10, na.rm = TRUE),
        p25 = ~quantile(., 0.25, na.rm = TRUE),
        p50 = ~quantile(., 0.50, na.rm = TRUE),
        p75 = ~quantile(., 0.75, na.rm = TRUE),
        p90 = ~quantile(., 0.90, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) |>
  tidyr::pivot_longer(
    everything(),
    names_to = c("year", "percentile"),
    names_pattern = "ÅDT\\.(\\d{4})_(p\\d{2})"
  ) |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  mutate(
    pct_change = 100 * (`2024` - `2023`) / `2023`
  )

print(percentile_summary)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Visualizing distribution stability ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(ggplot2)
library(tidyr)

# Prepare data in long format for plotting
aadt_long <- aadt2024 |>
  select(ÅDT.2023, ÅDT.2024, pred.2023, pred.2024, data.2023, data.2024) |>
  pivot_longer(
    everything(),
    names_to = c("type", "year"),
    names_pattern = "(.+)\\.(\\d{4})",
    values_to = "aadt"
  ) |>
  mutate(
    type = case_when(
      type == "ÅDT" ~ "ÅDT",
      type == "pred" ~ "Estimert",
      type == "data" ~ "Registrert"
    )
  )

aadt_plot <- filter(aadt_long, type %in% c("Registrert", "Estimert"))

# Density ridges 
library(ggridges)

ggplot(aadt_plot, aes(x = aadt + 1, y = 1, fill = year)) +
  geom_density_ridges(alpha = 0.7) +
  scale_x_log10(labels = scales::comma) +
  scale_fill_manual(values = c("darkred", "goldenrod")) +
  labs(
    title = "ÅDT fordeling: 2023 vs 2024",
    x = "ÅDT (log skala)",
    y = "",
    fill = "År"
  ) +
  facet_grid(~ type) +
  theme_minimal() 

# Scatter plot

# For predicted data
pred_scatter <- aadt2024 |>
  select(pred.2023, pred.2024, Vegkategori, county_name) |>
  mutate(type = "Estimert")

# For measured data  
data_scatter <- aadt2024 |>
  select(data.2023, data.2024, Vegkategori, county_name) |>
  rename(pred.2023 = data.2023, pred.2024 = data.2024) |>
  mutate(type = "Registrert")

# Combine them
scatter_data <- bind_rows(pred_scatter, data_scatter)

# Now plot
ggplot(scatter_data, aes(x = pred.2024 + 1, y = pred.2023 + 1)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkred") +
  geom_point(alpha = 0.3) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  coord_equal() +
  facet_wrap(~type) +
  labs(
    title = "ÅDT 2023 vs 2024",
    x = "ÅDT 2024",
    y = "ÅDT 2023",
    subtitle = "Punkter på diagonal-linja indikerer samme verdi for de to årene."
  ) +
  theme_minimal()

scatter_data %>% filter(Vegkategori == "F") %>% 
  ggplot(aes(x = pred.2024 + 1, y = pred.2023 + 1, color = type)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkred") +
  geom_point(alpha = 0.3) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c("goldenrod", "grey10")) +
  coord_equal() +
  facet_wrap(~county_name) +
  labs(
    title = "ÅDT 2023 vs 2024",
    x = "ÅDT 2024",
    y = "ÅDT 2023",
    subtitle = "Punkter på diagonal-linja indikerer samme verdi for de to årene."
  ) +
  theme_minimal()



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Examine year-over-year changes in measured data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dplyr)
library(ggplot2)

# Calculate various change metrics for measured data
data_changes <- aadt2024 |>
  filter(!is.na(data.2023) & !is.na(data.2024)) |>
  mutate(
    abs_change = data.2024 - data.2023,
    pct_change = 100 * (data.2024 - data.2023) / data.2023,
    log_ratio = log(data.2024 / data.2023),
    avg_aadt = (data.2023 + data.2024) / 2
  )

# Summary statistics
change_summary <- data_changes |>
  summarise(
    n = n(),
    mean_pct_change = mean(pct_change),
    median_pct_change = median(pct_change),
    sd_pct_change = sd(pct_change),
    p10 = quantile(pct_change, 0.10),
    p25 = quantile(pct_change, 0.25),
    p75 = quantile(pct_change, 0.75),
    p90 = quantile(pct_change, 0.90),
    iqr = IQR(pct_change)
  )

print(change_summary)

# Distribution of percent changes
ggplot(data_changes, aes(x = pct_change)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Year-over-year change in measured AADT (2023→2024)",
    x = "Percent change (%)",
    y = "Count"
  ) +
  theme_minimal()

# Bland-Altman style: difference vs average
ggplot(data_changes, aes(x = avg_aadt, y = pct_change)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = quantile(data_changes$pct_change, c(0.05, 0.95)), 
             linetype = "dotted", color = "blue") +
  scale_x_log10(labels = scales::comma) +
  labs(
    title = "Year-over-year variation vs traffic volume",
    x = "Average AADT (log scale)",
    y = "Percent change (%)",
    subtitle = "Blue lines show 5th and 95th percentiles"
  ) +
  theme_minimal()

# Check if variation depends on traffic volume
ggplot(data_changes, aes(x = avg_aadt, y = abs(pct_change))) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Does variability depend on traffic volume?",
    x = "Average AADT (log scale)",
    y = "Absolute percent change (log scale)"
  ) +
  theme_minimal()




log_data <- data.frame(x = 1:100, y = log10(1:100))

ggplot(log_data, aes(x = x, y = y)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 100, by = 10))

