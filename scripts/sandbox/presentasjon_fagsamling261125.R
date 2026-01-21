# Plott til fagsamling 26.-27. november 2025

# Load functions
files.sources = list.files("R/", full.names = TRUE)
sapply(files.sources, source)

# Load packages
library(sf)
library(tidyverse)


data <- readRDS("data/processed/engineered_data.rds")
aadt2024 <- read.csv("data/raw/traffic-links-aadt-data-2024.csv")
nodes <- readRDS("data/processed/nodes.RDS")




covariates <- c("functionalRoadClass:maxLanes",
                "minLanes:roadCategory",
                "functionalRoadClass",
                "functionalRoadClass:isRamp",
                "maxLanes",
                "roadCategory")#,
                #"hasOnlyPublicTransportLanes")

clusters <- strategic_network_clustering(data)


balancing_complete_nodes <- run_modeling_pipeline(
  data = data,
  balancing_grouping_variable = clusters,
  covariates = covariates,
  nodes_to_balance = "complete_nodes",
  model_name = "complete_nodes"
)


saveRDS(balancing_complete_nodes, "results/latest_model.rds")

plot_directed_links(df = balancing_complete_nodes$data, county = "Trøndelag")
plot_directed_links(df = balancing_complete_nodes$data, county = "Bergen")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Gjør originale estimater retta ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Count how many directed links each undirected link has
directed_counts <- data |>
  group_by(parentTrafficLinkId) |>
  summarise(n_directed = n(), .groups = "drop")

# Join the counts to the 2024 predictions
aadt2024_with_counts <- aadt2024 |>
  left_join(directed_counts, by = c("ID" = "parentTrafficLinkId"))

# Split the AADT values based on number of directed links
aadt2024_directed <- data |>
  select(id, parentTrafficLinkId, county, isRamp) |>
  left_join(aadt2024_with_counts, by = c("parentTrafficLinkId" = "ID")) |>
  mutate(
    ÅDT.Estimert = as.numeric(ÅDT.Estimert),
    ÅDT.Estimert.Directed = case_when(
      n_directed == 1 ~ ÅDT.Estimert,        # One-way: full volume
      n_directed == 2 ~ ÅDT.Estimert / 2,    # Bidirectional: split in half
      TRUE ~ NA_real_                         # Handle unexpected cases
    )
  ) |>
  select(id, parentTrafficLinkId, ÅDT.Estimert.Directed, county, isRamp) %>% 
  add_geometry_to_traffic_links()

aadt2024_directed$balanced_pred <- aadt2024_directed[["ÅDT.Estimert.Directed"]]

plot_directed_links(aadt2024_directed, county = "Møre og Romsdal")
plot_directed_links(aadt2024_directed, county = "Møre og Romsdal")




# Plot av siste prediksjoner mot fjorårets
undirected_latest <- balancing_complete_nodes$diagnostics$approval$undirected_data %>% 
  mutate(ÅDT.Estimert = as.numeric(ÅDT.Estimert))
  
pivot_longer(undirected_latest, cols = c("ÅDT.Estimert", "pred")) %>% 
  ggplot(aes(x = as.numeric(ÅDT.fjorårets), y = value, fill = Datagrunnlag == "null")) +
  geom_point(alpha = 0.4, shape = 21, stroke = NA, size = 2) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("darkgreen", "darkgoldenrod"))+
  facet_wrap(~ name) +
  #xlim(0, 50000) +
  #ylim(0, 50000) +
  coord_equal() +
  theme_minimal()


balancing_complete_nodes$diagnostics$approval$undirected_data





