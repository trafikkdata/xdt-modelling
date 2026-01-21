# Balancing manual estimates

# Load functions
files.sources = list.files("R/", full.names = TRUE)
sapply(files.sources, source)

# Packages
library(dplyr)
library(sf)

# Data
data <- readRDS("data/processed/engineered_data.rds")
aadt2024 <- read.csv("data/raw/traffic-links-aadt-data-2024.csv")
nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Map undirected traffic volumes to directed links ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Count how many directed links per parent (1 = one-way, 2 = bidirectional)
direction_count <- data %>%
  group_by(parentTrafficLinkId) %>%
  summarise(n_directions = n(), .groups = 'drop')

# Join traffic volumes and direction counts, then calculate directed volume
data_with_volumes <- data %>%
  left_join(aadt2024, by = c("parentTrafficLinkId" = "ID")) %>%
  left_join(direction_count, by = "parentTrafficLinkId") %>%
  mutate(pred = as.numeric(ÅDT.offisiell) / n_directions,
         sd = 0.06*pred) %>% 
  tidyr::drop_na(pred)

# Before balancing 
manual_approved <- calculate_approved(
  pred = "pred",
  sd = "sd",
  data = data_with_volumes, 
  data_manual = aadt2024,
  model_name = "manual")


# Balancing manual estimates
clusters <- strategic_network_clustering(data_with_volumes)
balanced_manual <- balance_predictions(data_with_volumes, nodes, clusters, "complete_nodes")

data_balanced <- left_join(balanced_manual$balanced_res, data_with_volumes)

balanced_manual_approved <- calculate_approved(
  pred = "balanced_pred",
  sd = "balanced_sd",
  data = data_balanced, 
  data_manual = aadt2024,
  model_name = "balanced_manual")

rbind(manual_approved$approved, balanced_manual_approved$approved)

data_balanced <- data_balanced %>% 
  mutate(balanced_pred = round(balanced_pred),
         difference = abs(pred - balanced_pred)) %>% 
  select(id, parentTrafficLinkId, roadSystemReferences, aadt, 
         traffic_volume_source, balanced_pred, pred, difference)


options(scipen = 999)
View(data_balanced)
