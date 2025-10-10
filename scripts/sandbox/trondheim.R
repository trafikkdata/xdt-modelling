# Testing model in various sub-areas (Trondheim)

library(sf)
library(dplyr)
library(INLA)

config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/model_fitting.R")
source("R/model_validation.R")
source("R/visualization.R")

trondheim_data <- read_sf("data/processed/trondheim_data.geojson")
aadt2024 <- read.csv("data/raw/traffic-links-aadt-data-2024.csv")
nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")


# Create spatial index - this is simply the row number for each traffic link
trondheim_data$spatial.idx <- 1:nrow(trondheim_data)

adj_sparse <- build_adjacency_matrix(trondheim_data)
#constraint_matrix <- build_incidence_matrix(nodes = nodes, trondheim_data)

formula <- aadt ~ minLanes + functionalRoadClass:maxLanes +
  minLanes:roadCategory + functionalRoadClass + maxLanes + roadCategory +
  f(spatial.idx, model = "besag", graph = adj_sparse, 
    adjust.for.con.comp = FALSE, scale.model = FALSE, constr = TRUE) +
  f(roadSystem, model="iid")
  

mod_trd <- inla(formula, 
                family = "poisson",
                data = trondheim_data,
                control.predictor=list(link=1))
summary(mod_trd)


balanced_trondheim <- balance_group_predictions(data = trondheim_data, 
                                                nodes = nodes, model = mod_trd, 
                                                nodes_to_balance = "complete_nodes")

print(balanced_trondheim$diagnostics)

inla_res <- calculate_approved(
  model = mod_trd, 
  data = trondheim_data, 
  data_manual = aadt2024,
  model_name = "inla")

balanced_res <- calculate_approved(
  pred = balanced_trondheim$results$balanced_pred, 
  sd = balanced_trondheim$results$balanced_pred,
  data = trondheim_data, 
  data_manual = aadt2024, 
  model_name = "balansert")

rbind(inla_res$approved, balanced_res$approved)




# Plotting

plot_directed_links(balanced_trondheim$results)




# Undersøker A1 for node 3508236 ----
okstadbakken_problem <- constraint_matrix["3508236",]
okstadbakken_links <- okstadbakken_problem[okstadbakken_problem != 0]
okstadbakken_node <- nodes %>% filter(id == "3508236")

nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")
traffic_node_network <- nodes
link_ids <- trondheim_data$id %>% as.vector()

turning_movements <- nodes %>% filter(id == "3508236")


# Testing modeling pipeline
covariates <- c("functionalRoadClass:maxLanes",
                           "minLanes:roadCategory",
                           "functionalRoadClass",
                           "maxLanes",
                           "roadCategory")

trd_mod <- run_modeling_pipeline(data = trondheim_data %>% st_drop_geometry(), 
                                 covariates = covariates, 
                                 balancing_grouping_variable = "run_clustering")

trd_mod$diagnostics$approval$approved
plot_directed_links(trd_mod$data)



predictions_by_group <- trd_mod$diagnostics$balancing_diagnostics$predictions_by_group

pred_1 <- filter(predictions_by_group, grouping == 1)
plot_directed_links(pred_1)

library(htmlwidgets)
saveWidget(m, file="gruppe16.html")




#


