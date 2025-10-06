
library(sf)
library(dplyr)
library(INLA)

config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/model_fitting.R")
source("R/build_matrices.R")
source("R/balancing_clusters.R")
source("R/model_validation.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Load data and matrices ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

data <- readRDS("data/processed/engineered_data.rds")
aadt2024 <- load_data(config$data_paths$raw$aadt_results)
nodes <- read_sf("data/raw/traffic-nodes-2024.geojson")


# Misc. tests ------------------------------------------------------------------

print_turning_movements_for_link_at_node(node_id = "347086", 
                                         link_id = "0.0-1.0@319629-WITH", 
                                         nodes)

turns <- get_turning_movements(nodes, node_id = "271925")

print_turning_movements_for_link("0.0@3112222-1.0@3112230-WITH", data, nodes)

# Find number of traffic links per county
data %>% group_by(county) %>% count() %>% arrange(desc(n))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Test "fit_group_model" function and "fit_national_model" ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# These are internal functions called by "run_modeling_pipeline".


# model <- fit_model(data, c("minLanes", "functionalRoadClass")) # This takes a long time to run
model_county <- fit_group_model(county_data, c("minLanes", "functionalRoadClass"))

national_res <- fit_national_model(
  data, 
  formula_covariates = c("minLanes", "functionalRoadClass"),
  grouping_variable = "county", 
  groups = unique(data$county),
  inla_scope = "local")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Test "run_modeling_pipeline" function for one county ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

res <- run_modeling_pipeline(inla_groups_to_process = c("Telemark"), 
                             inla_grouping_variable = "county")


retta <- res$data %>% 
  add_geometry_to_traffic_links() %>% 
  #st_as_sf() %>% 
  mutate(text = paste0("INLA: ", pred, 
                       "<br>Balanced: ", balanced_pred,
                       #"<br>ÅDT 2023: ", ÅDT.fjorårets,
                       "<br>Measured: ", aadt,
                       "<br>ID: ", id))
nvdb <- nvdb_objects()

pal <- leaflet::colorNumeric(
  palette = "viridis",
  reverse = TRUE,
  na.color = "#88807b",
  domain = retta$balanced_pred
)

leaflet::leaflet(retta, 
                 options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    color = ~ pal(balanced_pred),
    popup = ~ text,
    opacity = 1) |>
  leaflet::addLegend("bottomright", pal = pal, values = ~ balanced_pred, title = "ÅDT", opacity = 1)


pal <- leaflet::colorNumeric(
  palette = "viridis",
  reverse = TRUE,
  na.color = "#88807b",
  domain = retta$aadt
)
leaflet::leaflet(retta, 
                 options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    color = ~ pal(aadt),
    popup = ~ text,
    opacity = 1) |>
  leaflet::addLegend("bottomright", pal = pal, values = ~ aadt, title = "ÅDT", opacity = 1)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Merged vs not merged sparse covariate categories ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

good_formula <- c("functionalRoadClass:maxLanes", 
                  "minLanes:roadCategory", 
                  #"functionalRoadClass:roadCategory", 
                  "functionalRoadClass", 
                  "maxLanes", 
                  "minLanes", 
                  "roadCategory", 
                  "hasOnlyPublicTransportLanes", 
                  "isFerryRoute", 
                  "isNorwegianScenicRoute")

good_formula_merged <- c("functionalRoadClass_merged:maxLanes_merged", 
                         "minLanes_merged:roadCategory", 
                         #"functionalRoadClass_merged:roadCategory", 
                         "functionalRoadClass_merged", 
                         "maxLanes_merged", 
                         "minLanes_merged", 
                         "roadCategory", 
                         "hasOnlyPublicTransportLanes", 
                         "isFerryRoute", 
                         "isNorwegianScenicRoute")

merged_covariates <- run_modeling_pipeline(groups_to_process = "Trøndelag", 
                                           covariates = good_formula_merged)
original_covariates <- run_modeling_pipeline(groups_to_process = "Trøndelag",
                                             covariates = good_formula)

merged_approved <- calculate_approved(data = merged_covariates$data,
                                      data_manual = aadt2024,
                                      model_name = "merged_covariates")
original_approved <- calculate_approved(data = original_covariates$data,
                                      data_manual = aadt2024,
                                      model_name = "original_covariates")
rbind(merged_approved$approved, original_approved$approved)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Balancing I-intersections or not ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

balanced_i_intersections <- run_modeling_pipeline(inla_groups_to_process = "all", 
                                                  balancing_grouping_variable = "county",
                                                  covariates = good_formula_merged,
                                                  balance_i_intersections = TRUE)
saveRDS(balanced_i_intersections, "results/balanced_i_intersections.rds")
balanced_i_intersections <- readRDS("results/balanced_i_intersections.rds")

unbalanced_i_intersections <- run_modeling_pipeline(inla_groups_to_process = "all",
                                                    balancing_grouping_variable = "county",
                                                    covariates = good_formula_merged,
                                                    balance_i_intersections = FALSE)
saveRDS(unbalanced_i_intersections, "results/unbalanced_i_intersections.rds")


approved_bali <- calculate_approved(data = balanced_i_intersections$data, 
                                    data_manual = aadt2024,
                                    model_name = "balanced_i_intersections")
approved_unbali <- calculate_approved(data = unbalanced_i_intersections$data, 
                                      data_manual = aadt2024,
                                      model_name = "unbalanced_i_intersections")
rbind(approved_bali$approved, approved_unbali$approved)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Running the model separately vs jointly for all of Norway ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

separate_model <- run_modeling_pipeline(groups_to_process = "all", 
                                        inla_scope = "local",
                                        covariates = good_formula_merged,
                                        balance_predictions = FALSE)
joint_model <- run_modeling_pipeline(groups_to_process = "all", 
                                     inla_scope = "national",
                                     covariates = good_formula_merged,
                                     balance_predictions = FALSE)

separate <- calculate_approved(data = separate_model$data,
                                      pred = separate_model$data$pred,
                                      sd = separate_model$data$sd,
                                      data_manual = aadt2024,
                                      model_name = "separate")
joint <- calculate_approved(data = joint_model$data,
                                        pred = joint_model$data$pred,
                                        sd = joint_model$data$sd,
                                        data_manual = aadt2024,
                                        model_name = "joint")
rbind(separate$approved, joint$approved)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Trying to recreate the best results so far ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

covariates <- c("functionalRoadClass:maxLanes",
                "minLanes:roadCategory",
                "functionalRoadClass",
                "maxLanes",
                "roadCategory")

joint_model <- run_modeling_pipeline(inla_groups_to_process = "all", 
                                     covariates = covariates,
                                     balance_predictions = FALSE)

joint <- calculate_approved(data = joint_model$data,
                            pred = joint_model$data$pred,
                            sd = joint_model$data$sd,
                            data_manual = aadt2024,
                            model_name = "joint")

joint$approved


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Testing new grouping from Johannes with the pipeline ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

data <- readRDS("data/processed/engineered_data.rds")
# clustered <- read.csv("../../directed-traffic-links-2024-clustered.csv")
# Or:
clustered <- readRDS("results/cluster_assignments.rds") 

balanced_in_clusters <- run_modeling_pipeline(
  data = data,
  balancing_grouping_variable = clustered,
  covariates = good_formula_merged
  )

balanced_in_clusters$diagnostics$approval$approved


data_with_clusters <- readRDS("results/data_with_clusters.rds")

# Look at number of nodes per group
node_data <- data_with_clusters %>% 
  select(id, cluster_id, startTrafficNodeId, endTrafficNodeId) %>% 
  pivot_longer(c(3:4)) %>% group_by(cluster_id) %>% summarise(num_nodes = length(unique(value)))


library(tictoc)


# Testing I intersection options

tic()
balancing_all <- run_modeling_pipeline(
  data = data,
  balancing_grouping_variable = clustered,
  covariates = good_formula_merged,
  nodes_to_balance = "all",
  model_name = "all"
)
toc()

tic()
balancing_all_except_i_intersections <- run_modeling_pipeline(
  data = data,
  balancing_grouping_variable = clustered,
  covariates = good_formula_merged,
  nodes_to_balance = "all_except_i_intersections",
  model_name = "all_except_i_intersections"
)
toc()

tic()
balancing_complete_nodes <- run_modeling_pipeline(
  data = data,
  balancing_grouping_variable = clustered,
  covariates = good_formula_merged,
  nodes_to_balance = "complete_nodes",
  model_name = "complete_nodes"
)
toc()

tic()
no_balancing <- run_modeling_pipeline(
  data = data,
  covariates = good_formula_merged,
  balance_predictions = FALSE,
  model_name = "no_balancing"
)
toc()

rbind(balancing_all$diagnostics$approval$approved,
      balancing_all_except_i_intersections$diagnostics$approval$approved,
      balancing_complete_nodes$diagnostics$approval$approved,
      no_balancing$diagnostics$approval$approved)



# Data til Merijn -----

df <- no_balancing$data %>% 
  split_traffic_link_id() %>% 
  dplyr::select(id, parentTrafficLinkId, 
                startPosition, endPosition, 
                isTrafficWithMetering, 
                registeredAadt = aadt, registeredSd = aadt_sd, 
                pred, sd)

write.csv(df, "data/processed/predictions_for_merijn.csv", row.names = FALSE)



# Kommune-vis autogodkjenning ------

df <- no_balancing$diagnostics$approval$uretta

get_approval_per_group <- function(df, group_name){
  kommunenavn <- read.csv("data/raw/kommunenummer.csv", sep = ";") %>% select(kommunenummer, kommunenavn)
  
  approval_per_group <- df |>
    group_by(.data[[group_name]]) |>
    summarise(fraction_approved = sum(approved)/n(),
              links_in_group = n()) %>% 
    arrange(desc(fraction_approved)) %>% 
    left_join(kommunenavn, by = join_by(Kommunenr == kommunenummer)) 
  return(approval_per_group)
}

kommunenavn <- read.csv("data/raw/kommunenummer.csv", sep = ";") %>% select(kommunenummer, kommunenavn)

godkjent_per_kommune <- df %>% get_approval_per_group("Kommunenr") 




library(ggplot2)
ggplot(godkjent_per_kommune, aes(x = links_in_group, y = fraction_approved)) +
    geom_point()
  
df <- df %>% 
  left_join(kommunenavn, by = join_by(Kommunenr == kommunenummer))


plot_traffic_links <- function(df, group_name = NULL, group_entry = NULL){
  if(!is.null(group_name)){
    df <- df |> dplyr::filter(.data[[group_name]] == group_entry)
  }
  
  #df$pred <- df$pred
  
  df <- df %>% add_geometry_to_traffic_links(id = "ID") %>% 
    #st_as_sf() %>% 
    mutate(text = paste0("INLA: ", pred, 
                         #"<br>Balanced: ", balanced_pred,
                         #"<br>ÅDT 2023: ", ÅDT.fjorårets,
                         "<br>ÅDT 2024: ", ÅDT.offisiell,
                         "<br>Measured: ", ÅDT.fra.datagrunnlag,
                         "<br>Autoapproved: ", approved,
                         "<br>ID: ", ID))
  
  
  nvdb <- nvdb_objects()
  
  
  pal <- leaflet::colorNumeric(
    palette = "viridis",
    reverse = TRUE,
    na.color = "#88807b",
    domain = df$pred
  )
  leaflet::leaflet(df, 
                   options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
    leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
    leaflet::addPolylines(
      color = ~ pal(pred),
      popup = ~ text,
      opacity = 1) |>
    leaflet::addLegend("bottomright", pal = pal, values = ~ pred, title = "ÅDT", opacity = 1)
  
  
  
}  

plot_traffic_links(df, "kommunenavn", "Fedje")

plot_traffic_links(df, "kommunenavn", "Ås")

plot_traffic_links(df, "Fylkesnr", "50")


# Sammenligning a cluster-balansert Trøndelag og enhetlig balansert Trøndelag

trondelag_cluster <- run_modeling_pipeline(
  data = data,
  inla_grouping_variable = "county",
  inla_groups_to_process = "Trøndelag",
  balancing_grouping_variable = "run_clustering",
  covariates = good_formula_merged,
  nodes_to_balance = "complete_nodes",
  model_name = "balancing_in_clusters"
)

trondelag_in_one <- run_modeling_pipeline(
  data = data,
  inla_grouping_variable = "county",
  inla_groups_to_process = "Trøndelag",
  covariates = good_formula_merged,
  nodes_to_balance = "complete_nodes",
  model_name = "balancing_in_one"
)

approved_trondelag <- get_approval_per_group(trondelag_in_one$diagnostics$approval$uretta, "Kommunenr")

rbind(trondelag_cluster$diagnostics$approval$approved,
      trondelag_in_one$diagnostics$approval$approved)

uretta <- trondelag_in_one$diagnostics$approval$uretta
plot_traffic_links(trondelag_cluster$diagnostics$approval$uretta)



plot_directed_links <- function(df = trondelag_in_one$data, balanced = TRUE){
  # prepare geometry
  df <- df %>% add_geometry_to_traffic_links(id = "id")
  
  # decide which column to use
  use_balanced <- balanced && ("balanced_pred" %in% names(df))
  if (balanced && !("balanced_pred" %in% names(df))) {
    message("balanced = TRUE but 'balanced_pred' not found — falling back to 'pred'")
  }
  value_col <- if (use_balanced) "balanced_pred" else "pred"
  
  # create concrete plotting column and popup text (avoid .data inside pal)
  df$plot_value <- df[[value_col]]
  if (use_balanced) {
    df$text <- paste0("INLA: ", df$pred,
                      "<br>Balanced: ", df$balanced_pred,
                      "<br>Measured: ", df$aadt,
                      "<br>ID: ", df$id)
  } else {
    df$text <- paste0("INLA: ", df$pred,
                      "<br>Measured: ", df$aadt,
                      "<br>ID: ", df$id)
  }
  
  nvdb <- nvdb_objects()
  
  pal <- leaflet::colorNumeric(
    palette = "viridis",
    reverse = TRUE,
    na.color = "#88807b",
    domain = df$plot_value
  )
  
  leaflet::leaflet(df,
                   options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) %>%
    leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution) %>%
    leaflet::addPolylines(
      color = ~ pal(plot_value),
      popup = ~ text,
      opacity = 1
    ) %>%
    leaflet::addLegend("bottomright",
                       pal = pal,
                       values = ~ plot_value,
                       title = if (use_balanced) "ÅDT (balanced)" else "ÅDT (INLA)",
                       opacity = 1)
}

plot_directed_links(trondelag_in_one$data)


plot_undirected_links <- function(df = trondelag_in_one$data, balanced = TRUE){
  # prepare geometry
  df <- df %>% add_geometry_to_traffic_links(id = "id")
  
  # decide which column to use
  use_balanced <- balanced && ("balanced_pred" %in% names(df))
  if (balanced && !("balanced_pred" %in% names(df))) {
    message("balanced = TRUE but 'balanced_pred' not found — falling back to 'pred'")
  }
  value_col <- if (use_balanced) "balanced_pred" else "pred"
  
  # create concrete plotting column and popup text (avoid .data inside pal)
  df$plot_value <- df[[value_col]]
  if (use_balanced) {
    df$text <- paste0("INLA: ", df$pred,
                      "<br>Balanced: ", df$balanced_pred,
                      "<br>Measured: ", df$aadt,
                      "<br>ID: ", df$id)
  } else {
    df$text <- paste0("INLA: ", df$pred,
                      "<br>Measured: ", df$aadt,
                      "<br>ID: ", df$id)
  }
  
  nvdb <- nvdb_objects()
  
  pal <- leaflet::colorNumeric(
    palette = "viridis",
    reverse = TRUE,
    na.color = "#88807b",
    domain = df$plot_value
  )
  
  leaflet::leaflet(df,
                   options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) %>%
    leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution) %>%
    leaflet::addPolylines(
      color = ~ pal(plot_value),
      popup = ~ text,
      opacity = 1
    ) %>%
    leaflet::addLegend("bottomright",
                       pal = pal,
                       values = ~ plot_value,
                       title = if (use_balanced) "ÅDT (balanced)" else "ÅDT (INLA)",
                       opacity = 1)
}
