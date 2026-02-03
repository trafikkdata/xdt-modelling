# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Preprocess data and create everything that takes time.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(xdtkit)

year <- 2025
path <- paste0(year, "/")


# Traffic links: Load and preprocess
directed_traffic_links <- jsonlite::fromJSON(paste0(path, "data-raw/directed-traffic-links-", year, ".json"))
preprocessed_traffic_links <- preprocess_traffic_links(directed_traffic_links, year = year)

missing_counts <- colSums(is.na(preprocessed_traffic_links))
missing_counts[missing_counts > 0]


# Bus data: Load and preprocess
stops_on_traffic_links <- read.csv(paste0(path, "data-raw/Trafikklenker med holdeplasser ", year, ".csv"))
bus_counts <- read.csv(paste0(path, "data-raw/holdeplasspasseringer_entur_", year, ".csv"))

bus_aadt <- calculate_bus_aadt(stops_on_traffic_links, bus_counts, year = year)


# Fill missing values and add bus data
prepared_traffic_links <- fill_missing_values(
  df = preprocessed_traffic_links,
  unknown_impute_columns = c("functionClass", "highestSpeedLimit", "lowestSpeedLimit","maxLanes", "minLanes"),
  mode_impute_columns = c("hasOnlyPublicTransportLanes"),
  median_impute_columns = c("lastYearAadt_aadt", "lastYearAadt_heavyRatio", 
                            "lastYearAadt_heavyAadt")) |>
  remove_negative_aadt() |> 
  add_logLastYear() |>
  join_bus_to_traffic(bus_aadt)

missing_counts <- colSums(is.na(prepared_traffic_links))
missing_counts[missing_counts > 0]


# Nodes: Load and preprocess (may take a minute to run)
raw_nodes_geo <- sf::st_read(paste0(path, "data-raw/traffic-nodes-", year, ".geojson"))
nodes <- identify_unbalanceable_nodes(raw_nodes_geo, prepared_traffic_links) 


# Adjacency matrix (may take several minutes to run)
adjacency_matrix <- build_adjacency_matrix(
  prepared_traffic_links,
  exclude_public_transport = TRUE) 


# Balancing clusters
clusters <- strategic_network_clustering(
  data = prepared_traffic_links,
  year = year, 
  boundary_links = c("Trafikkdata_continuous", "AutoPASS", "Trafikkdata_periodic"),
  extra_boundary_links = c("0.47813092@181362-0.69434556@181186", # TL vest for Oslo
                           "0.64310018@971787-0.44481682@971788",# TL'er øst for Oslo
                           "0.59497974@971566-0.82908906@444258",
                           "0.31944922@443497-0.40997724@971559",
                           "0.51111532@971558-0.31944922@443497",
                           "0.78140309@971504-0.63295502@443465",
                           "0.65120232-0.91722091@705187", # TL'er nord-øst for Oslo
                           "0.69089123@704623-0.49119695@705214",
                           "0.0@2472765-0.55746674@2472766", # Tl'er nord for Oslo
                           "0.9628257@1060294-0.68755836@1060295"
  ))


clusters_heavy <- strategic_network_clustering(
  data = prepared_traffic_links,
  year = year, 
  boundary_links = c("Trafikkdata_continuous", "AutoPASS"),
  heavy_vehicle = TRUE)


data_with_clusters <- dplyr::left_join(prepared_traffic_links, clusters,
                                       by = dplyr::join_by(parentTrafficLinkId == id))
table(data_with_clusters$cluster_id)
plot_traffic_links_simple_map(dplyr::filter(data_with_clusters, cluster_id == 18),
                              color_by = "traffic_volume_source")


# Save everything (prepared_traffic_links, nodes, adjacency matrix, clusters)
saveRDS(prepared_traffic_links, paste0(path, "data-prepared/prepared_traffic_links", year, ".rds"))
saveRDS(nodes, paste0(path, "data-prepared/prepared_nodes", year, ".rds"))
saveRDS(adjacency_matrix, paste0(path, "data-prepared/adjacency_matrix", year, ".rds"))
saveRDS(clusters, paste0(path, "data-prepared/clusters", year, ".rds"))
saveRDS(clusters_heavy, paste0(path, "data-prepared/clusters_heavy", year, ".rds"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Model setup
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load data
prepared_traffic_links <- readRDS(paste0(path, "data-prepared/prepared_traffic_links", year, ".rds"))
nodes <- readRDS(paste0(path, "data-prepared/prepared_nodes", year, ".rds"))
adjacency_matrix <- readRDS(paste0(path, "data-prepared/adjacency_matrix", year, ".rds"))
clusters <- readRDS(paste0(path, "data-prepared/clusters", year, ".rds"))

covariates <- ~ functionalRoadClass:maxLanes +
  functionalRoadClass:roadCategory +
  minLanes:roadCategory + functionalRoadClass +
  maxLanes + roadCategory +
  hasOnlyPublicTransportLanes + #isFerryRoute + isNorwegianScenicRoute +
  functionalRoadClass*isRamp

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.a Run INLA model for total AADT.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

covariates_total <- update(covariates, ~ . + lastYearAadt_logAadt)

inla_model_total <- fit_inla_model(
  data = prepared_traffic_links,
  adjacency_matrix,
  fixed_effects = covariates_total,
  iid_effects = "roadSystem",
  family = "poisson")

inla_model_total

predictions_total <- dplyr::full_join(prepared_traffic_links, inla_model_total$predictions)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.b Run balancing for total AADT.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tictoc::tic()
balanced_model_total <- balance_predictions(data = predictions_total,
                                            nodes = nodes,
                                            balancing_grouping_variable = clusters,
                                            nodes_to_balance = "complete nodes",
                                            year = year)

predictions_total <- dplyr::full_join(predictions_total, balanced_model_total$balanced_res)

saveRDS(predictions_total, paste0(path, "results/predictions_total.rds"))
tictoc::toc()
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.a Run INLA model for heavy AADT.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

covariates_heavy <- update(covariates, ~ . + lastYearAadt_logHeavyAadt)

inla_model_heavy <- fit_inla_model(
  data = prepared_traffic_links,
  adjacency_matrix,
  fixed_effects = covariates_heavy,
  iid_effects = "roadSystem",
  family = "poisson",
  heavy_vehicle = TRUE)

inla_model_heavy

predictions_heavy <- dplyr::full_join(prepared_traffic_links, inla_model_heavy$predictions)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.b Run balancing for heavy AADT.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tictoc::tic()
balanced_model_heavy <- balance_predictions(data = predictions_heavy,
                                            nodes = nodes,
                                            balancing_grouping_variable = clusters_heavy,
                                            nodes_to_balance = "complete nodes",
                                            heavy_vehicle = TRUE,
                                            year = year)


predictions_heavy <- dplyr::full_join(predictions_heavy, balanced_model_heavy$balanced_res)

saveRDS(predictions_heavy, paste0(path, "results/predictions_heavy.rds"))
tictoc::toc()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Publish to GitHub.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
predictions_total <- readRDS(paste0(path, "results/predictions_total.rds"))
predictions_heavy <- readRDS(paste0(path, "results/predictions_heavy.rds"))

predictions <- dplyr::full_join(predictions_total, predictions_heavy, 
                                by = c("id", "parentTrafficLinkId", "traffic_volume_source", "aadt", "heavyAadt"))


# 1. Open your .Renviron file and add token there as
# TRAFIKKDATA_GH_PKG_TOKEN=ghp_YourActualTokenHere1234567890abcdef
# (no spaces around "=", no quotes, each environment variable on its own line.)
# usethis::edit_r_environ()

results2025 <- predictions |>
  dplyr::mutate(
    estimatedAadt = dplyr::case_when(
      traffic_volume_source == "Bus" | is.na(aadt) ~ balanced_pred,
      .default = aadt),
    estimatedAadtHeavy = dplyr::case_when(
      traffic_volume_source == "Bus" | is.na(heavyAadt) ~ balanced_pred_heavy,
      .default = heavyAadt)) |>
  dplyr::select(
    id, parentTrafficLinkId, estimatedAadt,
    estimatedAadtStandardDeviation = balanced_sd,
    estimatedAadtHeavy,
    estimatedAadtHeavyStandardDeviation = balanced_sd_heavy)

colSums(is.na(results2025))

saveRDS(results2025, paste0(path, "results/results2025.rds"))

# Try uploading (as test release)
#upload_df_to_github_release(data_2025, year = 2025, prerelease = TRUE, overwrite = TRUE)


# An actual release
upload_df_to_github_release(results2025, year = 2025, prerelease = FALSE, overwrite = TRUE)

