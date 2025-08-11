# Exploring bus data

library(sf)
library(dplyr)
library(INLA)

config <- yaml::read_yaml("config/data_config.yaml", readLines.warn = FALSE)

source("R/utilities.R")
source("R/model_fitting.R")
source("R/model_validation.R")


data <- readRDS("data/processed/preprocessed_data.rds")
aadt2024 <- load_data(config$data_paths$raw$aadt_results)


data$aadt_without_bus <- data$aadt
data$aadt_without_bus[!is.na(data$bus_aadt)] <- NA

# Create spatial index - this is simply the row number for each traffic link
data$spatial.idx <- 1:nrow(data)

adj_sparse <- readRDS("data/processed/adjacency_matrix_2024.rds")
constraint_matrix <- readRDS("data/processed/constraint_matrix_2024.rds")

# Model without bus data ----
formula_nobus <- aadt_without_bus ~ minLanes +
  f(spatial.idx, model = "besag", graph = adj_sparse, 
    adjust.for.con.comp = FALSE, scale.model = FALSE, constr = TRUE) +
  f(roadSystem, model="iid")

mod_nobus <- inla(formula_nobus, 
                  family = "poisson",
                  data = data,
                  control.predictor=list(link=1))

summary(mod_nobus)

# Model with bus data ----
formula_bus <- update(formula_nobus, aadt ~ . + hasOnlyPublicTransportLanes)
formula_bus

mod_bus <- inla(formula_bus, 
                family = "poisson",
                data = data,
                control.predictor=list(link=1))
summary(mod_bus)


approved_nobus <- calculate_approved(model = mod_nobus, data = data, 
                                     data_manual = aadt2024,
                                     truth_name = "??DT.offisiell",
                                     model_name = "no_bus_data")
approved_bus <- calculate_approved(model = mod_bus, data = data, 
                                   data_manual = aadt2024,
                                   truth_name = "??DT.offisiell",
                                   model_name = "bus_data")
rbind(approved_bus$approved, approved_nobus$approved)

bus_comparison <- full_join(approved_bus$uretta, approved_nobus$uretta,
                            suffix = c(".bus", ".nobus"),
                            by = join_by(ID, Trafikklenke??r, Vegsystemreferanse, Vegkategori, Vegnummer, Fylkesnr, Kommunenr,
                                         Strekningslengde..m., ??DT.fjor??rets, TBA.fjor??rets, Datagrunnlag, Datagrunnlag.??r, Datagrunnlag.TRP.ID,
                                         Datagrunnlag.flere.TRPer, ??DT.fra.datagrunnlag, TBA.fra.datagrunnlag, Korr.std.feil.fra.datagrunnlag, ??DT.Estimert,
                                         TBA.Estimert, Usikkerhet.Estimert, ??DT.Overstyrt, TBA.Overstyrt, ??DT.offisiell, TBA.offisiell,
                                         Trafikkarbeid.offisiell..i.kj??ret??y.km., Autogodkjent, Kontrollstatus))

# Uretta indikatorvariabel som sier om ei trafikklenke inneholder en retning som er bare kollektivtrafikk.

undirected_public_transport <- data %>%
  group_by(parentTrafficLinkId) %>%
  summarise(
    # Check if at least one direction has only public transport
    hasAnyDirectionWithOnlyPublicTransport = any(hasOnlyPublicTransportLanes == TRUE),
    # Additional useful information for analysis
    total_directions = n(),
    directions_with_only_public_transport = sum(hasOnlyPublicTransportLanes == TRUE, na.rm = TRUE),
    # Check if ALL directions have only public transport
    allDirectionsHaveOnlyPublicTransport = all(hasOnlyPublicTransportLanes == TRUE, na.rm = TRUE),
    # Check for mixed cases (some directions public only, others not)
    hasMixedDirections = any(hasOnlyPublicTransportLanes == TRUE, na.rm = TRUE) & 
      any(hasOnlyPublicTransportLanes == FALSE, na.rm = TRUE),
    .groups = 'drop'
  )

# Breakdown of the three options
undirected_public_transport %>%
  summarise(
    total_links = n(),
    links_with_public_only_directions = sum(hasAnyDirectionWithOnlyPublicTransport, na.rm = TRUE),
    links_all_directions_public_only = sum(allDirectionsHaveOnlyPublicTransport, na.rm = TRUE),
    links_with_mixed_directions = sum(hasMixedDirections, na.rm = TRUE)
  )

undirected_data <- bus_comparison %>% full_join(undirected_public_transport, by = join_by(ID == parentTrafficLinkId))

public_transport_locs <- filter(undirected_data, hasAnyDirectionWithOnlyPublicTransport) %>% 
  add_geometry_to_traffic_links(id_name = "ID", directed = FALSE) %>% 
  mutate(text = paste0("Prediction with bus data/model: ", bus_data_pred,
                       "<br>Prediction without bus data/model: ", no_bus_data_pred,
                       "<br>AADT 2024: ", ??DT.offisiell,
                       "<br>AADT 2023: ", ??DT.fjor??rets))

sum(public_transport_locs$approved.bus)/nrow(public_transport_locs)
sum(public_transport_locs$approved.nobus)/nrow(public_transport_locs)

library(ggplot2)
ggplot(public_transport_locs, aes(x = ??DT.offisiell)) +
  geom_point(aes(y = bus_data_pred), color = "darkcyan") +
  geom_point(aes(y = no_bus_data_pred), color = "tomato") +
  geom_abline(slope = 1, intercept = 1)+
  xlim(0, 25000) + ylim(0, 25000) + 
  theme_bw()

p_bus <- ggplot(public_transport_locs, aes(x = ??DT.offisiell)) +
  geom_point(aes(y = bus_data_pred), color = "darkcyan") +
  xlim(0, 5000) + ylim(0, 5000)

p_nobus <- ggplot(public_transport_locs, aes(x = ??DT.offisiell)) +
  geom_point(aes(y = no_bus_data_pred), color = "tomato")

library(patchwork)
p_bus + p_nobus

# Plotting
nvdb <- nvdb_objects()

pal_bin <- leaflet::colorFactor(
  palette = "viridis",
  domain = NULL, 
  na.color = "#88807b"
)

leaflet::leaflet(public_transport_locs, options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    color = ~ pal_bin(public_transport_locs[["approved.bus"]]),
    popup = ~ text,
    opacity = 1)