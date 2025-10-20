# Exploring bus data

# In this script, we compare a model with bus data to a model without bus data.

# Load functions
files.sources = list.files("R/", full.names = TRUE)
sapply(files.sources, source)

# Load packages
library(sf)
library(dplyr)
library(INLA)

# Data
data <- readRDS("data/processed/engineered_data.rds")
aadt2024 <- read.csv("data/raw/traffic-links-aadt-data-2024.csv")
bus_counts_on_traffic_links <- readRDS("data/processed/bus_counts_on_traffic_links.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Plot bus stuff ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

bus_counts_on_traffic_links <- add_geometry_to_traffic_links(bus_counts_on_traffic_links) %>% 
  mutate(text = paste0("Bus stop count: ", as.character(bus_aadt), "<br>", stopPointRef))

nvdb <- nvdb_objects()
pal <- leaflet::colorBin(
  palette = "viridis",
  domain = NULL, 
  reverse = TRUE,
  na.color = "#88807b"
)

leaflet::leaflet(bus_counts_on_traffic_links, options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
  leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution)  |>
  leaflet::addPolylines(
    color = ~ pal(bus_counts_on_traffic_links[["bus_aadt"]]), # Comment out to plot in blue
    popup = ~ paste0(as.character(bus_aadt), "<br>", stopPointRef),
    opacity = 1)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Modelling ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# Create spatial index - this is simply the row number for each traffic link
data$spatial.idx <- 1:nrow(data)

# Adjacency matrix
adj_sparse <- readRDS("data/processed/adjacency_matrix_2024_new.rds")


# Model without bus data ----
data$aadt_without_bus <- data$aadt
data$aadt_without_bus[!is.na(data$bus_aadt)] <- NA

formula_nobus <- aadt_without_bus ~ minLanes +
  f(spatial.idx, model = "besagproper", 
    graph = adj_sparse, 
    adjust.for.con.comp = FALSE, constr = TRUE) +
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
                                     model_name = "no_bus_data")
approved_bus <- calculate_approved(model = mod_bus, data = data, 
                                   data_manual = aadt2024,
                                   model_name = "bus_data")

# Autoapproved prediction rates
rbind(approved_bus$approved, approved_nobus$approved, approved_bus_int$approved)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Comparing to manual ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# Join predictions with and without bus data
bus_comparison <- full_join(
  approved_bus$uretta, approved_nobus$uretta,
  suffix = c(".bus", ".nobus"),
  by = join_by(ID, Trafikklenkeår, Vegsystemreferanse, Vegkategori, Vegnummer, 
               Fylkesnr, Kommunenr, Strekningslengde..m., ÅDT.fjorårets, 
               TBA.fjorårets, Datagrunnlag, Datagrunnlag.år, 
               Datagrunnlag.TRP.ID, Datagrunnlag.flere.TRPer, 
               ÅDT.fra.datagrunnlag, TBA.fra.datagrunnlag,
               Korr.std.feil.fra.datagrunnlag, ÅDT.Estimert, TBA.Estimert, 
               Usikkerhet.Estimert, ÅDT.Overstyrt, TBA.Overstyrt, ÅDT.offisiell, 
               TBA.offisiell, Trafikkarbeid.offisiell..i.kjøretøy.km., 
               Autogodkjent, Kontrollstatus))

# Create undirected indicator of whether a traffic link has at least one 
# direction that is only public transportation

undirected_public_transport <- data %>%
  group_by(parentTrafficLinkId) %>%
  summarise(
    containsBusData = any(!is.na(bus_aadt)), # Contains bus data in any direction?
    aadt = sum(aadt), # Undirected AADT is sum of directed AADT
    
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
    # Number of TL that have at least one direction with only public transportation
    links_with_public_only_directions = sum(hasAnyDirectionWithOnlyPublicTransport, na.rm = TRUE),
    # Number of TL with only public transport (both directions)
    links_all_directions_public_only = sum(allDirectionsHaveOnlyPublicTransport, na.rm = TRUE),
    # Number of links with public transport only in one direction, and all vehicles in other
    links_with_mixed_directions = sum(hasMixedDirections, na.rm = TRUE)
  )

undirected_data <- bus_comparison %>% 
  full_join(undirected_public_transport, 
            by = join_by(ID == parentTrafficLinkId))

public_transport_locs <- filter(undirected_data, hasAnyDirectionWithOnlyPublicTransport) %>% 
  add_geometry_to_traffic_links(id_name = "ID", directed = FALSE) %>% 
  mutate(text = paste0("Prediction with bus data/model: ", pred.bus,
                       "<br>Prediction without bus data/model: ", pred.nobus,
                       "<br>AADT 2024: ", ÅDT.offisiell,
                       "<br>AADT 2023: ", ÅDT.fjorårets,
                       "<br>Measured value: ", aadt))

# Autogodkjent på steder som bare har kollektivtrafikk
sum(public_transport_locs$approved.bus, na.rm = TRUE)/nrow(public_transport_locs)
sum(public_transport_locs$approved.nobus, na.rm = TRUE)/nrow(public_transport_locs)

library(ggplot2)
ggplot(public_transport_locs, aes(x = ÅDT.offisiell)) +
  geom_point(aes(y = pred.bus), color = "#6c99bb") +
  geom_point(aes(y = pred.nobus), color = "tomato") +
  geom_abline(slope = 1, intercept = 1)+
  ylab("Estimert ÅDT") +
  coord_equal() +
  xlim(0, 25000) + ylim(0, 25000) + 
  theme_bw()


undirected_bus_long <- tidyr::pivot_longer(public_transport_locs, 
                                           cols = c("pred.bus", "pred.nobus"),
             names_to = "model", values_to = "pred") 

extreme <- undirected_bus_long %>% filter(pred > 5000 & model == "pred.bus")

ggplot(undirected_bus_long, aes(x = ÅDT.offisiell, color = containsBusData)) +
  geom_point(aes(y = pred)) +
  coord_equal() +
  facet_wrap(~model) + theme_bw()

ggplot(undirected_bus_long, aes(x = as.numeric(ÅDT.fjorårets), color = containsBusData)) +
  geom_point(aes(y = pred)) +
  coord_equal() +
  facet_wrap(~model) + theme_bw()

p_bus <- ggplot(public_transport_locs, aes(x = ÅDT.offisiell)) +
  geom_point(aes(y = pred.bus), color = "#6c99bb") +
  xlim(0, 5000) + ylim(0, 5000)

p_nobus <- ggplot(public_transport_locs, aes(x = ÅDT.offisiell)) +
  geom_point(aes(y = pred.nobus), color = "tomato")

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

pt_data_undirected <- public_transport_locs %>% arrange(pred.bus)
pt_data <- filter(data, hasOnlyPublicTransportLanes) %>% arrange(aadt) 




