
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot directed traffic linksv ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_directed_links <- function(df, balanced = TRUE, county_to_plot = NULL){
  if(!is.null(county_to_plot)){
    df <- dplyr::filter(df, county == county_to_plot)
  }
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
                      "<br>Balanced: ", round(df$balanced_pred),
                      "<br>Measured: ", df$aadt,
                      "<br>Source: ", df$bestDataSourceAadt_registrationFrequency, ", ", df$bestDataSourceAadt_sourceType,
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot undirected traffic links ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Under construction
plot_undirected_links <- function(df, balanced = TRUE, column_to_plot = "pred"){
  # prepare geometry
  df <- df %>% add_geometry_to_traffic_links(id = "ID")
  
  # create concrete plotting column and popup text (avoid .data inside pal)
  df$plot_value <- df[[column_to_plot]]
  
  if (use_balanced) {
    df$text <- paste0("INLA: ", df$pred,
                      "<br>Balanced: ", df$balanced_pred,
                      "<br>ÅDT offisiell: ", df$ÅDT.offisiell,
                      "<br>ÅDT 2023: ", df$ÅDT.fjorårets,
                      "<br>Measured: ", df$ÅDT.fra.datagrunnlag,
                      "<br>ID: ", df$ID)
  } else {
    df$text <- paste0("Etimert ÅDT: ", df$pred,
                      "<br>ÅDT offisiell: ", df$ÅDT.offisiell,
                      "<br>ÅDT 2023: ", df$ÅDT.fjorårets,
                      "<br>Measured: ", df$ÅDT.fra.datagrunnlag,
                      "<br>ID: ", df$ID)
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
                       title = column_to_plot,
                       opacity = 1)
}

