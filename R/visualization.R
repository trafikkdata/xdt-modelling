
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Maps ----
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
                      "<br>Source: ", df$traffic_volume_source, ", ", df$traffic_volume_year,
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

# Under construction
plot_undirected_links <- function(df, balanced = TRUE, column_to_plot = "pred"){
  # prepare geometry
  df <- df %>% add_geometry_to_traffic_links(id = "ID")
  
  # create concrete plotting column and popup text (avoid .data inside pal)
  df$plot_value <- df[[column_to_plot]]
  
  df$text <- paste0("INLA: ", df$pred,
                      "<br>Balanced: ", df$balanced_pred,
                      "<br>ÅDT offisiell: ", df$ÅDT.offisiell,
                      "<br>ÅDT 2023: ", df$ÅDT.fjorårets,
                      "<br>Measured: ", df$ÅDT.fra.datagrunnlag,
                      "<br>ID: ", df$ID)

  
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

plot_decomp <- function(model, county_to_plot = NULL){
  decomp_df <- decompose_inla_predictions(model) %>% 
    add_decomposition_popup() %>% 
    add_geometry_to_traffic_links(id = "id")
  
  nvdb <- nvdb_objects()
  
  pal <- leaflet::colorNumeric(
    palette = "viridis",
    reverse = TRUE,
    na.color = "#88807b",
    domain = decomp_df$inla_pred
  )
  
  if(!is.null(county_to_plot)){
    decomp_df <- filter(decomp_df, county == county_to_plot)
  }
  
  leaflet::leaflet(decomp_df,
                   options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) %>%
    leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution) %>%
    leaflet::addPolylines(
      color = ~ pal(inla_pred),
      popup = ~ text,
      opacity = 1
    ) %>%
    leaflet::addLegend("bottomright",
                       pal = pal,
                       values = ~ inla_pred,
                       title = "ÅDT (INLA)",
                       opacity = 1)
}

plot_eale <- function(df, county_to_plot){
  #df <- undirected_results # REMOVE THIS ONCE DONE
  
  eale_80 <- quantile(df$eale, 0.8)
  plot_col <- "eale_capped"
  
  df <- add_geometry_to_traffic_links(df, id_name = "ID", directed = FALSE) %>% 
    mutate(
      eale = case_when(
        # +Inf becomes the finite max.
        eale ==  Inf ~ max(eale[is.finite(eale)]),
        # -Inf becomes the finite min.
        eale == -Inf ~ min(eale[is.finite(eale)]),
        # Other values stay the same.
        TRUE      ~  eale
      ),
      eale_capped = case_when(
        eale > eale_80 ~ eale_80,
        TRUE ~ eale
      ),
      text = paste0("Modell: ", pred,
                    "<br>ÅDT offisiell: ", ÅDT.offisiell,
                    "<br>ÅDT 2023: ", ÅDT.fjorårets,
                    "<br>Measured: ", ÅDT.fra.datagrunnlag,
                    "<br>ID: ", ID)
      ) %>% 
    filter(Fylkesnr == county_to_plot)
  
  df$plot_col <- df[[plot_col]]
  
  nvdb <- nvdb_objects()
  
  pal <- leaflet::colorNumeric(
    palette = "viridis",
    reverse = TRUE,
    na.color = "#88807b",
    domain = df$plot_col
  )
  
  leaflet::leaflet(df,
                   options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) %>%
    leaflet::addTiles(urlTemplate = nvdb$nvdb_url, attribution = nvdb$nvdb_attribution) %>%
    leaflet:: addPolylines(data = filter(df, ÅDT.fra.datagrunnlag != "null"),
      color = "black",
      weight = 9  
    ) %>% 
    leaflet::addPolylines(
      color = ~ pal(plot_col),
      popup = ~ text,
      weight = 4.5,
      opacity = 1
    ) %>%
    leaflet::addLegend("bottomright",
                       pal = pal,
                       values = ~ plot_col,
                       title = "eALE",
                       opacity = 1)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Non-map plots ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_last_year_scatter_plot <- function(){
  # NOT DONE. Add data transformation
  
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
}
