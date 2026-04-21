
# Author: Nuno Garcia
# Date: 21/04/2026
# LinkedIn: https://www.linkedin.com/in/nuno-garcia-97b780158/
# ORCID: https://orcid.org/0000-0001-7917-3286
#
# Script purpose:
# Map´s fucntions for the shiny app.

# Shiny app (main) purpose:
# Dashboard to explore biodiversity observations in Poland.
# Users can search species by vernacularName and scientificName,
# inspect observations on a map, and view a temporal timeline.

# ============================================================
# FUNCTIONS -> ui (map w/ species ocurrences)
# ============================================================
mod_observation_map_ui <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("obs_map"), height = 620)
}

mod_observation_map_server <- function(
    id,
    poland_bbox,
    current_mode,
    current_map_data,
    selected_species,
    show_heatmap,
    show_clusters,
    point_radius
) {
  moduleServer(id, function(input, output, session) {
    output$obs_map <- renderLeaflet({
      leaflet() %>%
        addMeasure(
          position = "topright",
          primaryLengthUnit = "kilometers",
          primaryAreaUnit = "sqmeters",
          activeColor = "#3D535D",
          completedColor = "#7D4479"
        ) %>%
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
        addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeoWorldMap") %>%
        fitBounds(
          lng1 = poland_bbox$xmin,
          lat1 = poland_bbox$ymin,
          lng2 = poland_bbox$xmax,
          lat2 = poland_bbox$ymax
        ) %>%
        addMiniMap(position = "bottomleft", toggleDisplay = TRUE) %>%
        addSearchOSM() %>%
        addLayersControl(
          baseGroups = c("CartoDB Positron", "Esri World Imagery", "OpenStreetMap", "NatGeoWorldMap"),
          options = layersControlOptions(collapsed = TRUE)
        )
    })
    
    observe({
      dat <- current_map_data()
      proxy <- leafletProxy("obs_map", session = session)
      
      proxy %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
        clearControls() %>%
        clearHeatmap()
      
      if (is.null(dat) || nrow(dat) == 0) return()
      
      if (current_mode() == "default") {
        if (isTRUE(show_heatmap())) {
          proxy %>% addHeatmap(
            lng = dat$longitude,
            lat = dat$latitude,
            blur = 18,
            max = 0.05,
            radius = 12,
            group = "Heatmap"
          )
        }
        
        popup_text <- build_default_popup(dat)
        
        if (isTRUE(show_clusters())) {
          proxy %>% addCircleMarkers(
            lng = dat$longitude,
            lat = dat$latitude,
            radius = point_radius(),
            stroke = FALSE,
            fillOpacity = 0.40,
            popup = popup_text,
            clusterOptions = markerClusterOptions(),
            group = "Observations"
          )
        } else {
          proxy %>% addCircleMarkers(
            lng = dat$longitude,
            lat = dat$latitude,
            radius = point_radius(),
            stroke = FALSE,
            fillOpacity = 0.30,
            popup = popup_text,
            group = "Observations"
          )
        }
        
        proxy %>% addLegend(
          position = "bottomright",
          colors = c("#2b8cbe"),
          labels = c("Polish observations"),
          title = "Map content",
          opacity = 0.8
        )
      } else {
        popup_text <- build_selected_popup(dat)
        
        if (isTRUE(show_clusters())) {
          proxy %>% addCircleMarkers(
            lng = dat$longitude,
            lat = dat$latitude,
            radius = point_radius() + 1,
            stroke = TRUE,
            weight = 1,
            color = "#6a1b9a",
            fillOpacity = 0.75,
            popup = popup_text,
            clusterOptions = markerClusterOptions(),
            group = "Selected species"
          )
        } else {
          proxy %>% addCircleMarkers(
            lng = dat$longitude,
            lat = dat$latitude,
            radius = point_radius() + 1,
            stroke = TRUE,
            weight = 1,
            color = "#6a1b9a",
            fillOpacity = 0.75,
            popup = popup_text,
            group = "Selected species"
          )
        }
        
        proxy %>% addLegend(
          position = "bottomright",
          colors = c("#6a1b9a"),
          labels = c(selected_species()),
          title = "Selected species",
          opacity = 0.9
        )
      }
    })
  })
}
