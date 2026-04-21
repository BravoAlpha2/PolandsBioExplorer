
# Author: Nuno Garcia
# Date: 21/04/2026
# LinkedIn: https://www.linkedin.com/in/nuno-garcia-97b780158/
# ORCID: https://orcid.org/0000-0001-7917-3286
#
# Script purpose:
# Functions for using the user interface in the shiny app.

# Shiny app (main) purpose:
# Dashboard to explore biodiversity observations in Poland.
# Users can search species by vernacularName and scientificName,
# inspect observations on a map, and view a temporal timeline.

# ============================================================
# FUNCTIONS -> ui (value boxes)
# ============================================================
mod_value_boxes_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    valueBoxOutput(ns("vb_total_obs"), width = 4),
    valueBoxOutput(ns("vb_total_species"), width = 4),
    valueBoxOutput(ns("vb_current_filter"), width = 4)
  )
}

mod_value_boxes_server <- function(
    id,
    current_mode,
    selected_species,
    selected_species_data,
    default_obs_count,
    default_species_count
) {
  moduleServer(id, function(input, output, session) {
    output$vb_total_obs <- renderValueBox({
      n_obs <- if (current_mode() == "default") default_obs_count else nrow(selected_species_data())
      valueBox(
        value = format(n_obs, big.mark = ","),
        subtitle = if (current_mode() == "default") "Observations in Poland" else "Observations for selected species",
        icon = icon("binoculars"),
        color = "green"
      )
    })
    
    output$vb_total_species <- renderValueBox({
      n_species <- if (current_mode() == "default") default_species_count else 1
      valueBox(
        value = format(n_species, big.mark = ","),
        subtitle = if (current_mode() == "default") "Unique species in Poland" else "Unique species in current view",
        icon = icon("paw"),
        color = "blue"
      )
    })
    
    output$vb_current_filter <- renderValueBox({
      valueBox(
        value = if (current_mode() == "default") "None" else selected_species(),
        subtitle = "Current species filter",
        icon = icon("filter"),
        color = "yellow"
      )
    })
  })
}