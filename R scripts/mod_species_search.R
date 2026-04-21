
# Author: Nuno Garcia
# Date: 21/04/2026
# LinkedIn: https://www.linkedin.com/in/nuno-garcia-97b780158/
# ORCID: https://orcid.org/0000-0001-7917-3286
#
# Script purpose:
# Functions for search species in the shiny app.

# Shiny app (main) purpose:
# Dashboard to explore biodiversity observations in Poland.
# Users can search species by vernacularName and scientificName,
# inspect observations on a map, and view a temporal timeline.

# ============================================================
# FUNCTIONS
# ============================================================
mod_species_search_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = "padding: 12px;",
      
      selectizeInput(
        inputId = ns("species_search"),
        label = "Search species:",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        options = list(
          placeholder = "Ex.: Grus grus (Common crane)"
        )
      ),
      
      fluidRow(
        column(
          width = 5,
          actionButton(
            ns("clear_species"),
            "Clear species",
            icon = icon("times"),
            width = "100%"
          )
        ),
        column(
          width = 5,
          actionButton(
            ns("reset_options"),
            "Reset options",
            icon = icon("rotate-left"),
            width = "100%"
          )
        )
      ),
      
      hr(),
      
      h5("Map options"),
      
      checkboxInput(
        ns("show_heatmap"),
        "Show heatmap in default view",
        value = FALSE
      ),
      
      checkboxInput(
        ns("show_clusters"),
        "Cluster points",
        value = TRUE
      ),
      
      sliderInput(
        ns("point_radius"),
        "Point radius",
        min = 3,
        max = 10,
        value = 5,
        step = 1
      ),
      
      hr(),
      
      h5("Optional filters"),
      
      checkboxInput(
        ns("only_with_media"),
        "Only show records with media",
        value = FALSE
      ),
      
      sliderInput(
        ns("year_range"),
        "Observation year range",
        min = 1900,
        max = as.integer(format(Sys.Date(), "%Y")),
        value = c(1950, as.integer(format(Sys.Date(), "%Y"))),
        step = 1,
        sep = ""
      ),
      
      br()
    )
  )
}

# ============================================================
# SERVER
# ============================================================
mod_species_search_server <- function(id, species_index) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      updateSelectizeInput(
        session = session,
        inputId = "species_search",
        choices = setNames(
          species_index$scientific_name_key,
          species_index$display_label
        ),
        selected = "",
        server = TRUE
      )
    })
    
    observeEvent(input$clear_species, {
      updateSelectizeInput(session, "species_search", selected = "")
    })
    
    list(
      selected_species = reactive({
        if (is.null(input$species_search) || identical(input$species_search, "")) {
          return(NULL)
        }
        input$species_search
      }),
      
      show_heatmap = reactive({
        isTRUE(input$show_heatmap)
      }),
      
      show_clusters = reactive({
        isTRUE(input$show_clusters)
      }),
      
      point_radius = reactive({
        input$point_radius
      }),
      
      only_with_media = reactive({
        isTRUE(input$only_with_media)
      }),
      
      year_range = reactive({
        input$year_range
      }),
      
      basis_filter = reactive({
        input$basis_filter
      })
    )
  })
}
