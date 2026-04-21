
# Author: Nuno Garcia
# Date: 21/04/2026
# LinkedIn: https://www.linkedin.com/in/nuno-garcia-97b780158/
# ORCID: https://orcid.org/0000-0001-7917-3286
#
# Script purpose:
# Species traits / profile module for the Shiny app.

# Shiny app (main) purpose:
# Dashboard to explore biodiversity observations in Poland.
# Users can search species by vernacularName and scientificName,
# inspect observations on a map, and view a temporal timeline.

# ============================================================
# Sp. TRAITS
# ============================================================
mod_traits_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        uiOutput(ns("traits_intro"))
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        box(
          width = 12,
          class = "app-card",
          title = tagList(tags$span(class = "card-icon", icon("paw")), "Species card"),
          status = NULL,
          solidHeader = FALSE,
          uiOutput(ns("species_card"))
        )
      ),
      
      column(
        width = 6,
        fluidRow(
          column(
            width = 6,
            valueBoxOutput(ns("obs_box"), width = 12)
          ),
          column(
            width = 6,
            valueBoxOutput(ns("year_box"), width = 12)
          )
        ),
        fluidRow(
          column(
            width = 6,
            valueBoxOutput(ns("datasets_box"), width = 12)
          ),
          column(
            width = 6,
            valueBoxOutput(ns("basis_box"), width = 12)
          )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              class = "app-card",
              title = tagList(tags$span(class = "card-icon", icon("table")), "Trait table"),
              status = NULL,
              solidHeader = FALSE,
              tableOutput(ns("traits_table"))
            )
          )
        )
      )
    )
  )
}

mod_traits_server <- function(id, current_mode, selected_species_data, selected_species_traits) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    fallback_value <- function(x, fallback = "N/A") {
      if (is.null(x) || length(x) == 0) return(fallback)
      x <- as.character(x)
      x <- x[!is.na(x)]
      x <- x[trimws(x) != ""]
      if (length(x) == 0) fallback else x[1]
    }
    
    compact_text <- function(x, max_chars = 140, fallback = "N/A") {
      val <- fallback_value(x, fallback = fallback)
      if (identical(val, fallback)) return(fallback)
      if (nchar(val) > max_chars) {
        paste0(substr(val, 1, max_chars), " ...")
      } else {
        val
      }
    }
    
    trait_profile <- reactive({
      if (current_mode() == "default") return(NULL)
      
      x <- selected_species_traits()
      if (is.null(x) || nrow(x) == 0) return(NULL)
      
      x[1, , drop = FALSE]
    })
    
    output$traits_intro <- renderUI({
      if (current_mode() == "default") {
        return(
          div(
            class = "mode-banner default-mode",
            div(class = "mode-banner-title", "Traits view unavailable"),
            div(
              class = "mode-banner-subtitle",
              "Select one species first. This tab works at species level."
            )
          )
        )
      }
      
      div(
        class = "mode-banner selected-mode",
        div(class = "mode-banner-title", "Species trait profile"),
        div(
          class = "mode-banner-subtitle",
          "This tab shows the species profile using the fields that actually exist in the current dataset."
        )
      )
    })
    
    output$species_card <- renderUI({
      if (current_mode() == "default") {
        return(div("No species selected."))
      }
      
      x <- trait_profile()
      req(!is.null(x))
      
      sci_name <- fallback_value(x$scientific_name)
      vern_name <- fallback_value(x$vernacular_name, "—")
      n_obs <- suppressWarnings(as.integer(x$n_obs))
      year_min <- fallback_value(x$first_year, "N/A")
      year_max <- fallback_value(x$last_year, "N/A")
      media_url <- fallback_value(x$media_url, "")
      
      div(
        class = "species-card",
        
        if (!is.null(media_url) && !is.na(media_url) && media_url != "" && media_url != "N/A") {
          tags$img(
            src = media_url,
            style = "max-width:100%; border-radius:14px; margin-bottom:12px;"
          )
        } else {
          tags$img(
            src = "https://placehold.co/500x350?text=No+image",
            style = "max-width:100%; border-radius:14px; margin-bottom:12px;"
          )
        },
        
        tags$h3(style = "margin-top:0; font-weight:800;", sci_name),
        tags$p(tags$strong("Vernacular name: "), vern_name),
        tags$p(tags$strong("Observations: "), format(n_obs, big.mark = ",")),
        tags$p(tags$strong("Temporal coverage: "), paste0(year_min, " - ", year_max)),
        tags$p(tags$strong("Media available in selection: "), fallback_value(x$n_obs_with_media, "0"))
      )
    })
    
    output$trait_summary <- renderUI({
      if (current_mode() == "default") {
        return(div("Select a species to inspect its profile."))
      }
      
      x <- trait_profile()
      req(!is.null(x))
      
      tagList(
        tags$p(tags$strong("Basis of record: "), compact_text(x$basis_record_summary)),
        tags$p(tags$strong("Datasets: "), compact_text(x$dataset_summary)),
        tags$p(tags$strong("Localities: "), compact_text(x$locality_summary)),
        tags$p(tags$strong("First event date: "), fallback_value(x$first_event_date)),
        tags$p(tags$strong("Last event date: "), fallback_value(x$last_event_date))
      )
    })
    
    output$obs_box <- renderValueBox({
      x <- trait_profile()
      
      valueBox(
        value = if (is.null(x)) "N/A" else format(as.integer(x$n_obs), big.mark = ","),
        subtitle = "Observations",
        icon = icon("binoculars"),
        color = "purple"
      )
    })
    
    output$year_box <- renderValueBox({
      x <- trait_profile()
      
      year_label <- "N/A"
      if (!is.null(x)) {
        y1 <- fallback_value(x$first_year, "N/A")
        y2 <- fallback_value(x$last_year, "N/A")
        year_label <- if (y1 == "N/A" && y2 == "N/A") "N/A" else paste0(y1, " - ", y2)
      }
      
      valueBox(
        value = year_label,
        subtitle = "Year range",
        icon = icon("calendar"),
        color = "blue"
      )
    })
    
    output$datasets_box <- renderValueBox({
      x <- trait_profile()
      
      valueBox(
        value = if (is.null(x)) "N/A" else fallback_value(x$n_datasets, "0"),
        subtitle = "Datasets",
        icon = icon("database"),
        color = "green"
      )
    })
    
    output$basis_box <- renderValueBox({
      x <- trait_profile()
      
      valueBox(
        value = if (is.null(x)) "N/A" else fallback_value(x$n_basis_types, "0"),
        subtitle = "Basis record types",
        icon = icon("layer-group"),
        color = "yellow"
      )
    })
    
    output$traits_table <- renderTable({
      if (current_mode() == "default") return(NULL)
      
      x <- trait_profile()
      req(!is.null(x))
      
      data.frame(
        Trait = c(
          "Scientific name",
          "Vernacular name",
          "Observations",
          "Year range",
          "Basis of record",
          "Number of basis record types",
          "Datasets",
          "Number of datasets",
          "Localities",
          "Number of localities",
          "Observations with media",
          "First event date",
          "Last event date"
        ),
        Value = c(
          fallback_value(x$scientific_name),
          fallback_value(x$vernacular_name, "—"),
          fallback_value(x$n_obs, "0"),
          paste0(fallback_value(x$first_year, "N/A"), " - ", fallback_value(x$last_year, "N/A")),
          fallback_value(x$basis_record_summary),
          fallback_value(x$n_basis_types, "0"),
          fallback_value(x$dataset_summary),
          fallback_value(x$n_datasets, "0"),
          fallback_value(x$locality_summary),
          fallback_value(x$n_localities, "0"),
          fallback_value(x$n_obs_with_media, "0"),
          fallback_value(x$first_event_date),
          fallback_value(x$last_event_date)
        ),
        check.names = FALSE
      )
    }, striped = TRUE, bordered = FALSE, spacing = "m")
  })
}