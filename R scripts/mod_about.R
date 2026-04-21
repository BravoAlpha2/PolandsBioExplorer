# Author: Nuno Garcia
# Date: 21/04/2026
# LinkedIn: https://www.linkedin.com/in/nuno-garcia-97b780158/
# ORCID: https://orcid.org/0000-0001-7917-3286
#
# Script purpose:
# Information tab/menu for the shiny app.

# Shiny app (main) purpose:
# Dashboard to explore biodiversity observations in Poland.
# Users can search species by vernacularName and scientificName,
# inspect observations on a map, and view a temporal timeline.

# ============================================================
# MENU/TABITEM FOR USERS - INFORMATION ABOUT THE APP
# ============================================================
mod_about_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "about",
    
    fluidRow(
      column(
        width = 12,
        div(
          class = "hero-shell",
          div(
            class = "hero-layout",
            div(
              div(class = "hero-kicker", icon("circle-info"), "About this application"),
              div(
                class = "hero-title",
                "A focused dashboard for exploring biodiversity observations across Poland."
              ),
              div(
                class = "hero-text",
                "This application is designed to help users move from broad spatial exploration to species-level inspection. It combines search, mapping, temporal summaries, and tabular outputs in a single analytical workflow."
              )
            ),
            div(
              class = "hero-side-grid",
              div(
                class = "mini-status",
                div(class = "mini-status-label", "Geographic scope"),
                div(class = "mini-status-value", "Poland")
              ),
              div(
                class = "mini-status",
                div(class = "mini-status-label", "Backend"),
                div(class = "mini-status-value", "DuckDB")
              ),
              div(
                class = "mini-status",
                div(class = "mini-status-label", "Version context"),
                div(class = "mini-status-value", "April 2026")
              )
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        box(
          width = 12,
          class = "app-card",
          title = tagList(tags$span(class = "card-icon", icon("compass")), "What this dashboard is for"),
          status = NULL,
          solidHeader = FALSE,
          div(
            class = "card-lead",
            "The dashboard is built to support fast interpretation of biodiversity records without forcing users to work through raw files or fragmented outputs."
          ),
          tags$p(
            "Its main purpose is to help users identify a species, inspect where observations are concentrated, understand how records are distributed through time, and review the underlying details in a structured and exportable format."
          ),
          tags$p(
            "Although the original occurrence source is broader, this interface is intentionally constrained to Poland in order to provide a clearer and more focused analytical experience."
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        box(
          width = 12,
          class = "app-card",
          title = tagList(tags$span(class = "card-icon", icon("layer-group")), "Core components"),
          status = NULL,
          solidHeader = FALSE,
          div(
            class = "card-lead",
            "The interface combines a small number of components, each with a clear role in the workflow."
          ),
          tags$ul(
            tags$li(tags$b("Species search and filters: "), "search by scientific or vernacular name and refine the visible records."),
            tags$li(tags$b("Observation map: "), "inspect spatial distribution and clustering patterns."),
            tags$li(tags$b("Observation timeline: "), "review temporal variation and record intensity through time."),
            tags$li(tags$b("Species details card: "), "summarize the selected species and show representative metadata."),
            tags$li(tags$b("Records table: "), "inspect detailed observations and export them in standard formats.")
          )
        )
      ),
      
      column(
        width = 6,
        box(
          width = 12,
          class = "app-card",
          title = tagList(tags$span(class = "card-icon", icon("route")), "Suggested workflow"),
          status = NULL,
          solidHeader = FALSE,
          div(
            class = "card-lead",
            "The dashboard is easiest to use when approached as a sequence rather than as isolated widgets."
          ),
          tags$ol(
            tags$li("Confirm the current selection and understand whether the app is in default or selected-species mode."),
            tags$li("Inspect the map to identify spatial spread, density, and coverage gaps."),
            tags$li("Use the timeline to distinguish broad observation effort from more stable temporal patterns."),
            tags$li("Review the details card and records table only after the broader context is clear."),
            tags$li("Export tabular outputs when you need a record-level view outside the dashboard.")
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        box(
          width = 12,
          class = "app-card",
          title = tagList(tags$span(class = "card-icon", icon("database")), "Data and performance"),
          status = NULL,
          solidHeader = FALSE,
          div(
            class = "card-lead",
            "The application is designed to stay responsive while working from larger source files."
          ),
          tags$p(
            "DuckDB is used as the backend engine to query the occurrence and multimedia CSV files efficiently, without requiring the full global dataset to be loaded directly into memory."
          ),
          tags$p(
            "This allows the dashboard to materialize only the Poland subset and the records needed for the current selection, reducing unnecessary overhead and improving usability."
          )
        )
      ),
      
      column(
        width = 6,
        box(
          width = 12,
          class = "app-card",
          title = tagList(tags$span(class = "card-icon", icon("triangle-exclamation")), "Interpretation notes"),
          status = NULL,
          solidHeader = FALSE,
          div(
            class = "card-lead",
            "The dashboard shows observation records, not direct ecological truth."
          ),
          tags$ul(
            tags$li("Observed patterns may reflect sampling effort as much as biological distribution."),
            tags$li("Temporal peaks may indicate changes in observation intensity, data mobilization, or reporting practices."),
            tags$li("Media availability is uneven and should not be interpreted as a quality indicator on its own."),
            tags$li("Absence of records in a location does not imply confirmed species absence.")
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        box(
          width = 12,
          class = "app-card",
          title = tagList(tags$span(class = "card-icon", icon("user")), "Author and contact"),
          status = NULL,
          solidHeader = FALSE,
          div(
            class = "card-lead",
            "Developed by Nuno Garcia."
          ),
          tags$p("Geospatial Data Scientist · April 2026"),
          tags$ul(
            tags$li(tags$a(href = "https://www.linkedin.com/in/nuno-garcia-97b780158/", target = "_blank", "LinkedIn")),
            tags$li(tags$a(href = "https://orcid.org/0000-0001-7917-3286", target = "_blank", "ORCID")),
            tags$li(tags$a(href = "https://www.researchgate.net/profile/Nuno-Garcia-4", target = "_blank", "ResearchGate"))
          )
        )
      )
    )
  )
}

mod_about_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}