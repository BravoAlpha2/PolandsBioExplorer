
# Author: Nuno Garcia
# Date: 21/04/2026
# LinkedIn: https://www.linkedin.com/in/nuno-garcia-97b780158/
# ORCID: https://orcid.org/0000-0001-7917-3286
#
# Shiny app (main) purpose:
# Dashboard to explore biodiversity observations in Poland.
# Users can search species by vernacularName and scientificName,
# inspect observations on a map, and view a temporal timeline.

# Main principles:
# - visible sidebar with navigation + search/filter controls
# - map-first layout with clearer analytical flow
# - cleaner cards and stronger hierarchy
# - existing modules preserved
# - selected-species filters applied consistently

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(shinyBS)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(tidyr)
  library(leaflet)
  library(leaflet.extras)
  library(plotly)
  library(DT)
  library(htmltools)
  library(DBI)
  library(duckdb)
})

source("R scripts/helpers.R")
source("R scripts/data_access.R")
source("R scripts/mod_species_search.R")
source("R scripts/mod_value_boxes.R")
source("R scripts/mod_observation_map.R")
source("R scripts/mod_timeline.R")
source("R scripts/mod_species_details.R")
source("R scripts/mod_about.R")
source("R scripts/mod_traits.R")

path_multimedia <- "Data/biodiversity-data/multimedia_poland.csv"
path_occurrence <- "Data/biodiversity-data/occurence_poland.csv"

con_global <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

onStop(function() {
  try(dbDisconnect(con_global, shutdown = TRUE), silent = TRUE)
})

app_data <- load_app_data(
  con = con_global,
  path_occurrence = path_occurrence,
  path_multimedia = path_multimedia
)

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    titleWidth = 400,
    title = tags$div(
      class = "topbar-brand",
      tags$div(class = "topbar-mark"),
      tags$div(
        class = "topbar-brand-copy",
        tags$div(class = "topbar-title", "Poland´s BioExplorer"),
        tags$div(class = "topbar-subtitle", "Spatial and temporal biodiversity exploration")
      )
    )
  ),
  
  dashboardSidebar(
    width = 400,
    div(
      class = "sidebar-shell",
      
      br(),
      br(),
      
      div(
        class = "sidebar-brand-block",
        div(class = "sidebar-brand-title", "Explore biodiversity observations"),
        div(
          class = "sidebar-brand-copy",
          "Search species, filter records, inspect spatial patterns, validate trends, and export detailed observations."
        )
      ),
      
      sidebarMenu(
        id = "tabs",
        menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
        menuItem("Traits", tabName = "traits", icon = icon("list-alt")),
        menuItem("About", tabName = "about", icon = icon("circle-info"))
      ),
      
      hr(),
      
      div(class = "sidebar-section-label", "Search and filters"),
      div(
        class = "sidebar-module-card",
        mod_species_search_ui("species_search")
      ),
      
      div(
        class = "sidebar-footer-note",
        tags$strong("Workflow: "),
        "Confirm the current selection, inspect the map, validate patterns over time, then review detailed records and exports."
      ),
      br(),
      HTML(paste0(
        "<script>",
        "var today = new Date();",
        "var yyyy = today.getFullYear();",
        "</script>",
        "<p style = 'text-align: center;'><small>&copy; - <a href='https://www.appsilon.com/' target='_blank'>Developed by Appsilon</a> - <script>document.write(yyyy);</script></small></p>")
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        :root {
          --bg: #f5f7fb;
          --surface: #ffffff;
          --surface-2: #f8faff;
          --surface-3: #f3f6fc;
          --text: #101828;
          --muted: #5b6472;
          --soft: #7a8597;
          --line: #e5eaf2;
          --brand: #5b3cc4;
          --brand-dark: #42239f;
          --brand-soft: #efeaff;
          --accent: #f59e0b;
          --success: #1c8c63;
          --shadow-sm: 0 4px 14px rgba(16, 24, 40, 0.05);
          --shadow-md: 0 14px 32px rgba(16, 24, 40, 0.08);
          --shadow-lg: 0 18px 40px rgba(16, 24, 40, 0.10);
          --radius-sm: 14px;
          --radius-md: 18px;
          --radius-lg: 24px;
        }

        html, body {
          background: var(--bg);
          color: var(--text);
          font-family: Inter, system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        }

        .skin-black .main-header .logo,
        .skin-black .main-header .navbar {
          background: linear-gradient(90deg, var(--brand-dark) 0%, var(--brand) 100%);
          box-shadow: 0 2px 12px rgba(0,0,0,0.12);
        }

        .skin-black .main-header .logo:hover,
        .skin-black .main-header .navbar:hover {
          background: linear-gradient(90deg, var(--brand-dark) 0%, var(--brand) 100%);
        }

        .topbar-brand {
          height: 50px;
          display: flex;
          align-items: center;
          gap: 12px;
        }

        .topbar-mark {
          width: 14px;
          height: 14px;
          border-radius: 50%;
          background: #ffffff;
          box-shadow: 0 0 0 4px rgba(255,255,255,0.18);
          flex-shrink: 0;
        }

        .topbar-brand-copy {
          display: flex;
          flex-direction: column;
          line-height: 1.05;
        }

        .topbar-title {
          color: #ffffff;
          font-size: 18px;
          font-weight: 800;
          letter-spacing: 0.1px;
        }

        .topbar-subtitle {
          color: rgba(255,255,255,0.82);
          font-size: 11px;
          font-weight: 500;
        }

        .topbar-info-icon {
          color: #ffffff !important;
          font-size: 18px;
          padding: 15px 18px;
          display: block;
          text-decoration: none !important;
        }

        .topbar-info-icon:hover,
        .topbar-info-icon:focus {
          color: #f3f4f6 !important;
          background: rgba(255,255,255,0.08);
          text-decoration: none !important;
        }

        .popover {
          max-width: 420px;
          border-radius: 14px;
          border: 1px solid #e5e7eb;
          box-shadow: 0 14px 30px rgba(0,0,0,0.12);
        }

        .popover-title {
          font-weight: 800;
        }

        .main-sidebar {
          background: #1f2430;
          padding-top: 0;
        }

        .sidebar {
          background: #1f2430;
        }

        .sidebar-shell {
          padding-bottom: 18px;
        }

        .sidebar-brand-block {
          padding: 18px 18px 14px 18px;
          border-bottom: 1px solid rgba(255,255,255,0.08);
          margin-bottom: 8px;
        }

        .sidebar-brand-title {
          color: #ffffff;
          font-size: 16px;
          font-weight: 800;
          margin-bottom: 6px;
        }

        .sidebar-brand-copy {
          color: #c7cfdb;
          font-size: 12px;
          line-height: 1.6;
        }

        .sidebar-section-label {
          color: #aab4c4;
          font-size: 11px;
          font-weight: 800;
          letter-spacing: 0.7px;
          text-transform: uppercase;
          padding: 14px 18px 8px 18px;
        }

        .sidebar-menu {
          margin-bottom: 8px;
        }

        .sidebar-menu > li > a {
          font-weight: 700;
        }

        .skin-black .sidebar-menu > li.active > a {
          border-left-color: #8b6cff;
          background: rgba(255,255,255,0.06);
        }

        .sidebar-module-card,
        .sidebar-selection-card {
          margin: 0 12px 0 12px;
          border: 1px solid rgba(255,255,255,0.08);
          background: rgba(255,255,255,0.04);
          border-radius: 16px;
          box-shadow: none;
        }

        .sidebar-selection-card {
          padding: 14px;
        }

        .sidebar-selection-title {
          color: #aab4c4;
          font-size: 11px;
          font-weight: 800;
          letter-spacing: 0.6px;
          text-transform: uppercase;
          margin-bottom: 8px;
        }

        .sidebar-selection-main {
          color: #ffffff;
          font-size: 16px;
          font-weight: 800;
          line-height: 1.3;
          margin-bottom: 8px;
          word-break: break-word;
        }

        .sidebar-selection-meta {
          color: #d8deea;
          font-size: 12px;
          line-height: 1.6;
        }

        .sidebar-footer-note {
          color: #c7cfdb;
          font-size: 12px;
          line-height: 1.6;
          padding: 14px 18px 0 18px;
        }

        .content-wrapper, .right-side {
          background: linear-gradient(180deg, #f5f7fb 0%, #f9fbff 100%);
        }

        .content {
          padding: 22px;
        }

        .hero-shell {
          background: linear-gradient(135deg, #ffffff 0%, #f2edff 100%);
          border: 1px solid #e7defd;
          border-radius: 26px;
          box-shadow: var(--shadow-md);
          padding: 28px;
          margin-bottom: 18px;
        }

        .hero-layout {
          display: grid;
          grid-template-columns: minmax(0, 1.7fr) minmax(260px, 0.8fr);
          gap: 18px;
          align-items: start;
        }

        .hero-kicker {
          display: inline-flex;
          align-items: center;
          gap: 8px;
          background: var(--brand-soft);
          color: var(--brand);
          border: 1px solid #ddd0ff;
          border-radius: 999px;
          padding: 7px 12px;
          font-size: 11px;
          font-weight: 800;
          letter-spacing: 0.7px;
          text-transform: uppercase;
          margin-bottom: 14px;
        }

        .hero-title {
          font-size: 32px;
          line-height: 1.08;
          font-weight: 800;
          color: var(--text);
          margin-bottom: 12px;
          max-width: 820px;
        }

        .hero-text {
          color: var(--muted);
          font-size: 14px;
          line-height: 1.75;
          max-width: 820px;
        }

        .hero-side-grid {
          display: grid;
          gap: 12px;
        }

        .mini-status {
          background: rgba(255,255,255,0.95);
          border: 1px solid var(--line);
          border-radius: 16px;
          padding: 14px 15px;
          box-shadow: var(--shadow-sm);
        }

        .mini-status-label {
          color: var(--soft);
          font-size: 11px;
          font-weight: 800;
          letter-spacing: 0.6px;
          text-transform: uppercase;
          margin-bottom: 4px;
        }

        .mini-status-value {
          color: var(--text);
          font-size: 14px;
          font-weight: 700;
          line-height: 1.4;
        }

        .mode-banner {
          background: #ffffff;
          border: 1px solid var(--line);
          border-radius: 20px;
          padding: 16px 18px;
          box-shadow: var(--shadow-sm);
          margin-bottom: 18px;
        }

        .mode-banner.default-mode {
          border-left: 6px solid var(--brand);
        }

        .mode-banner.selected-mode {
          border-left: 6px solid var(--accent);
        }

        .mode-banner-title {
          color: var(--text);
          font-size: 16px;
          font-weight: 800;
          margin-bottom: 4px;
        }

        .mode-banner-subtitle {
          color: var(--muted);
          font-size: 13px;
          line-height: 1.55;
        }

        .app-card.box {
          background: var(--surface);
          border: 1px solid var(--line);
          border-top: 0 !important;
          border-radius: var(--radius-lg);
          box-shadow: var(--shadow-md);
          overflow: hidden;
          margin-bottom: 18px;
        }

        .app-card > .box-header {
          padding: 18px 20px 8px 20px;
          border-bottom: 0;
        }

        .app-card > .box-body {
          padding: 8px 20px 20px 20px;
        }

        .box-title {
          color: var(--text);
          font-size: 18px;
          font-weight: 800;
        }

        .card-icon {
          color: var(--brand);
          margin-right: 8px;
        }

        .card-lead,
        .section-subtitle {
          color: var(--muted);
          font-size: 13px;
          line-height: 1.6;
          margin-top: 6px;
          margin-bottom: 14px;
        }

        .selection-card {
          background: linear-gradient(180deg, #ffffff 0%, #f9fbff 100%);
          border: 1px solid var(--line);
          border-radius: 18px;
          padding: 16px;
          box-shadow: var(--shadow-sm);
          margin-bottom: 14px;
        }

        .selection-card-title {
          color: var(--soft);
          font-size: 11px;
          font-weight: 800;
          letter-spacing: 0.6px;
          text-transform: uppercase;
          margin-bottom: 8px;
        }

        .selection-card-main {
          color: var(--text);
          font-size: 18px;
          font-weight: 800;
          line-height: 1.3;
          margin-bottom: 8px;
          word-break: break-word;
        }

        .selection-card-meta {
          color: #374151;
          font-size: 13px;
          line-height: 1.65;
        }

        .small-box {
          border-radius: 20px;
          box-shadow: var(--shadow-md);
          overflow: hidden;
        }

        .small-box h3 {
          font-size: 26px;
          font-weight: 800;
          white-space: normal;
        }

        .small-box p {
          font-size: 13px;
          line-height: 1.45;
        }

        .leaflet-container {
          border-radius: 16px;
        }

        .species-card img {
          border-radius: 14px;
          margin-bottom: 12px;
          max-width: 100%;
        }

        .dataTables_wrapper .dataTables_filter input {
          border-radius: 10px;
          border: 1px solid #d1d5db;
          padding: 6px 10px;
        }

        @media (max-width: 1200px) {
          .hero-layout { grid-template-columns: 1fr; }
        }

        @media (max-width: 991px) {
          .content { padding: 14px; }
          .hero-title { font-size: 28px; }
        }
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "overview",
        
        fluidRow(
          column(
            width = 12,
            div(
              class = "hero-shell",
              div(
                class = "hero-layout",
                div(
                  div(class = "hero-kicker", icon("binoculars"), "Poland biodiversity exploration"),
                  div(class = "hero-title", "Explore biodiversity patterns with a layout that matches the actual analytical workflow."),
                  div(
                    class = "hero-text",
                    "Use the sidebar to search and filter records, then read the current state, inspect the map, validate temporal patterns, and finally review detailed records and exports."
                  )
                ),
                div(
                  class = "hero-side-grid",
                  div(
                    class = "mini-status",
                    div(class = "mini-status-label", "Coverage"),
                    div(class = "mini-status-value", "Poland-only observation view")
                  ),
                  div(
                    class = "mini-status",
                    div(class = "mini-status-label", "Engine"),
                    div(class = "mini-status-value", "DuckDB-backed exploration")
                  ),
                  div(
                    class = "mini-status",
                    div(class = "mini-status-label", "Last update"),
                    div(class = "mini-status-value", "21 April 2026")
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            uiOutput("mode_banner_ui")
          )
        ),
        
        mod_value_boxes_ui("value_boxes"),
        
        fluidRow(
          column(
            width = 6,
            box(
              width = 12,
              class = "app-card",
              title = tagList(tags$span(class = "card-icon", icon("map-location-dot")), "Species occurrences map"),
              status = NULL,
              solidHeader = FALSE,
              mod_observation_map_ui("obs_map")
            )
          ),
          
          column(
            width = 6,
            box(
              width = 12,
              class = "app-card",
              title = tagList(tags$span(class = "card-icon", icon("chart-line")), "Observation timeline"),
              status = NULL,
              solidHeader = FALSE,
              mod_timeline_ui("timeline")
            )
          ),
          column(
            width = 6,
            box(
            width = 12,
            class = "app-card",
            title = tagList(tags$span(class = "card-icon", icon("paw")), "Species card"),
            status = NULL,
            solidHeader = FALSE,
            mod_species_card_ui("species_details")
          )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              class = "app-card",
              title = tagList(tags$span(class = "card-icon", icon("table")), "Species records"),
              status = NULL,
              solidHeader = FALSE,
              mod_species_table_ui("species_details")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "traits",
        mod_traits_ui("traits")
      ),
      
      tabItem(
        tabName = "about",
        mod_about_ui("about")
      )
    )
  )
)
    
    

server <- function(input, output, session) {
  
  addPopover(
    session = session,
    id = "about_topbar_info",
    title = HTML("<b>About this dashboard</b>"),
    content = HTML(paste0(
      "<p><b>General overview</b><br>",
      "This dashboard explores biodiversity observations from Poland using a larger occurrence dataset originally covering the world.</p>",
      "<p><b>What it shows</b><br>",
      "• Searchable species selector using vernacular and scientific names.<br>",
      "• Map of selected-species observations or Poland-wide overview.<br>",
      "• Timeline of observations through time.<br>",
      "• Summary table with top species or selected-species records.</p>",
      "<p><b>Author</b><br>",
      "Nuno Garcia, Geospatial Data Scientist (April 2026)<br>",
      "<a href='https://www.linkedin.com/in/nuno-garcia-97b780158/' target='_blank'>LinkedIn</a><br>",
      "<a href='https://orcid.org/0000-0001-7917-3286' target='_blank'>ORCID</a><br>",
      "<a href='https://www.researchgate.net/profile/Nuno-Garcia-4' target='_blank'>ResearchGate</a></p>"
    )),
    placement = "bottom",
    trigger = "click",
    options = list(container = "body", html = TRUE)
  )
  
  search_state <- mod_species_search_server(
    id = "species_search",
    species_index = app_data$species_index
  )
  
  selected_species_data <- reactive({
    req(search_state$selected_species())
    
    dat <- get_selected_species_data(
      app_data$con,
      search_state$selected_species()
    )
    
    req(!is.null(dat))
    
    if (isTRUE(search_state$only_with_media()) && "media_url" %in% names(dat)) {
      dat <- dat %>%
        dplyr::filter(!is.na(media_url) & media_url != "")
    }
    
    if (!is.null(search_state$year_range()) &&
        length(search_state$year_range()) == 2 &&
        "observation_year" %in% names(dat)) {
      dat <- dat %>%
        dplyr::filter(
          !is.na(observation_year),
          observation_year >= search_state$year_range()[1],
          observation_year <= search_state$year_range()[2]
        )
    }
    
    dat
  })
  
  selected_species_traits <- reactive({
    req(search_state$selected_species())
    
    traits <- get_selected_species_traits(
      app_data$con,
      search_state$selected_species()
    )
    
    req(!is.null(traits), nrow(traits) > 0)
    traits
  })
  
  current_mode <- reactive({
    if (is.null(search_state$selected_species())) "default" else "selected"
  })
  
  current_map_data <- reactive({
    if (current_mode() == "default") {
      app_data$default_map_data
    } else {
      selected_species_data()
    }
  })
  
  current_timeline_data <- reactive({
    if (current_mode() == "default") {
      app_data$default_timeline
    } else {
      dat <- selected_species_data()
      
      if (is.null(dat) || nrow(dat) == 0) {
        return(data.frame(
          observation_year = numeric(0),
          n_obs = numeric(0)
        ))
      }
      
      dat %>%
        dplyr::filter(!is.na(observation_year)) %>%
        dplyr::count(observation_year, name = "n_obs") %>%
        dplyr::arrange(observation_year)
    }
  })
  
  selection_summary <- reactive({
    if (current_mode() == "default") {
      return(list(
        title = "National overview",
        subtitle = "No species selected",
        obs_n = app_data$default_obs_count,
        years = NULL
      ))
    }
    
    dat <- selected_species_data()
    obs_n <- if (!is.null(dat) && nrow(dat) > 0) nrow(dat) else 0
    
    year_values <- NULL
    possible_year_cols <- c("observation_year", "year", "event_year")
    year_col_found <- possible_year_cols[possible_year_cols %in% names(dat)]
    
    if (length(year_col_found) > 0 && obs_n > 0) {
      year_values <- suppressWarnings(as.numeric(dat[[year_col_found[1]]]))
      year_values <- year_values[!is.na(year_values)]
    } else if ("event_date" %in% names(dat) && obs_n > 0) {
      year_values <- suppressWarnings(lubridate::year(as.Date(dat$event_date)))
      year_values <- year_values[!is.na(year_values)]
    }
    
    year_label <- NULL
    if (!is.null(year_values) && length(year_values) > 0) {
      year_label <- paste0(min(year_values), " - ", max(year_values))
    }
    
    list(
      title = as.character(search_state$selected_species()),
      subtitle = "Selected species",
      obs_n = obs_n,
      years = year_label
    )
  })
  
  output$sidebar_selection_ui <- renderUI({
    x <- selection_summary()
    
    div(
      class = "sidebar-selection-card",
      div(class = "sidebar-selection-title", "Current selection"),
      div(class = "sidebar-selection-main", x$title),
      div(
        class = "sidebar-selection-meta",
        tags$div(tags$strong("Mode: "), x$subtitle),
        tags$div(tags$strong("Observations: "), format(x$obs_n, big.mark = ",")),
        if (!is.null(x$years)) tags$div(tags$strong("Years: "), x$years)
      )
    )
  })
  
  output$selection_summary_ui <- renderUI({
    x <- selection_summary()
    
    div(
      class = "selection-card",
      div(class = "selection-card-title", "Current selection"),
      div(class = "selection-card-main", x$title),
      div(
        class = "selection-card-meta",
        tags$div(tags$strong("Mode: "), x$subtitle),
        tags$div(tags$strong("Observations: "), format(x$obs_n, big.mark = ",")),
        if (!is.null(x$years)) tags$div(tags$strong("Years: "), x$years)
      )
    )
  })
  
  output$mode_banner_ui <- renderUI({
    if (current_mode() == "default") {
      div(
        class = "mode-banner default-mode",
        div(class = "mode-banner-title", "Default mode: national biodiversity overview"),
        div(
          class = "mode-banner-subtitle",
          "The app is currently showing all available biodiversity observations for Poland. Use the sidebar to search for a species and narrow the view with filters."
        )
      )
    } else {
      div(
        class = "mode-banner selected-mode",
        div(
          class = "mode-banner-title",
          paste0("Selected species mode: ", as.character(search_state$selected_species()))
        ),
        div(
          class = "mode-banner-subtitle",
          "The map, timeline and details panels are now focused on the selected species and reflect the active sidebar filters."
        )
      )
    }
  })
  
  mod_value_boxes_server(
    id = "value_boxes",
    current_mode = current_mode,
    selected_species = search_state$selected_species,
    selected_species_data = selected_species_data,
    default_obs_count = app_data$default_obs_count,
    default_species_count = app_data$default_species_count
  )
  
  mod_observation_map_server(
    id = "obs_map",
    poland_bbox = app_data$poland_bbox,
    current_mode = current_mode,
    current_map_data = current_map_data,
    selected_species = search_state$selected_species,
    show_heatmap = search_state$show_heatmap,
    show_clusters = search_state$show_clusters,
    point_radius = search_state$point_radius
  )
  
  mod_timeline_server(
    id = "timeline",
    current_timeline_data = current_timeline_data
  )
  
  mod_species_details_server(
    id = "species_details",
    current_mode = current_mode,
    selected_species_data = selected_species_data,
    default_top_species = app_data$default_top_species
  )
  
  mod_traits_server(
    id = "traits",
    current_mode = current_mode,
    selected_species_data = selected_species_data,
    selected_species_traits = selected_species_traits
  )
  
  mod_about_server("about")
}

shinyApp(ui = ui, server = server)