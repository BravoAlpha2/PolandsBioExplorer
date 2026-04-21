# Author: Nuno Garcia
# Date: 21/04/2026
# LinkedIn: https://www.linkedin.com/in/nuno-garcia-97b780158/
# ORCID: https://orcid.org/0000-0001-7917-3286
# ResearchGate: https://www.researchgate.net/profile/Nuno-Garcia-4
#
# Shiny app purpose:
# Dashboard to explore biodiversity observations in Poland.
# Users can search species by vernacularName and scientificName,
# inspect observations on a map, and view a temporal timeline.
#
# Design choices:
# - The source dataset is large and global, but the app uses only Poland.
# - DuckDB is used to avoid loading the full CSVs into memory.
# - Default state is useful, not empty:
#     * map = all Polish observations clustered
#     * timeline = total observations per year in Poland
#     * side panel = most observed species in Poland
# - Some UI ideas:
#     * dashboard layout
#     * info/welcome panel
#     * value boxes
#     * leaflet tools (minimap, measure, layer controls)
#     * species information panel

# ============================================================
# 1) PACKAGES
# ============================================================
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

# ============================================================
# 2) PATHS
# ============================================================
path_multimedia <- "C:/Users/nunog/OneDrive/Desktop/Shiny - Appsilon Interview/Data/biodiversity-data/multimedia.csv"
path_occurrence <- "C:/Users/nunog/OneDrive/Desktop/Shiny - Appsilon Interview/Data/biodiversity-data/occurence.csv"

# ============================================================
# 3) HELPERS
# ============================================================
clean_label <- function(x) {
  x <- as.character(x)
  x <- ifelse(is.na(x) | stringr::str_trim(x) == "", NA_character_, stringr::str_squish(x))
  x
}

safe_int <- function(x) suppressWarnings(as.integer(x))

mode_value <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

sql_escape <- function(x) {
  DBI::dbQuoteString(con_global, x) |> as.character()
}

# ============================================================
# 4) DUCKDB CONNECTION
# ============================================================
con_global <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

onStop(function() {
  try(dbDisconnect(con_global, shutdown = TRUE), silent = TRUE)
})

# ============================================================
# 5) BUILD POLAND TABLES INSIDE DUCKDB
# ============================================================
poland_bbox <- list(
  xmin = 14.07,
  xmax = 24.20,
  ymin = 49.00,
  ymax = 54.90
)

# ------------------------------------------------------------
# 5.1) Detect occurrence CSV columns
# ------------------------------------------------------------
occ_cols <- dbGetQuery(
  con_global,
  sprintf(
    "DESCRIBE SELECT * FROM read_csv_auto('%s', header = TRUE, all_varchar = TRUE)",
    gsub("\\\\", "/", path_occurrence)
  )
)$column_name

pick_col <- function(candidates, cols) {
  hit <- candidates[candidates %in% cols]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

sql_col_or_null <- function(col, alias, cast = "VARCHAR", trim = FALSE, null_if_empty = FALSE) {
  if (is.na(col)) {
    return(sprintf("CAST(NULL AS %s) AS %s", cast, alias))
  }
  
  expr <- col
  if (trim) expr <- sprintf("TRIM(%s)", expr)
  if (null_if_empty) expr <- sprintf("NULLIF(%s, '')", expr)
  
  if (!is.null(cast)) {
    expr <- sprintf("CAST(%s AS %s)", expr, cast)
  }
  
  sprintf("%s AS %s", expr, alias)
}

sql_try_cast_or_null <- function(col, alias, cast_type = "DOUBLE") {
  if (is.na(col)) {
    return(sprintf("CAST(NULL AS %s) AS %s", cast_type, alias))
  }
  sprintf("TRY_CAST(%s AS %s) AS %s", col, cast_type, alias)
}

occ_col_lon        <- pick_col(c("decimalLongitude", "longitudeDecimal", "longitude", "lon"), occ_cols)
occ_col_lat        <- pick_col(c("decimalLatitude", "latitudeDecimal", "latitude", "lat"), occ_cols)
occ_col_scientific <- pick_col(c("scientificName", "scientific_name"), occ_cols)
occ_col_vernacular <- pick_col(c("vernacularName", "vernacular_name"), occ_cols)
occ_col_country    <- pick_col(c("country", "countryCode", "country_code", "countryName"), occ_cols)
occ_col_basis      <- pick_col(c("basisOfRecord", "basis_of_record"), occ_cols)
occ_col_occid      <- pick_col(c("occurrenceID", "occurrence_id", "id"), occ_cols)
occ_col_id         <- pick_col(c("id"), occ_cols)
occ_col_dataset    <- pick_col(c("datasetName", "dataset_name"), occ_cols)
occ_col_locality   <- pick_col(c("locality", "municipality", "stateProvince", "state_province"), occ_cols)
occ_col_eventdate  <- pick_col(c("eventDate", "event_date"), occ_cols)
occ_col_year       <- pick_col(c("year"), occ_cols)
occ_col_month      <- pick_col(c("month"), occ_cols)
occ_col_day        <- pick_col(c("day"), occ_cols)

if (is.na(occ_col_lon) || is.na(occ_col_lat) || is.na(occ_col_scientific)) {
  stop("Required occurrence columns were not found: longitude, latitude, or scientific name.")
}

occurrence_id_expr <- if (!is.na(occ_col_occid) && !is.na(occ_col_id) && occ_col_occid != occ_col_id) {
  sprintf("CAST(COALESCE(%s, %s) AS VARCHAR) AS occurrence_id", occ_col_occid, occ_col_id)
} else if (!is.na(occ_col_occid)) {
  sprintf("CAST(%s AS VARCHAR) AS occurrence_id", occ_col_occid)
} else if (!is.na(occ_col_id)) {
  sprintf("CAST(%s AS VARCHAR) AS occurrence_id", occ_col_id)
} else {
  "CAST(NULL AS VARCHAR) AS occurrence_id"
}

occ_sql <- sprintf(
  "
  CREATE OR REPLACE TABLE occurrence_poland AS
  WITH src AS (
    SELECT
      %s,
      %s,
      %s,
      %s,
      %s,
      %s,
      %s,
      %s,
      %s,
      %s,
      %s,
      %s,
      %s
    FROM read_csv_auto(
      '%s',
      header = TRUE,
      ignore_errors = TRUE,
      all_varchar = TRUE
    )
  )
  SELECT
    longitude,
    latitude,
    scientific_name,
    vernacular_name,
    country_value,
    basis_record,
    occurrence_id,
    dataset_name,
    locality_name,
    event_date_raw,
    year_value,
    month_value,
    day_value,
    CASE
      WHEN event_date_raw IS NOT NULL AND TRY_CAST(event_date_raw AS DATE) IS NOT NULL
        THEN TRY_CAST(event_date_raw AS DATE)
      WHEN year_value IS NOT NULL AND month_value IS NOT NULL AND day_value IS NOT NULL
        THEN TRY_CAST(MAKE_DATE(year_value, month_value, day_value) AS DATE)
      WHEN year_value IS NOT NULL AND month_value IS NOT NULL
        THEN TRY_CAST(MAKE_DATE(year_value, month_value, 1) AS DATE)
      WHEN year_value IS NOT NULL
        THEN TRY_CAST(MAKE_DATE(year_value, 1, 1) AS DATE)
      ELSE NULL
    END AS event_date,
    COALESCE(
      YEAR(
        CASE
          WHEN event_date_raw IS NOT NULL AND TRY_CAST(event_date_raw AS DATE) IS NOT NULL
            THEN TRY_CAST(event_date_raw AS DATE)
          WHEN year_value IS NOT NULL AND month_value IS NOT NULL AND day_value IS NOT NULL
            THEN TRY_CAST(MAKE_DATE(year_value, month_value, day_value) AS DATE)
          WHEN year_value IS NOT NULL AND month_value IS NOT NULL
            THEN TRY_CAST(MAKE_DATE(year_value, month_value, 1) AS DATE)
          WHEN year_value IS NOT NULL
            THEN TRY_CAST(MAKE_DATE(year_value, 1, 1) AS DATE)
          ELSE NULL
        END
      ),
      year_value
    ) AS observation_year
  FROM src
  WHERE longitude IS NOT NULL
    AND latitude IS NOT NULL
    AND longitude BETWEEN -180 AND 180
    AND latitude BETWEEN -90 AND 90
    AND scientific_name IS NOT NULL
    AND (
      LOWER(country_value) IN ('poland', 'polska', 'pl')
      OR (
        country_value IS NULL
        AND longitude BETWEEN %f AND %f
        AND latitude BETWEEN %f AND %f
      )
    )
  ",
  sql_try_cast_or_null(occ_col_lon, "longitude", "DOUBLE"),
  sql_try_cast_or_null(occ_col_lat, "latitude", "DOUBLE"),
  sql_col_or_null(occ_col_scientific, "scientific_name", cast = "VARCHAR", trim = TRUE, null_if_empty = TRUE),
  sql_col_or_null(occ_col_vernacular, "vernacular_name", cast = "VARCHAR", trim = TRUE, null_if_empty = TRUE),
  sql_col_or_null(occ_col_country, "country_value", cast = "VARCHAR", trim = TRUE, null_if_empty = TRUE),
  sql_col_or_null(occ_col_basis, "basis_record", cast = "VARCHAR", trim = TRUE, null_if_empty = TRUE),
  occurrence_id_expr,
  sql_col_or_null(occ_col_dataset, "dataset_name", cast = "VARCHAR", trim = TRUE, null_if_empty = TRUE),
  sql_col_or_null(occ_col_locality, "locality_name", cast = "VARCHAR", trim = TRUE, null_if_empty = TRUE),
  sql_col_or_null(occ_col_eventdate, "event_date_raw", cast = "VARCHAR"),
  sql_try_cast_or_null(occ_col_year, "year_value", "INTEGER"),
  sql_try_cast_or_null(occ_col_month, "month_value", "INTEGER"),
  sql_try_cast_or_null(occ_col_day, "day_value", "INTEGER"),
  gsub("\\\\", "/", path_occurrence),
  poland_bbox$xmin, poland_bbox$xmax, poland_bbox$ymin, poland_bbox$ymax
)

dbExecute(con_global, occ_sql)

n_poland <- dbGetQuery(con_global, "SELECT COUNT(*) AS n FROM occurrence_poland")$n[[1]]
if (is.na(n_poland) || n_poland == 0) {
  stop("No observations remained after Poland filtering. Check your country field and coordinates.")
}

# ------------------------------------------------------------
# 5.2) Detect multimedia CSV columns
# ------------------------------------------------------------
mult_cols <- dbGetQuery(
  con_global,
  sprintf(
    "DESCRIBE SELECT * FROM read_csv_auto('%s', header = TRUE, all_varchar = TRUE)",
    gsub("\\\\", "/", path_multimedia)
  )
)$column_name

mult_col_occid <- pick_col(c("occurrenceID", "occurrence_id", "id"), mult_cols)
mult_col_id    <- pick_col(c("id"), mult_cols)
mult_col_media <- pick_col(c("identifier", "references", "accessURI", "access_uri"), mult_cols)

mult_occurrence_id_expr <- if (!is.na(mult_col_occid) && !is.na(mult_col_id) && mult_col_occid != mult_col_id) {
  sprintf("CAST(COALESCE(%s, %s) AS VARCHAR) AS occurrence_id", mult_col_occid, mult_col_id)
} else if (!is.na(mult_col_occid)) {
  sprintf("CAST(%s AS VARCHAR) AS occurrence_id", mult_col_occid)
} else if (!is.na(mult_col_id)) {
  sprintf("CAST(%s AS VARCHAR) AS occurrence_id", mult_col_id)
} else {
  "CAST(NULL AS VARCHAR) AS occurrence_id"
}

mult_media_expr <- if (!is.na(mult_col_media)) {
  sprintf("CAST(%s AS VARCHAR) AS media_url", mult_col_media)
} else {
  "CAST(NULL AS VARCHAR) AS media_url"
}

media_sql <- sprintf(
  "
  CREATE OR REPLACE TABLE media_lookup AS
  SELECT occurrence_id, media_url
  FROM (
    SELECT
      %s,
      %s,
      ROW_NUMBER() OVER (
        PARTITION BY occurrence_id
        ORDER BY media_url
      ) AS rn
    FROM read_csv_auto(
      '%s',
      header = TRUE,
      ignore_errors = TRUE,
      all_varchar = TRUE
    )
  ) x
  WHERE occurrence_id IS NOT NULL
    AND media_url IS NOT NULL
    AND media_url <> ''
    AND rn = 1
  ",
  mult_occurrence_id_expr,
  mult_media_expr,
  gsub("\\\\", "/", path_multimedia)
)

dbExecute(con_global, media_sql)

dbExecute(con_global, "
  CREATE OR REPLACE VIEW occurrence_poland_enriched AS
  SELECT o.*, m.media_url
  FROM occurrence_poland o
  LEFT JOIN media_lookup m
    ON o.occurrence_id = m.occurrence_id
")

# ============================================================
# 6) DEFAULT CONTENT FROM DUCKDB
# ============================================================
species_index <- dbGetQuery(con_global, "
  SELECT DISTINCT
    scientific_name,
    vernacular_name,
    CASE
      WHEN vernacular_name IS NOT NULL AND vernacular_name <> ''
        THEN vernacular_name || ' — ' || scientific_name
      ELSE scientific_name
    END AS display_label
  FROM occurrence_poland_enriched
  ORDER BY display_label
")

default_timeline <- dbGetQuery(con_global, "
  SELECT observation_year, COUNT(*) AS n_obs
  FROM occurrence_poland_enriched
  WHERE observation_year IS NOT NULL
  GROUP BY observation_year
  ORDER BY observation_year
")

default_top_species <- dbGetQuery(con_global, "
  SELECT
    scientific_name,
    COALESCE(vernacular_name, '—') AS vernacular_name,
    COUNT(*) AS n_obs
  FROM occurrence_poland_enriched
  GROUP BY scientific_name, COALESCE(vernacular_name, '—')
  ORDER BY n_obs DESC, scientific_name
  LIMIT 15
")

default_obs_count <- dbGetQuery(con_global, "SELECT COUNT(*) AS n_obs FROM occurrence_poland_enriched")$n_obs[[1]]
default_species_count <- dbGetQuery(con_global, "SELECT COUNT(DISTINCT scientific_name) AS n_species FROM occurrence_poland_enriched")$n_species[[1]]

# Default map data can still be heavy, but at least it is Poland-only.
default_map_data <- dbGetQuery(con_global, "
  SELECT
    longitude,
    latitude,
    scientific_name,
    vernacular_name,
    observation_year,
    locality_name,
    dataset_name
  FROM occurrence_poland_enriched
")

# ============================================================
# 7) UI
# ============================================================
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Poland´s Biodiversity", titleWidth = 300),
  dashboardSidebar(
    width = 400,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("globe-europe")),
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      div(
        style = "padding: 12px;",
        h4("How it works"),
        p("Type either a vernacular name or a scientific name."),
        p("Pick one result to update the map, timeline, and summary."),
        p("Without a selection, the app shows a Poland overview.")
        ),
      br(),
      div(
        style = "padding: 12px;",
        selectizeInput(
          inputId = "species_search",
          label = "Search species",
          choices = NULL,
          selected = NULL,
          multiple = FALSE,
          options = list(
            placeholder = "Search by vernacular or scientific name"
          )
        ),
        actionButton("clear_species", "Clear selection", icon = icon("times"), width = "60%"),
        br(), br(),
        h3("Options"),
        checkboxInput("show_heatmap", "Show heatmap in default view", value = FALSE),
        checkboxInput("show_clusters", "Cluster points", value = TRUE),
        sliderInput("point_radius", "Point radius", min = 3, max = 10, value = 5, step = 1)
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .small-box h3 {font-size: 24px; white-space: normal;}
        .content-wrapper, .right-side {background-color: #f4f6f9;}
        .box {border-radius: 8px;}
        .species-card img {border-radius: 6px; margin-bottom: 10px;}
      "))
    ),
    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("vb_total_obs", width = 4),
          valueBoxOutput("vb_total_species", width = 4),
          valueBoxOutput("vb_current_filter", width = 4)
        ),
        fluidRow(
          box(
            width = 8,
            title = "Observation map",
            status = "primary",
            solidHeader = TRUE,
            leafletOutput("obs_map", height = 620)
          ),
          box(
            width = 4,
            title = "Species details",
            status = "warning",
            solidHeader = TRUE,
            htmlOutput("species_card"),
            br(),
            DTOutput("summary_table")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Observation timeline",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("timeline_plot", height = 320)
          )
        ),
        fluidRow(
          infoBox(
            title = "Default view logic",
            value = "All Polish observations",
            subtitle = "The app starts with a national overview instead of blank outputs.",
            icon = icon("map"),
            color = "purple",
            width = 4
          ),
          infoBox(
            title = "Search logic",
            value = "Vernacular + scientific",
            subtitle = "Users can search species with either naming system.",
            icon = icon("search"),
            color = "blue",
            width = 4
          ),
          infoBox(
            title = "Backend",
            value = "DuckDB",
            subtitle = "The app queries Poland directly instead of loading the full global CSV into memory.",
            icon = icon("database"),
            color = "green",
            width = 4
          )
        )
      ),
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            width = 12,
            title = "About this dashboard",
            status = "primary",
            solidHeader = TRUE,
            tabsetPanel(
              tabPanel(
                "General overview",
                br(),
                p("This dashboard is built to explore biodiversity observations from Poland using a larger occurrence dataset originally covering the world."),
                p("Its core job is simple: help users find a species quickly, see where it was observed, and understand when it was observed."),
                p("The DuckDB backend reduces startup pain by querying the CSVs directly and only materializing the Poland subset and the selected species outputs.")
              ),
              tabPanel(
                "What the app shows",
                br(),
                tags$ul(
                  tags$li("A searchable species selector using vernacular and scientific names."),
                  tags$li("A map of observations for the selected species, or a Poland-wide overview if no species is selected."),
                  tags$li("A timeline of observations through time."),
                  tags$li("A summary table showing either top species or selected-species records.")
                )
              ),
              tabPanel(
                "Author",
                br(),
                p("Nuno Garcia"),
                p(tags$a(href = "https://www.linkedin.com/in/nuno-garcia-97b780158/", target = "_blank", "LinkedIn")),
                p(tags$a(href = "https://orcid.org/0000-0001-7917-3286", target = "_blank", "ORCID")),
                p(tags$a(href = "https://www.researchgate.net/profile/Nuno-Garcia-4", target = "_blank", "ResearchGate"))
              )
            )
          )
        )
      )
    )
  )
)

# ============================================================
# 8) SERVER
# ============================================================
server <- function(input, output, session) {
  
  observe({
    updateSelectizeInput(
      session = session,
      inputId = "species_search",
      choices = setNames(species_index$scientific_name, species_index$display_label),
      selected = "",
      server = TRUE
    )
  })
  
  observeEvent(input$clear_species, {
    updateSelectizeInput(session, "species_search", selected = "")
  })
  
  selected_species_data <- reactive({
    req(input$species_search)
    if (is.null(input$species_search) || identical(input$species_search, "")) return(NULL)
    
    sql <- sprintf(
      "
      SELECT
        longitude,
        latitude,
        scientific_name,
        vernacular_name,
        basis_record,
        locality_name,
        dataset_name,
        event_date,
        observation_year,
        media_url
      FROM occurrence_poland_enriched
      WHERE scientific_name = %s
      ",
      sql_escape(input$species_search)
    )
    
    dbGetQuery(con_global, sql)
  })
  
  current_mode <- reactive({
    if (is.null(selected_species_data())) "default" else "selected"
  })
  
  current_map_data <- reactive({
    if (current_mode() == "default") default_map_data else selected_species_data()
  })
  
  current_timeline_data <- reactive({
    if (current_mode() == "default") {
      default_timeline
    } else {
      sql <- sprintf(
        "
        SELECT observation_year, COUNT(*) AS n_obs
        FROM occurrence_poland_enriched
        WHERE scientific_name = %s
          AND observation_year IS NOT NULL
        GROUP BY observation_year
        ORDER BY observation_year
        ",
        sql_escape(input$species_search)
      )
      dbGetQuery(con_global, sql)
    }
  })
  
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
      value = if (current_mode() == "default") "None" else input$species_search,
      subtitle = "Current species filter",
      icon = icon("filter"),
      color = "yellow"
    )
  })
  
  output$species_card <- renderUI({
    if (current_mode() == "default") {
      top_one <- default_top_species %>% slice(1)
      tags$div(
        class = "species-card",
        tags$h4("Poland overview"),
        tags$p("No species selected yet. That is intentional."),
        tags$p("The map shows all Polish observations, the timeline shows all observation years, and the table lists the most frequently observed species."),
        tags$hr(),
        tags$p(tags$b("Most observed species in Poland:")),
        tags$p(paste0(top_one$scientific_name, " (", top_one$vernacular_name, ")")),
        tags$p(paste0("Observations: ", format(top_one$n_obs, big.mark = ",")))
      )
    } else {
      dat <- selected_species_data()
      sci <- unique(dat$scientific_name)[1]
      ver <- mode_value(dat$vernacular_name)
      n_obs <- nrow(dat)
      first_year <- suppressWarnings(min(dat$observation_year, na.rm = TRUE))
      last_year  <- suppressWarnings(max(dat$observation_year, na.rm = TRUE))
      image_url  <- dplyr::first(na.omit(dat$media_url))
      
      if (!is.finite(first_year)) first_year <- NA
      if (!is.finite(last_year)) last_year <- NA
      if (is.na(ver)) ver <- "—"
      if (length(image_url) == 0 || is.na(image_url)) image_url <- "https://via.placeholder.com/500x280?text=No+image+available"
      
      tags$div(
        class = "species-card",
        tags$img(
          src = image_url,
          width = "100%",
          onerror = "this.onerror=null; this.src='https://via.placeholder.com/500x280?text=No+image+available';"
        ),
        tags$h4(sci),
        tags$p(tags$b("Vernacular name: "), ver),
        tags$p(tags$b("Observations: "), format(n_obs, big.mark = ",")),
        tags$p(tags$b("Observed period: "), ifelse(is.na(first_year), "Unknown", paste0(first_year, " to ", last_year))),
        tags$p(tags$b("Most common basis of record: "), dplyr::coalesce(mode_value(dat$basis_record), "—")),
        tags$p(tags$b("Frequent locality label: "), dplyr::coalesce(mode_value(dat$locality_name), "—"))
      )
    }
  })
  
  output$summary_table <- renderDT({
    if (current_mode() == "default") {
      datatable(
        default_top_species,
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE),
        colnames = c("Scientific name", "Vernacular name", "Observations")
      )
    } else {
      dat <- selected_species_data() %>%
        transmute(
          scientific_name,
          vernacular_name = ifelse(is.na(vernacular_name), "—", vernacular_name),
          event_date = as.character(event_date),
          year = observation_year,
          locality = ifelse(is.na(locality_name), "—", locality_name),
          basis_of_record = ifelse(is.na(basis_record), "—", basis_record),
          dataset = ifelse(is.na(dataset_name), "—", dataset_name)
        )
      
      datatable(dat, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
    }
  })
  
  output$obs_map <- renderLeaflet({
    leaflet() %>%
      addMeasure(
        position = "topright",
        primaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479"
      ) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>% # CartoDB
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>% # Esri W.I.
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%  # OpenStreetMap
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeoWorldMap") %>%  # NatGeoWorldMap
      fitBounds(
        lng1 = poland_bbox$xmin,
        lat1 = poland_bbox$ymin,
        lng2 = poland_bbox$xmax,
        lat2 = poland_bbox$ymax
      ) %>%
      addMiniMap(position = "bottomleft", toggleDisplay = TRUE) %>%
      addSearchOSM() %>%
      addLayersControl(
        baseGroups = c("CartoDB Positron", "Esri World Imagery", "OpenStreetMap","NatGeoWorldMap"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  observe({
    dat <- current_map_data()
    proxy <- leafletProxy("obs_map")
    
    proxy %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      clearControls() %>%
      clearHeatmap()
    
    if (current_mode() == "default") {
      if (isTRUE(input$show_heatmap)) {
        proxy %>% addHeatmap(
          lng = dat$longitude,
          lat = dat$latitude,
          blur = 18,
          max = 0.05,
          radius = 12,
          group = "Heatmap"
        )
      }
      
      popup_text <- paste0(
        "<b>", htmlEscape(dat$scientific_name), "</b><br/>",
        "Vernacular: ", htmlEscape(ifelse(is.na(dat$vernacular_name), "—", dat$vernacular_name)), "<br/>",
        "Year: ", ifelse(is.na(dat$observation_year), "Unknown", dat$observation_year), "<br/>",
        "Locality: ", htmlEscape(ifelse(is.na(dat$locality_name), "—", dat$locality_name))
      )
      
      if (isTRUE(input$show_clusters)) {
        proxy %>% addCircleMarkers(
          lng = dat$longitude,
          lat = dat$latitude,
          radius = input$point_radius,
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
          radius = input$point_radius,
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
      popup_text <- paste0(
        "<b>", htmlEscape(dat$scientific_name), "</b><br/>",
        "Vernacular: ", htmlEscape(ifelse(is.na(dat$vernacular_name), "—", dat$vernacular_name)), "<br/>",
        "Year: ", ifelse(is.na(dat$observation_year), "Unknown", dat$observation_year), "<br/>",
        "Locality: ", htmlEscape(ifelse(is.na(dat$locality_name), "—", dat$locality_name)), "<br/>",
        "Dataset: ", htmlEscape(ifelse(is.na(dat$dataset_name), "—", dat$dataset_name))
      )
      
      if (isTRUE(input$show_clusters)) {
        proxy %>% addCircleMarkers(
          lng = dat$longitude,
          lat = dat$latitude,
          radius = input$point_radius + 1,
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
          radius = input$point_radius + 1,
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
        labels = c(input$species_search),
        title = "Selected species",
        opacity = 0.9
      )
    }
  })
  
  output$timeline_plot <- renderPlotly({
    dat <- current_timeline_data()
    
    plot_ly(
      data = dat,
      x = ~observation_year,
      y = ~n_obs,
      type = "scatter",
      mode = "lines+markers",
      hovertemplate = "Year: %{x}<br>Observations: %{y}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Number of observations"),
        margin = list(l = 60, r = 20, b = 50, t = 20)
      )
  })
}

# ============================================================
# 9) RUN APP
# ============================================================
shinyApp(ui = ui, server = server)
