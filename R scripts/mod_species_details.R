
# Author: Nuno Garcia
# Date: 21/04/2026
# LinkedIn: https://www.linkedin.com/in/nuno-garcia-97b780158/
# ORCID: https://orcid.org/0000-0001-7917-3286
#
# Script purpose:
# Species details (ui) functions for the shiny app.

# Shiny app (main) purpose:
# Dashboard to explore biodiversity observations in Poland.
# Users can search species by vernacularName and scientificName,
# inspect observations on a map, and view a temporal timeline.

# ============================================================
# FUNCTIONS -> ui (map w/ species ocurrences)
# ============================================================
#
# ============================================================
# UI -> species card only
# ============================================================
mod_species_card_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    htmlOutput(ns("species_card"))
  )
}

# ============================================================
# UI -> species table only
# ============================================================
mod_species_table_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    DTOutput(ns("summary_table"))
  )
}

# ============================================================
# SERVER
# ============================================================
mod_species_details_server <- function(
    id,
    current_mode,
    selected_species_data,
    default_top_species
) {
  moduleServer(id, function(input, output, session) {
    
    output$species_card <- renderUI({
      if (current_mode() == "default") {
        top_one <- default_top_species %>% dplyr::slice(1)
        build_species_card_default(top_one)
      } else {
        dat <- selected_species_data()
        req(!is.null(dat), nrow(dat) > 0)
        
        sci <- unique(dat$scientific_name)[1]
        ver <- mode_value(dat$vernacular_name)
        n_obs <- nrow(dat)
        first_year <- suppressWarnings(min(dat$observation_year, na.rm = TRUE))
        last_year <- suppressWarnings(max(dat$observation_year, na.rm = TRUE))
        image_url <- dplyr::first(stats::na.omit(dat$media_url))
        
        if (!is.finite(first_year)) first_year <- NA
        if (!is.finite(last_year)) last_year <- NA
        if (is.na(ver) || ver == "") ver <- "—"
        if (length(image_url) == 0 || is.na(image_url) || image_url == "") {
          image_url <- "https://placehold.co/500x350?text=No+image"
        }
        
        tags$div(
          class = "species-card",
          style = "
        display:flex;
        gap:18px;
        align-items:flex-start;
        background:#ffffff;
        border-radius:16px;
        padding:16px;
      ",
          
          tags$div(
            style = "
          flex:0 0 220px;
          max-width:220px;
        ",
            br(),
            tags$img(
              src = image_url,
              style = "
            width:100%;
            height:160px;
            object-fit:cover;
            border-radius:12px;
            display:block;
            border:1px solid #e5e7eb;
          ",
              onerror = "this.onerror=null;this.src='https://placehold.co/500x350?text=No+image';"
            )
          ),
          
          tags$div(
            style = "
          flex:1;
          min-width:0;
        ",
            tags$h4(
              sci,
              style = "
            margin-top:0;
            margin-bottom:10px;
            font-weight:700;
            color:#111827;
          "
            ),
            tags$p(tags$b("Vernacular name: "), ver),
            tags$p(tags$b("Observations: "), format(n_obs, big.mark = ",")),
            tags$p(
              tags$b("Observed period: "),
              ifelse(is.na(first_year), "Unknown", paste0(first_year, " to ", last_year))
            ),
            tags$p(
              tags$b("Most common basis of record: "),
              dplyr::coalesce(mode_value(dat$basis_record), "—")
            ),
            tags$p(
              tags$b("Frequent locality label: "),
              dplyr::coalesce(mode_value(dat$locality_name), "—")
            )
          )
        )
      }
    })
    
    output$summary_table <- renderDT({
      if (current_mode() == "default") {
        dat <- default_top_species
        
        datatable(
          dat,
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            dom = "Bfrtip",
            pageLength = 5,
            scrollX = TRUE,
            buttons = list(
              list(extend = "csv", title = paste0("species_data_", Sys.Date())),
              list(extend = "excel", title = paste0("species_data_", Sys.Date())),
              list(extend = "pdf", title = paste0("species_data_", Sys.Date()))
            )
          ),
          colnames = c("Scientific name", "Vernacular name", "Observations")
        )
      } else {
        dat <- selected_species_data() %>%
          transmute(
            scientific_name = scientific_name,
            vernacular_name = ifelse(is.na(vernacular_name) | vernacular_name == "", "—", vernacular_name),
            event_date = as.character(event_date),
            year = observation_year,
            locality = ifelse(is.na(locality_name) | locality_name == "", "—", locality_name),
            basis_of_record = ifelse(is.na(basis_record) | basis_record == "", "—", basis_record),
            dataset = ifelse(is.na(dataset_name) | dataset_name == "", "GBIF", dataset_name)
          )
        
        datatable(
          dat,
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            dom = "Bfrtip",
            pageLength = 8,
            scrollX = TRUE,
            buttons = list(
              list(extend = "csv", title = paste0("species_data_", Sys.Date())),
              list(extend = "excel", title = paste0("species_data_", Sys.Date())),
              list(extend = "pdf", title = paste0("species_data_", Sys.Date()))
            )
          ),
          colnames = c(
            "Scientific name",
            "Vernacular name",
            "Event date",
            "Year",
            "Locality",
            "Basis of record",
            "Dataset"
          )
        )
      }
    })
  })
}