
# Author: Nuno Garcia
# Date: 21/04/2026
# LinkedIn: https://www.linkedin.com/in/nuno-garcia-97b780158/
# ORCID: https://orcid.org/0000-0001-7917-3286
#
# Script purpose:
# Script´s helpers.

# Shiny app (main) purpose:
# Dashboard to explore biodiversity observations in Poland.
# Users can search species by vernacularName and scientificName,
# inspect observations on a map, and view a temporal timeline.

# ============================================================
# HELPERS
# ============================================================
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

clean_label <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | stringr::str_trim(x) == "", NA_character_, stringr::str_squish(x))
}

mode_value <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

img_placeholder <- "https://via.placeholder.com/500x280?text=No+image+available"

# Or
# # From wikipedia -> no image placeholder
# img_placeholder <- "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ac/No_image_available.svg/1024px-No_image_available.svg.png"

build_species_card_default <- function(top_one) {
  tags$div(
    class = "species-card",
    tags$h4("Poland overview"),
    tags$p("No species selected yet."),
    tags$p("The map shows all Polish observations, the timeline shows all observation years, and the table lists the most frequently observed species."),
    tags$hr(),
    tags$p(tags$b("Most observed species in Poland:")),
    tags$p(paste0(top_one$scientific_name, " (", top_one$vernacular_name, ")")),
    tags$p(paste0("Observations: ", format(top_one$n_obs, big.mark = ",")))
  )
}

build_species_card_selected <- function(dat) {
  sci <- unique(dat$scientific_name)[1]
  ver <- mode_value(dat$vernacular_name)
  n_obs <- nrow(dat)
  first_year <- suppressWarnings(min(dat$observation_year, na.rm = TRUE))
  last_year <- suppressWarnings(max(dat$observation_year, na.rm = TRUE))
  image_url <- dplyr::first(stats::na.omit(dat$media_url))
  
  if (!is.finite(first_year)) first_year <- NA
  if (!is.finite(last_year)) last_year <- NA
  if (is.na(ver)) ver <- "—"
  if (length(image_url) == 0 || is.na(image_url)) image_url <- img_placeholder
  
  tags$div(
    class = "species-card",
    tags$img(
      src = image_url,
      width = "100%",
      onerror = sprintf("this.onerror=null; this.src='%s';", img_placeholder)
    ),
    tags$h4(sci),
    tags$p(tags$b("Vernacular name: "), ver),
    tags$p(tags$b("Observations: "), format(n_obs, big.mark = ",")),
    tags$p(tags$b("Observed period: "), ifelse(is.na(first_year), "Unknown", paste0(first_year, " to ", last_year))),
    tags$p(tags$b("Most common basis of record: "), dplyr::coalesce(mode_value(dat$basis_record), "—")),
    tags$p(tags$b("Frequent locality label: "), dplyr::coalesce(mode_value(dat$locality_name), "—"))
  )
}

build_default_popup <- function(dat) {
  paste0(
    "<b>", htmltools::htmlEscape(dat$scientific_name), "</b><br/>",
    "Vernacular: ", htmltools::htmlEscape(ifelse(is.na(dat$vernacular_name), "—", dat$vernacular_name)), "<br/>",
    "Year: ", ifelse(is.na(dat$observation_year), "Unknown", dat$observation_year), "<br/>",
    "Locality: ", htmltools::htmlEscape(ifelse(is.na(dat$locality_name), "—", dat$locality_name))
  )
}

build_selected_popup <- function(dat) {
  paste0(
    "<b>", htmltools::htmlEscape(dat$scientific_name), "</b><br/>",
    "Vernacular: ", htmltools::htmlEscape(ifelse(is.na(dat$vernacular_name), "—", dat$vernacular_name)), "<br/>",
    "Year: ", ifelse(is.na(dat$observation_year), "Unknown", dat$observation_year), "<br/>",
    "Locality: ", htmltools::htmlEscape(ifelse(is.na(dat$locality_name), "—", dat$locality_name)), "<br/>",
    "Dataset: ", htmltools::htmlEscape(ifelse(is.na(dat$dataset_name), "—", dat$dataset_name))
  )
}


# ============================================================
# TESTABLE DATA LOGIC HELPERS
# ============================================================

filter_observation_data <- function(dat,
                                    year_range = NULL,
                                    only_with_media = FALSE) {
  if (is.null(dat) || nrow(dat) == 0) {
    return(dat)
  }
  
  if (isTRUE(only_with_media) && "media_url" %in% names(dat)) {
    dat <- dat %>%
      dplyr::filter(!is.na(media_url) & media_url != "")
  }
  
  if (!is.null(year_range) &&
      length(year_range) == 2 &&
      "observation_year" %in% names(dat)) {
    dat <- dat %>%
      dplyr::filter(
        !is.na(observation_year),
        observation_year >= year_range[1],
        observation_year <= year_range[2]
      )
  }
  
  dat
}

build_timeline_data <- function(dat) {
  if (is.null(dat) || nrow(dat) == 0 || !"observation_year" %in% names(dat)) {
    return(data.frame(
      observation_year = numeric(0),
      n_obs = numeric(0)
    ))
  }
  
  dat <- dat %>%
    dplyr::filter(!is.na(observation_year)) %>%
    dplyr::count(observation_year, name = "n_obs") %>%
    dplyr::arrange(observation_year)
  
  if (nrow(dat) == 0) {
    return(data.frame(
      observation_year = numeric(0),
      n_obs = numeric(0)
    ))
  }
  
  dat
}

build_selection_summary <- function(dat, selected_species = NULL, default_obs_count = NULL) {
  if (is.null(selected_species)) {
    return(list(
      title = "National overview",
      subtitle = "No species selected",
      obs_n = default_obs_count,
      years = NULL
    ))
  }
  
  if (is.null(dat) || nrow(dat) == 0) {
    return(list(
      title = as.character(selected_species),
      subtitle = "Selected species",
      obs_n = 0,
      years = NULL
    ))
  }
  
  obs_n <- nrow(dat)
  
  year_values <- NULL
  possible_year_cols <- c("observation_year", "year", "event_year")
  year_col_found <- possible_year_cols[possible_year_cols %in% names(dat)]
  
  if (length(year_col_found) > 0) {
    year_values <- suppressWarnings(as.numeric(dat[[year_col_found[1]]]))
    year_values <- year_values[!is.na(year_values)]
  } else if ("event_date" %in% names(dat)) {
    year_values <- suppressWarnings(lubridate::year(as.Date(dat$event_date)))
    year_values <- year_values[!is.na(year_values)]
  }
  
  year_label <- NULL
  if (!is.null(year_values) && length(year_values) > 0) {
    year_label <- paste0(min(year_values), " - ", max(year_values))
  }
  
  list(
    title = as.character(selected_species),
    subtitle = "Selected species",
    obs_n = obs_n,
    years = year_label
  )
}
