# Author: Nuno Garcia
# Date: 21/04/2026
# LinkedIn: https://www.linkedin.com/in/nuno-garcia-97b780158/
# ORCID: https://orcid.org/0000-0001-7917-3286
#
# Script purpose:
# Timeline (plot) functions for the shiny app.

# Shiny app (main) purpose:
# Dashboard to explore biodiversity observations in Poland.
# Users can search species by vernacularName and scientificName,
# inspect observations on a map, and view a temporal timeline.

# ============================================================
# FUNCTIONS -> ui (timeline plot using the species occurrences)
# ============================================================

mod_timeline_ui <- function(id) {
  ns <- NS(id)
  
  plotlyOutput(ns("timeline_plot"), height = 220)
}

mod_timeline_server <- function(id, current_timeline_data) {
  moduleServer(id, function(input, output, session) {
    
    output$timeline_plot <- renderPlotly({
      dat <- current_timeline_data()
      req(!is.null(dat), nrow(dat) > 0)
      
      # Defensive cleaning
      dat <- dat %>%
        dplyr::filter(!is.na(observation_year), !is.na(n_obs)) %>%
        dplyr::mutate(
          observation_year = as.numeric(observation_year),
          n_obs = as.numeric(n_obs)
        ) %>%
        dplyr::arrange(observation_year)
      
      req(nrow(dat) > 0)
      
      # Add label text for richer hover
      dat <- dat %>%
        dplyr::mutate(
          hover_text = paste0(
            "<b>Year:</b> ", observation_year,
            "<br><b>Observations:</b> ", scales::comma(n_obs)
          )
        )
      
      ymax <- max(dat$n_obs, na.rm = TRUE)
      
      plot_ly(
        data = dat,
        x = ~observation_year,
        y = ~n_obs,
        type = "scatter",
        mode = "lines+markers",
        text = ~hover_text,
        hovertemplate = "%{text}<extra></extra>",
        line = list(
          width = 3,
          shape = "spline",
          smoothing = 0.6
        ),
        marker = list(
          size = 7,
          line = list(width = 1, color = "white")
        ),
        fill = "tozeroy",
        fillcolor = "rgba(66, 135, 245, 0.15)"
      ) %>%
        layout(
          title = list(
            x = 0.02,
            xanchor = "left",
            font = list(size = 18)
          ),
          xaxis = list(
            title = "Year",
            tickmode = "linear",
            dtick = ifelse(length(unique(dat$observation_year)) > 15, 2, 1),
            showgrid = FALSE,
            zeroline = FALSE
          ),
          yaxis = list(
            title = "Number of observations",
            rangemode = "tozero",
            gridcolor = "rgba(0,0,0,0.08)",
            zeroline = FALSE
          ),
          hovermode = "x unified",
          margin = list(l = 70, r = 20, b = 60, t = 50),
          plot_bgcolor = "white",
          paper_bgcolor = "white",
          showlegend = FALSE
        ) %>%
        config(
          displayModeBar = FALSE,
          responsive = TRUE
        )
    })
  })
}