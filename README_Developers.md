# PolandsBioExplorer

## Overview
PolandsBioExplorer is an interactive Shiny dashboard designed to explore biodiversity observations in Poland. The application enables users to search for species, visualize spatial distributions, analyze temporal trends, and inspect detailed ecological information using data from GBIF.

## Key Features
- Species search by scientific and vernacular names
- Interactive map with clustering and heatmap options
- Dynamic observation timeline
- Species detail cards with media
- Full records table with export options (CSV, Excel, PDF)
- Traits module with ecological information
- Advanced filters (year range, media availability, observation type)

## Technical Architecture
- Backend: DuckDB (in-memory analytical database)
- Frontend: Shiny + shinydashboard
- Visualization: Leaflet, Plotly, DataTables
- Data handling: Efficient querying without loading full datasets into memory

## Performance Optimization
- DuckDB for large CSV processing
- Pre-filtered Poland dataset
- Reactive filtering pipeline
- Efficient map updates (no full re-render)

## Project Structure
```
app.R

R scripts/
  helpers.R
  data_access.R
  mod_species_search.R
  mod_value_boxes.R
  mod_observation_map.R
  mod_timeline.R
  mod_species_details.R
  mod_traits.R
  mod_about.R

Data/
  biodiversity-data/
    occurence_poland.csv
    multimedia_poland.csv
```

## Running the App
```r
shiny::runApp()
```

## Testing
Unit tests are implemented using `testthat`.

Run tests with:
```r
testthat::test_dir("tests/testthat")
```

## Deployment
The application is designed for deployment via:
- shinyapps.io
- Docker + Shiny Server / ShinyProxy

Note: Large raw GBIF datasets must be pre-filtered before deployment.

## Author
Nuno Garcia  
Geospatial Data Scientist  

- LinkedIn: https://www.linkedin.com/in/nuno-garcia-97b780158/  
- ORCID: https://orcid.org/0000-0001-7917-3286
