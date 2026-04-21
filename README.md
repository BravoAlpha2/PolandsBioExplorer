# PolandsBioExplorer
Interactive Shiny dashboard to explore biodiversity observations in Poland. Users can search species, filter records, visualize occurrences on a map, analyze trends over time, and inspect detailed records and traits. Powered by DuckDB for fast, efficient data querying.

---

<img width="1882" height="908" alt="image" src="https://github.com/user-attachments/assets/8b4d7146-d1f3-4058-8a9e-a09ec526ac07" />

---

## Features

### Species Search
Search species by scientific or vernacular name with fast lookup.

### Map Visualization
Interactive map showing species occurrences (clusters / heatmap supported).

### Observation Timeline
Temporal trends of observations, dynamically updated by filters.

### Species Details
Summary card with key metadata and media.

### Records Table
Full observation table with export options (CSV, Excel, PDF).

### Traits Module
Dedicated section for species-level traits and ecological information.

### Filters
- Year range  
- Media availability  
- Observation type  
- Map display options  

---

## Business Requirements

- Poland-focused dataset (filtered from global GBIF data)  
- Meaningful default state (not empty UI)  
- Map-first analytical workflow  
- Consistent filtering across modules  
- Modular architecture for scalability  

---

## Technical Architecture

- **Backend:** DuckDB (in-memory analytical database)  
- **Frontend:** Shiny + shinydashboard  

### Visualization
- Leaflet (maps)  
- Plotly (timeline)  
- DataTables (records)  

### Data Handling
Efficient querying without loading full datasets into memory  

---

## Performance Optimization

- DuckDB query engine for large CSVs  
- Lazy data loading (only Poland subset + selected species)  
- Reactive filtering pipeline  
- Efficient map updates (no full re-render)  

---

## Infrastructure

Designed for deployment via:
- Docker + Shiny Server 

Dataset pre-processing required due to large GBIF files  

---

## Project Structure

app.R # Main app entry point
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
occurence_poland.csv
multimedia_poland.csv


# Contacts:
For more information, please contact the main authors: 

Nuno Garcia, Junior Researcher, Vrije Universiteit Amsterdam

  Contact(s):
- Email: nunogarcia8@gmail.com;
- ORCID: https://orcid.org/0000-0001-7917-3286;
-  ResearchGate: https://www.researchgate.net/profile/Nuno-Garcia-4;
- LinkedIn: https://www.linkedin.com/in/nuno-garcia-97b780158/;

