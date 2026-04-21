# PolandsBioExplorer
Interactive Shiny dashboard to explore biodiversity observations in Poland. Users can search species, filter records, visualize occurrences on a map, analyze trends over time, and inspect detailed records and traits. Powered by DuckDB for fast, efficient data querying.

![image](ttps://github.com/user-attachments/assets/39f1d8ef-e1d2-4b41-a8b9-66d124061b47)

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
- Docker + Shiny Server / ShinyProxy  
- Cloud environments (AWS / Azure / GCP)  

Dataset pre-processing required due to large GBIF files  

---

## Project Structure
