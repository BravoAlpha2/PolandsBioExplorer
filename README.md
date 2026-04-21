## Poland´s BioExplorer
Interactive Shiny dashboard to explore biodiversity observations in Poland. Users can search species, filter records, visualize occurrences on a map, analyze trends over time, and inspect detailed records and traits. Powered by DuckDB for fast, efficient data querying.


[<img width="1882" height="908" alt="image" src="https://github.com/user-attachments/assets/8b4d7146-d1f3-4058-8a9e-a09ec526ac07" />](https://nunogarcia.shinyapps.io/shiny_-_appsilon_interview/)


---

## Caractheristics

The app enables users to explore biodiversity observations through an integrated workflow that begins with fast species search using scientific or vernacular names and extends to spatial visualization on an interactive map. Observations are displayed with support for clustering and heatmaps, allowing rapid identification of distribution patterns, while a dynamic timeline reveals how records evolve over time based on the current selection and applied filters.

Beyond spatial and temporal exploration, the app delivers detailed species insights through summary cards, a complete records table with export options, and a dedicated traits module with ecological information. Users can refine analyses using filters such as year range, media availability, observation type, and map display options. The system is built on a Poland-focused subset of GBIF ([Global Biodiversity Information Facility](https://www.gbif.org/)) data, providing a meaningful default view, a map-first analytical workflow, consistent filtering across modules, and a modular architecture designed for scalability.


## Technical Architecture

The backend relies on DuckDB for fast, in-memory querying of large datasets, while the frontend is built using Shiny and shinydashboard to deliver a structured and interactive interface. Visualization components are handled through Leaflet for spatial data, Plotly for temporal trends, and DataTables for tabular exploration. Data handling is optimized to query only the required subsets, avoiding full dataset loading and ensuring responsiveness even with large inputs.


## PPerformance Optimization

Performance is driven by DuckDB’s ability to efficiently process large CSV files, combined with a lazy loading strategy that restricts operations to the Poland subset and selected species. The reactive filtering pipeline ensures that all components update consistently without redundant computations, while map interactions are optimized to avoid full re-rendering, resulting in smoother user experience and faster response times.


## PInfrastructure

The application is designed for flexible deployment using containerized environments such as Docker, typically paired with Shiny Server or similar solutions. Due to the size of the original GBIF datasets, pre-processing is required to reduce data volume before deployment, ensuring that the application remains performant and deployable within infrastructure constraints.


## Project Structure

app.R # Main app entry point

- R scripts/
  - helpers.R
  - data_access.R
  - mod_species_search.R
  - mod_value_boxes.R
  - mod_observation_map.R
  - mod_timeline.R
  - mod_species_details.R
  - mod_traits.R
  - mod_about.R

- Data/
  - occurence_poland.csv
  - multimedia_poland.csv

---

# Contacts:
For more information, please contact the main authors: 

Nuno Garcia, Junior Researcher, Vrije Universiteit Amsterdam

  Contact(s):
- Email: nunogarcia8@gmail.com;
- ORCID: https://orcid.org/0000-0001-7917-3286;
-  ResearchGate: https://www.researchgate.net/profile/Nuno-Garcia-4;
- LinkedIn: https://www.linkedin.com/in/nuno-garcia-97b780158/;

