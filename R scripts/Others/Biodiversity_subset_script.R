# Author: Nuno Garcia
# Date: 21/04/2026
# Full UI redesign - sidebar-first version
#
# Main principles:
# - Subset the biodiversity data

library(DBI)
library(duckdb)

con <- dbConnect(duckdb::duckdb())

# Load only Poland occurrence references once
dbExecute(con, "
  CREATE TEMP TABLE occ_poland AS
  SELECT *
  FROM read_csv_auto('C:/Users/nunog/OneDrive/Desktop/PolandsBioexplorer/Data/biodiversity-data/occurence.csv')
  WHERE countryCode = 'PL'
")

# Export Poland occurrence
dbExecute(con, "
  COPY occ_poland
  TO 'C:/Users/nunog/OneDrive/Desktop/PolandsBioexplorer/Data/biodiversity-data/occurence_poland.csv'
  (HEADER, DELIMITER ',')
")

dbExecute(con, "
  COPY (
    SELECT m.*
    FROM read_csv_auto('C:/Users/nunog/OneDrive/Desktop/PolandsBioexplorer/Data/biodiversity-data/multimedia.csv') AS m
    INNER JOIN occ_poland AS o
      ON m.CoreId = CAST(o.id AS VARCHAR)
  )
  TO 'C:/Users/nunog/OneDrive/Desktop/PolandsBioexplorer/Data/biodiversity-data/multimedia_poland.csv'
  (HEADER, DELIMITER ',')
")

dbDisconnect(con, shutdown = TRUE)
