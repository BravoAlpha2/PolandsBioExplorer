# Author: Nuno Garcia
# Date: 21/04/2026
# LinkedIn: https://www.linkedin.com/in/nuno-garcia-97b780158/
# ORCID: https://orcid.org/0000-0001-7917-3286
#
# Script purpose:
# Data access utilities for the Shiny app.

# ============================================================
# DATA ACCESS (DuckDB / SQL)
# ============================================================

get_poland_bbox <- function() {
  list(
    xmin = 14.07,
    xmax = 24.20,
    ymin = 49.00,
    ymax = 54.90
  )
}

sql_escape <- function(con, x) {
  DBI::dbQuoteString(con, x) |> as.character()
}

# Case-insensitive column picker that returns the original column name
pick_col <- function(candidates, cols) {
  if (length(cols) == 0) return(NA_character_)
  
  cols_lc <- tolower(trimws(cols))
  cand_lc <- tolower(trimws(candidates))
  
  hit_idx <- match(cand_lc, cols_lc)
  hit_idx <- hit_idx[!is.na(hit_idx)]
  
  if (length(hit_idx) == 0) return(NA_character_)
  cols[hit_idx[1]]
}

normalize_species_name <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x <- tolower(x)
  x[x == ""] <- NA_character_
  x
}

sql_col_or_null <- function(col, alias, cast = "VARCHAR", trim = FALSE, null_if_empty = FALSE) {
  if (is.na(col)) {
    return(sprintf("CAST(NULL AS %s) AS %s", cast, alias))
  }
  
  expr <- sprintf('"%s"', col)
  if (trim) expr <- sprintf("TRIM(%s)", expr)
  if (null_if_empty) expr <- sprintf("NULLIF(%s, '')", expr)
  if (!is.null(cast)) expr <- sprintf("CAST(%s AS %s)", expr, cast)
  
  sprintf("%s AS %s", expr, alias)
}

sql_try_cast_or_null <- function(col, alias, cast_type = "DOUBLE") {
  if (is.na(col)) {
    return(sprintf("CAST(NULL AS %s) AS %s", cast_type, alias))
  }
  
  sprintf('TRY_CAST("%s" AS %s) AS %s', col, cast_type, alias)
}

build_occurrence_poland <- function(con, path_occurrence, poland_bbox) {
  occ_cols <- DBI::dbGetQuery(
    con,
    sprintf(
      "DESCRIBE SELECT * FROM read_csv_auto('%s', header = TRUE, all_varchar = TRUE)",
      gsub("\\\\", "/", path_occurrence)
    )
  )$column_name
  
  occ_col_lon        <- pick_col(c("decimalLongitude", "longitudeDecimal", "longitude", "lon"), occ_cols)
  occ_col_lat        <- pick_col(c("decimalLatitude", "latitudeDecimal", "latitude", "lat"), occ_cols)
  occ_col_scientific <- pick_col(c("scientificName", "scientific_name"), occ_cols)
  occ_col_vernacular <- pick_col(c("vernacularName", "vernacular_name"), occ_cols)
  occ_col_country    <- pick_col(c("country", "countryCode", "country_code", "countryName"), occ_cols)
  occ_col_basis      <- pick_col(c("basisOfRecord", "basis_of_record"), occ_cols)
  
  # IMPORTANT: in this file, `id` matches multimedia CoreId
  occ_col_occid <- pick_col(c("id"), occ_cols)
  occ_col_id    <- NA_character_
  
  occ_col_dataset    <- pick_col(c("datasetName", "dataset_name"), occ_cols)
  occ_col_locality   <- pick_col(c("locality", "municipality", "stateProvince", "state_province"), occ_cols)
  occ_col_eventdate  <- pick_col(c("eventDate", "event_date"), occ_cols)
  occ_col_year       <- pick_col(c("year"), occ_cols)
  occ_col_month      <- pick_col(c("month"), occ_cols)
  occ_col_day        <- pick_col(c("day"), occ_cols)
  
  if (is.na(occ_col_lon) || is.na(occ_col_lat) || is.na(occ_col_scientific)) {
    stop("Required occurrence columns were not found: longitude, latitude, or scientific name.")
  }
  
  occurrence_id_expr <- if (!is.na(occ_col_occid)) {
    sprintf(
      "NULLIF(TRIM(CAST(\"%s\" AS VARCHAR)), '') AS occurrence_id",
      occ_col_occid
    )
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
    sql_col_or_null(occ_col_eventdate, "event_date_raw", cast = "VARCHAR", trim = TRUE, null_if_empty = TRUE),
    sql_try_cast_or_null(occ_col_year, "year_value", "INTEGER"),
    sql_try_cast_or_null(occ_col_month, "month_value", "INTEGER"),
    sql_try_cast_or_null(occ_col_day, "day_value", "INTEGER"),
    gsub("\\\\", "/", path_occurrence),
    poland_bbox$xmin, poland_bbox$xmax, poland_bbox$ymin, poland_bbox$ymax
  )
  
  DBI::dbExecute(con, occ_sql)
  
  n_poland <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM occurrence_poland")$n[[1]]
  if (is.na(n_poland) || n_poland == 0) {
    stop("No observations remained after Poland filtering. Check your country field and coordinates.")
  }
}

build_media_lookup <- function(con, path_multimedia) {
  mult_cols <- DBI::dbGetQuery(
    con,
    sprintf(
      "DESCRIBE SELECT * FROM read_csv_auto('%s', header = TRUE, all_varchar = TRUE)",
      gsub("\\\\", "/", path_multimedia)
    )
  )$column_name
  
  mult_col_occid <- pick_col(
    c("CoreId", "coreid", "occurrenceID", "occurrence_id", "gbifID", "gbifId"),
    mult_cols
  )
  
  mult_col_id <- pick_col(c("id"), mult_cols)
  
  mult_col_media <- pick_col(
    c("accessURI", "access_uri", "Identifier", "identifier", "references"),
    mult_cols
  )
  
  mult_col_format <- pick_col(c("format", "Format"), mult_cols)
  
  mult_occurrence_id_expr <- if (!is.na(mult_col_occid) && !is.na(mult_col_id) && mult_col_occid != mult_col_id) {
    sprintf(
      "NULLIF(TRIM(CAST(COALESCE(\"%s\", \"%s\") AS VARCHAR)), '') AS occurrence_id",
      mult_col_occid, mult_col_id
    )
  } else if (!is.na(mult_col_occid)) {
    sprintf(
      "NULLIF(TRIM(CAST(\"%s\" AS VARCHAR)), '') AS occurrence_id",
      mult_col_occid
    )
  } else if (!is.na(mult_col_id)) {
    sprintf(
      "NULLIF(TRIM(CAST(\"%s\" AS VARCHAR)), '') AS occurrence_id",
      mult_col_id
    )
  } else {
    "CAST(NULL AS VARCHAR) AS occurrence_id"
  }
  
  mult_media_expr <- if (!is.na(mult_col_media)) {
    sprintf(
      "NULLIF(TRIM(CAST(\"%s\" AS VARCHAR)), '') AS media_url",
      mult_col_media
    )
  } else {
    "CAST(NULL AS VARCHAR) AS media_url"
  }
  
  mult_format_expr <- if (!is.na(mult_col_format)) {
    sprintf(
      "NULLIF(TRIM(LOWER(CAST(\"%s\" AS VARCHAR))), '') AS media_format",
      mult_col_format
    )
  } else {
    "CAST(NULL AS VARCHAR) AS media_format"
  }
  
  media_sql <- sprintf(
    "
    CREATE OR REPLACE TABLE media_lookup AS
    SELECT occurrence_id, media_url
    FROM (
      SELECT
        %s,
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
      AND LOWER(media_url) LIKE 'http%%'
      AND (
        media_format LIKE 'image/%%'
        OR LOWER(media_url) LIKE '%%.jpg%%'
        OR LOWER(media_url) LIKE '%%.jpeg%%'
        OR LOWER(media_url) LIKE '%%.png%%'
        OR LOWER(media_url) LIKE '%%.webp%%'
      )
      AND rn = 1
    ",
    mult_occurrence_id_expr,
    mult_media_expr,
    mult_format_expr,
    gsub("\\\\", "/", path_multimedia)
  )
  
  DBI::dbExecute(con, media_sql)
  
  DBI::dbExecute(con, "
    CREATE OR REPLACE VIEW occurrence_poland_enriched AS
    SELECT o.*, m.media_url
    FROM occurrence_poland o
    LEFT JOIN media_lookup m
      ON LOWER(TRIM(o.occurrence_id)) = LOWER(TRIM(m.occurrence_id))
  ")
}

build_clean_occurrence_view <- function(con) {
  DBI::dbExecute(con, "
    CREATE OR REPLACE VIEW occurrence_poland_clean AS
    SELECT
      longitude,
      latitude,
      TRIM(REGEXP_REPLACE(LOWER(scientific_name), '\\s+', ' ')) AS scientific_name_key,
      TRIM(REGEXP_REPLACE(scientific_name, '\\s+', ' ')) AS scientific_name,
      NULLIF(TRIM(REGEXP_REPLACE(vernacular_name, '\\s+', ' ')), '') AS vernacular_name,
      basis_record,
      occurrence_id,
      dataset_name,
      locality_name,
      event_date,
      observation_year,
      media_url
    FROM occurrence_poland_enriched
    WHERE scientific_name IS NOT NULL
      AND TRIM(scientific_name) <> ''
  ")
}

load_app_data <- function(con, path_occurrence, path_multimedia) {
  poland_bbox <- get_poland_bbox()
  
  build_occurrence_poland(con, path_occurrence, poland_bbox)
  build_media_lookup(con, path_multimedia)
  build_clean_occurrence_view(con)
  
  species_index <- DBI::dbGetQuery(con, "
    SELECT
      scientific_name_key,
      scientific_name,
      COALESCE(vernacular_name, '—') AS vernacular_name,
      CASE
        WHEN vernacular_name IS NOT NULL AND vernacular_name <> ''
          THEN scientific_name || ' (' || vernacular_name || ')'
        ELSE scientific_name
      END AS display_label
    FROM (
      SELECT
        scientific_name_key,
        scientific_name,
        vernacular_name,
        ROW_NUMBER() OVER (
          PARTITION BY scientific_name_key
          ORDER BY
            CASE WHEN vernacular_name IS NOT NULL AND vernacular_name <> '' THEN 0 ELSE 1 END,
            scientific_name,
            vernacular_name
        ) AS rn
      FROM occurrence_poland_clean
      WHERE scientific_name_key IS NOT NULL
    ) x
    WHERE rn = 1
    ORDER BY display_label
  ")
  
  default_timeline <- DBI::dbGetQuery(con, "
    SELECT observation_year, COUNT(*) AS n_obs
    FROM occurrence_poland_clean
    WHERE observation_year IS NOT NULL
    GROUP BY observation_year
    ORDER BY observation_year
  ")
  
  default_top_species <- DBI::dbGetQuery(con, "
    SELECT
      scientific_name,
      COALESCE(vernacular_name, '—') AS vernacular_name,
      n_obs
    FROM (
      SELECT
        scientific_name_key,
        scientific_name,
        vernacular_name,
        COUNT(*) AS n_obs,
        ROW_NUMBER() OVER (
          PARTITION BY scientific_name_key
          ORDER BY
            CASE WHEN vernacular_name IS NOT NULL AND vernacular_name <> '' THEN 0 ELSE 1 END,
            scientific_name,
            vernacular_name
        ) AS rn
      FROM occurrence_poland_clean
      GROUP BY scientific_name_key, scientific_name, vernacular_name
    ) x
    WHERE rn = 1
    ORDER BY n_obs DESC, scientific_name
    LIMIT 15
  ")
  
  default_obs_count <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n_obs FROM occurrence_poland_clean"
  )$n_obs[[1]]
  
  default_species_count <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(DISTINCT scientific_name_key) AS n_species FROM occurrence_poland_clean"
  )$n_species[[1]]
  
  default_map_data <- DBI::dbGetQuery(con, "
    SELECT
      longitude,
      latitude,
      scientific_name_key,
      scientific_name,
      vernacular_name,
      observation_year,
      locality_name,
      dataset_name,
      media_url
    FROM occurrence_poland_clean
  ")
  
  list(
    con = con,
    poland_bbox = poland_bbox,
    species_index = species_index,
    default_timeline = default_timeline,
    default_top_species = default_top_species,
    default_obs_count = default_obs_count,
    default_species_count = default_species_count,
    default_map_data = default_map_data
  )
}

get_selected_species_data <- function(con, scientific_name_key) {
  if (is.null(scientific_name_key) || identical(scientific_name_key, "")) return(NULL)
  
  sql <- sprintf(
    "
    SELECT
      longitude,
      latitude,
      scientific_name_key,
      scientific_name,
      vernacular_name,
      basis_record,
      locality_name,
      dataset_name,
      event_date,
      observation_year,
      media_url
    FROM occurrence_poland_clean
    WHERE scientific_name_key = %s
    ",
    sql_escape(con, scientific_name_key)
  )
  
  DBI::dbGetQuery(con, sql)
}

get_selected_timeline_data <- function(con, scientific_name_key) {
  if (is.null(scientific_name_key) || identical(scientific_name_key, "")) return(NULL)
  
  sql <- sprintf(
    "
    SELECT observation_year, COUNT(*) AS n_obs
    FROM occurrence_poland_clean
    WHERE scientific_name_key = %s
      AND observation_year IS NOT NULL
    GROUP BY observation_year
    ORDER BY observation_year
    ",
    sql_escape(con, scientific_name_key)
  )
  
  DBI::dbGetQuery(con, sql)
}

get_selected_species_traits <- function(con, scientific_name_key) {
  if (is.null(scientific_name_key) || identical(scientific_name_key, "")) return(NULL)
  
  sql <- sprintf(
    "
    WITH base AS (
      SELECT
        scientific_name_key,
        scientific_name,
        vernacular_name,
        basis_record,
        locality_name,
        dataset_name,
        event_date,
        observation_year,
        media_url
      FROM occurrence_poland_clean
      WHERE scientific_name_key = %s
    ),
    
    names_cte AS (
      SELECT
        scientific_name,
        vernacular_name,
        ROW_NUMBER() OVER (
          ORDER BY
            CASE WHEN vernacular_name IS NOT NULL AND vernacular_name <> '' THEN 0 ELSE 1 END,
            scientific_name,
            vernacular_name
        ) AS rn
      FROM (
        SELECT DISTINCT scientific_name, vernacular_name
        FROM base
      ) x
    ),
    
    basis_cte AS (
      SELECT STRING_AGG(basis_record, ', ' ORDER BY basis_record) AS basis_record_summary
      FROM (
        SELECT DISTINCT basis_record
        FROM base
        WHERE basis_record IS NOT NULL AND TRIM(basis_record) <> ''
      ) x
    ),
    
    dataset_cte AS (
      SELECT STRING_AGG(dataset_name, ', ' ORDER BY dataset_name) AS dataset_summary
      FROM (
        SELECT DISTINCT dataset_name
        FROM base
        WHERE dataset_name IS NOT NULL AND TRIM(dataset_name) <> ''
      ) x
    ),
    
    locality_cte AS (
      SELECT STRING_AGG(locality_name, ', ' ORDER BY locality_name) AS locality_summary
      FROM (
        SELECT DISTINCT locality_name
        FROM base
        WHERE locality_name IS NOT NULL AND TRIM(locality_name) <> ''
        LIMIT 10
      ) x
    ),
    
    media_cte AS (
      SELECT media_url
      FROM base
      WHERE media_url IS NOT NULL AND TRIM(media_url) <> ''
      ORDER BY media_url
      LIMIT 1
    )
    
    SELECT
      (SELECT scientific_name FROM names_cte WHERE rn = 1) AS scientific_name,
      COALESCE((SELECT vernacular_name FROM names_cte WHERE rn = 1), '—') AS vernacular_name,
      (SELECT basis_record_summary FROM basis_cte) AS basis_record_summary,
      (SELECT dataset_summary FROM dataset_cte) AS dataset_summary,
      (SELECT locality_summary FROM locality_cte) AS locality_summary,
      (SELECT media_url FROM media_cte) AS media_url,
      COUNT(*) AS n_obs,
      COUNT(DISTINCT dataset_name) AS n_datasets,
      COUNT(DISTINCT locality_name) AS n_localities,
      COUNT(DISTINCT basis_record) AS n_basis_types,
      COUNT(*) FILTER (
        WHERE media_url IS NOT NULL AND TRIM(media_url) <> ''
      ) AS n_obs_with_media,
      MIN(observation_year) AS first_year,
      MAX(observation_year) AS last_year,
      MIN(event_date) AS first_event_date,
      MAX(event_date) AS last_event_date
    FROM base
    ",
    sql_escape(con, scientific_name_key)
  )
  
  out <- DBI::dbGetQuery(con, sql)
  if (nrow(out) == 0) return(NULL)
  out
}