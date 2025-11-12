# --------------------------
# 1) Packages
# --------------------------
packages <- c("sf", "dplyr", "stringr", "lubridate", "leaflet", "RColorBrewer",
              "ggplot2", "scales", "httr")
to_install <- setdiff(packages, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
lapply(packages, library, character.only = TRUE)

# --------------------------
# 2) Parameters
# --------------------------
base_url <- "https://services9.arcgis.com/RHVPKKiFTONKtxq3/arcgis/rest/services/USGS_Seismic_Data_v1/FeatureServer/0/query"
since_utc <- as.POSIXct("2024-10-15 00:00:00", tz = "UTC")

# ArcGIS REST 'where' with TIMESTAMP literal
where_sql <- sprintf("mag >= 3 AND eventTime >= TIMESTAMP '%s' AND place LIKE '%%USA%%'",
                     format(since_utc, "%Y-%m-%d %H:%M:%S"))

# Build query URL returning GeoJSON in WGS84 (EPSG:4326)
build_url <- function(where_clause) {
  modify_url(
    base_url,
    query = list(
      where = where_clause,
      outFields = "*",
      outSR = 4326,            # request lat/lon
      f = "geojson",
      orderByFields = "eventTime DESC",
      resultRecordCount = 2000 # more than enough for day-scale queries
    )
  )
}

# --------------------------
# 3) Fetch as sf (robustness: fallback to epoch-millis if TIMESTAMP literal fails)
# --------------------------
attempt_1_url <- build_url(where_sql)

fetch_sf <- function(u) {
  # sf can read GeoJSON from URL directly
  sf::read_sf(u, quiet = TRUE)
}

quakes <- try(fetch_sf(attempt_1_url), silent = TRUE)

if (inherits(quakes, "try-error")) {
  # Some ArcGIS layers prefer epoch-millis in the WHERE clause for date fields
  epoch_ms <- as.integer(as.numeric(since_utc) * 1000)
  where_epoch <- sprintf("mag >= 3 AND eventTime >= %d AND place LIKE '%%USA%%'", epoch_ms)
  attempt_2_url <- build_url(where_epoch)
  quakes <- fetch_sf(attempt_2_url)
}

stopifnot(inherits(quakes, "sf"))
message(sprintf("Pulled %s features", nrow(quakes)))
