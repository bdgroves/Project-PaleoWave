library(arcgislayers)

# Build the same query but ask for ESRI JSON, not GeoJSON
build_url_esri <- function(where_clause) {
  modify_url(
    base_url,
    query = list(
      where = where_clause,
      outFields = "*",
      outSR = 4326,
      f = "json",              # <---- IMPORTANT
      orderByFields = "eventTime DESC",
      resultRecordCount = 2000
    )
  )
}

attempt_1_url <- build_url_esri(where_sql)

quakes <- try(
  arcgislayers::arc_open(attempt_1_url) |> arcgislayers::arc_select(),
  silent = TRUE
)

if (inherits(quakes, "try-error")) {
  epoch_ms <- as.integer(as.numeric(since_utc) * 1000)
  where_epoch <- sprintf("mag >= 3 AND eventTime >= %d AND place LIKE '%%USA%%'", epoch_ms)
  attempt_2_url <- build_url_esri(where_epoch)
  
  quakes <- arcgislayers::arc_open(attempt_2_url) |> arcgislayers::arc_select()
}

quakes <- st_as_sf(quakes)  # keep geometry+attributes
str(quakes)
