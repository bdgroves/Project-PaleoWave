# ============================================================
# 1) Packages
# ============================================================
packages <- c("sf", "dplyr", "stringr", "lubridate",
              "leaflet", "RColorBrewer", "httr", "arcgislayers")
to_install <- setdiff(packages, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
lapply(packages, library, character.only = TRUE)


# ============================================================
# 2) Parameters
# ============================================================
base_url <- "https://services9.arcgis.com/RHVPKKiFTONKtxq3/arcgis/rest/services/USGS_Seismic_Data_v1/FeatureServer/0/query"

since_utc <- as.POSIXct("2024-10-15 00:00:00", tz = "UTC")

# ALL earthquakes (no magnitude filter)
where_sql <- sprintf(
  "eventTime >= TIMESTAMP '%s' AND place LIKE '%%USA%%'",
  format(since_utc, "%Y-%m-%d %H:%M:%S")
)


# ============================================================
# 3) Query Builder  (ESRI JSON — attributes included)
# ============================================================
build_url <- function(where_clause) {
  modify_url(
    base_url,
    query = list(
      where = where_clause,
      outFields = "*",
      outSR = 4326,
      f = "json",
      orderByFields = "eventTime DESC",
      resultRecordCount = 2000
    )
  )
}


# ============================================================
# 4) Fetch + convert to sf
# ============================================================
attempt_1_url <- build_url(where_sql)

fetch_sf <- function(url) {
  arcgislayers::arc_open(url) |> arcgislayers::arc_select() |> st_as_sf()
}

quakes <- try(fetch_sf(attempt_1_url), silent = TRUE)

# fallback: epoch-ms
if (inherits(quakes, "try-error")) {
  epoch_ms <- as.integer(as.numeric(since_utc) * 1000)
  where_epoch <- sprintf(
    "eventTime >= %d AND place LIKE '%%USA%%'", epoch_ms
  )
  quakes <- fetch_sf(build_url(where_epoch))
}

message(sprintf("Pulled %s earthquakes", nrow(quakes)))


# ============================================================
# 5) Fix CRS — ensure WGS84 lon/lat
# ============================================================
quakes <- st_transform(quakes, 4326)


# ============================================================
# 6) Parse dates + numeric fields
# ============================================================
as_posix_arc <- function(x) {
  if (is.numeric(x)) as.POSIXct(x/1000, origin="1970-01-01", tz="UTC")
  else suppressWarnings(as.POSIXct(x, tz="UTC"))
}

quakes <- quakes %>%
  mutate(
    eventTime_dt = as_posix_arc(eventTime),
    updated_dt   = as_posix_arc(updated),
    mag   = suppressWarnings(as.numeric(mag)),
    depth = suppressWarnings(as.numeric(depth)),
    state_guess = {
      m  <- stringr::str_match(place, ",\\s*([^,]+)\\s*,\\s*USA\\s*$")
      sg <- ifelse(!is.na(m[,2]),
                   m[,2],
                   stringr::str_trim(stringr::word(place, -1, sep=fixed(","))))
      ifelse(is.na(sg), "Unknown", sg)
    }
  )


# ============================================================
# 7) Color scale for ALL magnitudes
# ============================================================
pal <- colorBin(
  "YlOrRd",
  domain = quakes$mag,
  bins = c(0, 1, 2, 3, 4, 5, 6, 10),
  na.color = "#cccccc"
)


# ============================================================
# 8) Leaflet map
# ============================================================
leaflet(quakes, options = leafletOptions(minZoom = 2)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    radius = ~ scales::rescale(mag, to = c(3, 12),
                               from = range(mag, na.rm = TRUE)),
    color = ~ pal(mag),
    fillOpacity = 0.85,
    stroke = FALSE,
    popup = ~ sprintf(
      "<b>%s</b><br/>Magnitude: %.1f (%s)<br/>Depth: %s km<br/>Time: %s UTC<br/><br/>%s",
      id,
      mag,
      magType,
      ifelse(is.na(depth), '–', sprintf('%.1f', depth)),
      ifelse(is.na(eventTime_dt), '–', format(eventTime_dt, "%Y-%m-%d %H:%M")),
      htmltools::htmlEscape(place)
    )
  ) %>%
  addLegend(
    pal = pal, values = ~mag, title = "Magnitude", opacity = 0.95
  )
