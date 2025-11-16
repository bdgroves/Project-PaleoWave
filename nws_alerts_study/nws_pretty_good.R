# ============================================================
# NOAA WWA + CAP FAILOVER → Clean sf → Leaflet Map
# Counties + Active Filtering + Alert Levels
# ============================================================

# 0) Packages ------------------------------------------------
packages <- c("httr","jsonlite","sf","dplyr","leaflet",
              "RColorBrewer","tigris","lubridate","xml2")
to_install <- setdiff(packages, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install, quiet=TRUE)
lapply(packages, library, character.only = TRUE)

options(tigris_use_cache = TRUE)

# ----------------- Parameters -----------------
arcgis_url <- "https://mapservices.weather.noaa.gov/eventdriven/rest/services/WWA/watch_warn_adv/MapServer/1/query"
query_params <- list(where = "1=1", outFields = "*", f = "geojson")

# Patterns for filtering important alerts
include_types_regex <- "(Warning|Watch|Advisory|Tornado|Flood|Heat|Hurricane|Wind|Blizzard|Storm)"
exclude_types_regex <- "(Special Weather Statement|Special Marine Warning|Information|Statement)"

# ============================================================
# 1) Try ArcGIS WWA feed
# ============================================================
message("Fetching ArcGIS WWA alerts...")

resp <- httr::GET(arcgis_url, query = query_params)
httr::stop_for_status(resp)

geojson_txt <- httr::content(resp, "text", encoding="UTF-8")

alerts_sf <- try(sf::st_read(geojson_txt, quiet=TRUE), silent=TRUE)

# ============================================================
# 2) If ArcGIS fails → fallback to NOAA CAP active feed
# ============================================================
if(inherits(alerts_sf,"try-error") || nrow(alerts_sf)==0){
  
  message("ArcGIS returned 0 alerts — switching to NOAA CAP feed...")
  
  cap_url <- "https://api.weather.gov/alerts/active"
  cap_resp <- httr::GET(cap_url, httr::timeout(20))
  httr::stop_for_status(cap_resp)
  
  cap_json <- httr::content(cap_resp, as="text", encoding="UTF-8")
  cap_list <- jsonlite::fromJSON(cap_json)
  
  if("features" %in% names(cap_list)){
    alerts_sf <- sf::st_as_sf(cap_list$features)
  } else {
    stop("No usable data returned from NOAA CAP.")
  }
  
  # Fix fields
  alerts_sf$prod_type  <- alerts_sf$properties$event
  alerts_sf$issuance   <- alerts_sf$properties$onset
  alerts_sf$expiration <- alerts_sf$properties$ends
  alerts_sf$url        <- alerts_sf$properties$`@id`
  
} else {
  
  message(sprintf("Fetched %d alert polygons from ArcGIS", nrow(alerts_sf)))
  
}

# Convert CRS
alerts_sf <- sf::st_transform(alerts_sf, 4326)

# ============================================================
# 3) Clean & filter alert text fields
# ============================================================
alerts_sf <- alerts_sf %>%
  mutate(prod_type = as.character(prod_type)) %>%
  filter(
    grepl(include_types_regex, prod_type, ignore.case = TRUE),
    !grepl(exclude_types_regex, prod_type, ignore.case = TRUE)
  )

message(sprintf("After filtering: %d important alerts", nrow(alerts_sf)))

# ============================================================
# 4) Parse times & keep currently active alerts only
# ============================================================
alerts_sf <- alerts_sf %>%
  mutate(
    issuance   = suppressWarnings(ymd_hms(issuance, tz="UTC")),
    expiration = suppressWarnings(ymd_hms(expiration, tz="UTC"))
  ) %>%
  filter(!is.na(issuance), !is.na(expiration)) %>%
  filter(Sys.time() >= issuance & Sys.time() <= expiration)

message(sprintf("Active alerts right now: %d", nrow(alerts_sf)))

# ============================================================
# 5) Load states + counties and spatially join
# ============================================================
states_sf   <- tigris::states(cb=TRUE) %>% st_transform(4326)
counties_sf <- tigris::counties(cb=TRUE) %>% st_transform(4326)

alerts_sf <- st_join(alerts_sf, states_sf["STUSPS"], join=st_intersects)
alerts_sf <- st_join(alerts_sf, counties_sf[,c("STATEFP","COUNTYFP","NAME")], join=st_intersects)

# ============================================================
# 6) Classify alert levels
# ============================================================
alerts_sf <- alerts_sf %>%
  mutate(alert_level =
           case_when(
             grepl("Warning", prod_type, ignore.case=TRUE) ~ "Warning",
             grepl("Watch",   prod_type, ignore.case=TRUE) ~ "Watch",
             grepl("Advisory",prod_type, ignore.case=TRUE) ~ "Advisory",
             TRUE ~ "Other"
           )
  )

# ============================================================
# 7) Prepare map layers
# ============================================================
pal <- colorFactor(
  palette = c("Warning"="red", "Watch"="orange", "Advisory"="yellow", "Other"="gray"),
  domain = alerts_sf$alert_level
)

alerts_split <- split(alerts_sf, alerts_sf$alert_level)

# ============================================================
# 8) LEAFLET MAP
# ============================================================
m <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron")

for(level in names(alerts_split)) {
  
  m <- m %>%
    addPolygons(
      data = alerts_split[[level]],
      color = ~pal(alert_level),
      fillOpacity = 0.35,
      opacity = 0.8,
      weight = 2,
      group = level,
      popup = ~paste0(
        "<b>", prod_type, "</b><br>",
        "<b>Phenomenon:</b> ", phenom, "<br>",
        "<b>Issued:</b> ", issuance, "<br>",
        "<b>Expires:</b> ", expiration, "<br>",
        ifelse(!is.na(STUSPS), paste0("<b>State:</b> ", STUSPS, "<br>"), ""),
        ifelse(!is.na(NAME), paste0("<b>County:</b> ", NAME, "<br>"), ""),
        "<b>More info:</b> <a href='", url, "' target='_blank'>Open</a>"
      )
    )
}

m <- m %>%
  addLayersControl(
    overlayGroups = names(alerts_split),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = alerts_sf$alert_level,
    title = "Alert Level"
  )

# ============================================================
# 9) Show map
# ============================================================
m
