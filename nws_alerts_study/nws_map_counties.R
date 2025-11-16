# ============================================================
# NOAA WWA Alerts → sf → Leaflet Map (Enhanced)
# Filtered, Color-coded, Joined with States & Counties
# ============================================================

# 0) Packages ------------------------------------------------
packages <- c(
  "httr", "jsonlite", "sf", "dplyr", "leaflet", "RColorBrewer", "tigris"
)
to_install <- setdiff(packages, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install, quiet = TRUE)
lapply(packages, library, character.only = TRUE)

options(tigris_use_cache = TRUE)

# 1) ArcGIS REST Service URL for Watches/Warnings
arcgis_url <- "https://mapservices.weather.noaa.gov/eventdriven/rest/services/WWA/watch_warn_adv/MapServer/1/query"

# 2) Query parameters (GeoJSON)
params <- list(
  where = "1=1",
  outFields = "*",
  f = "geojson"
)

# 3) Fetch GeoJSON
resp <- httr::GET(arcgis_url, query = params)
httr::stop_for_status(resp)
geojson_txt <- httr::content(resp, "text", encoding = "UTF-8")

# 4) Read into sf
alerts_sf <- sf::st_read(geojson_txt, quiet = TRUE)
alerts_sf <- sf::st_transform(alerts_sf, 4326)  # Leaflet needs WGS84

# 5) Keep only important alerts (filter out general ones)
# Common unimportant: "Special Weather Statement", "Special Marine Warning"
important_alerts <- alerts_sf %>%
  filter(!prod_type %in% c("Special Weather Statement", "Special Marine Warning"))

# 6) Join with US state and county boundaries (tigris)
states_sf  <- tigris::states(cb = TRUE) %>% sf::st_transform(4326)
counties_sf <- tigris::counties(cb = TRUE) %>% sf::st_transform(4326)

# Optional: add state name to each alert
important_alerts <- sf::st_join(important_alerts, states_sf["STUSPS"], join = st_intersects)

# 7) Define color palette for event type
# Assign 3 main colors: Watch, Warning, Advisory
important_alerts <- important_alerts %>%
  mutate(alert_level = case_when(
    prod_type %in% c("Watch") ~ "Watch",
    prod_type %in% c("Warning") ~ "Warning",
    prod_type %in% c("Advisory", "Flood Advisory") ~ "Advisory",
    TRUE ~ "Other"
  ))

pal <- colorFactor(
  palette = c("Warning" = "red", "Watch" = "orange", "Advisory" = "yellow", "Other" = "gray"),
  domain = important_alerts$alert_level
)

# 8) Leaflet Map
leaflet(important_alerts) %>%
  addTiles() %>%
  addPolygons(
    color = ~pal(alert_level),
    weight = 2,
    opacity = 0.7,
    fillOpacity = 0.3,
    label = ~paste0(prod_type, " - ", phenom, " (", STUSPS, ")"),
    popup = ~paste0(
      "<b>Event:</b> ", prod_type, "<br>",
      "<b>Phenomenon:</b> ", phenom, "<br>",
      "<b>Status:</b> ", msg_type, "<br>",
      "<b>Issued:</b> ", issuance, "<br>",
      "<b>Expires:</b> ", expiration, "<br>",
      "<b>State:</b> ", STUSPS, "<br>",
      "<b>More info:</b> <a href='", url, "' target='_blank'>Link</a>"
    )
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~alert_level,
    title = "Alert Level"
  )
