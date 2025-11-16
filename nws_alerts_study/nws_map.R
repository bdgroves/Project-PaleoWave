# ============================================================
# NOAA WWA Alerts → sf → Leaflet Map
# Fully Working Script
# ============================================================

# 0) Packages ------------------------------------------------
packages <- c(
  "httr", "jsonlite", "sf", "dplyr", "leaflet", "RColorBrewer"
)
to_install <- setdiff(packages, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install, quiet = TRUE)
lapply(packages, library, character.only = TRUE)

# 1) ArcGIS REST Service URL for Watches/Warnings
arcgis_url <- "https://mapservices.weather.noaa.gov/eventdriven/rest/services/WWA/watch_warn_adv/MapServer/1/query"

# 2) Build query parameters (get all features as GeoJSON)
params <- list(
  where = "1=1",           # all features
  outFields = "*",         # all fields
  f = "geojson"            # return GeoJSON
)

# 3) Fetch GeoJSON
resp <- httr::GET(arcgis_url, query = params)
httr::stop_for_status(resp)
geojson_txt <- httr::content(resp, "text", encoding = "UTF-8")

# 4) Read into sf
alerts_sf <- sf::st_read(geojson_txt, quiet = TRUE)

# 5) Transform to WGS84 for Leaflet
alerts_sf <- sf::st_transform(alerts_sf, crs = 4326)

# 6) Inspect column names (for reference)
print(names(alerts_sf))

# 7) Simple color palette by prod_type (event type)
# Limit to 5 colors; if more event types exist, colors will recycle
pal <- colorFactor(
  palette = brewer.pal(min(5, length(unique(alerts_sf$prod_type))), "YlOrRd"),
  domain = alerts_sf$prod_type
)

# 8) Leaflet Map
leaflet(alerts_sf) %>%
  addTiles() %>%
  addPolygons(
    color = ~pal(prod_type),
    weight = 2,
    opacity = 0.7,
    fillOpacity = 0.3,
    label = ~paste0(prod_type, " - ", phenom),
    popup = ~paste0(
      "<b>Event:</b> ", prod_type, "<br>",
      "<b>Phenom:</b> ", phenom, "<br>",
      "<b>Status:</b> ", msg_type, "<br>",
      "<b>Issued:</b> ", issuance, "<br>",
      "<b>Expires:</b> ", expiration, "<br>",
      "<b>More info:</b> <a href='", url, "' target='_blank'>Link</a>"
    )
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~prod_type,
    title = "Event Type"
  )
