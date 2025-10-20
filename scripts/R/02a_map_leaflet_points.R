#!/usr/bin/env Rscript
# Project PaleoWave — Leaflet PBDB points (multi-basemap + overlays, version-safe)

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(leaflet)
  library(htmlwidgets)
  library(tigris) # counties overlay
  options(tigris_use_cache = TRUE)
})

IN_GPKG  <- "data/processed/pbdb_ichthyosaurs_nv.gpkg"
LAYER    <- "occurrences"
OUT_HTML <- "data/outputs/pbdb_points_leaflet.html"

dir.create("data/outputs", recursive = TRUE, showWarnings = FALSE)

# --- Load points ---
pts_sf <- sf::st_read(IN_GPKG, layer = LAYER, quiet = TRUE)
stopifnot(nrow(pts_sf) > 0)
if (is.na(sf::st_crs(pts_sf))) sf::st_crs(pts_sf) <- 4326
pts_sf <- sf::st_transform(pts_sf, 4326)

# lon/lat + simple popups
coords      <- sf::st_coordinates(pts_sf)
pts_df      <- sf::st_drop_geometry(pts_sf)
pts_df$lon  <- coords[,1]
pts_df$lat  <- coords[,2]

popup_cols <- intersect(
  c("taxon_name","identified_name","early_interval","stage",
    "formation","member","lithology1","environment","reference_no"),
  names(pts_df)
)
make_popup <- function(r) {
  title <- if (!is.na(r[["taxon_name"]])) r[["taxon_name"]] else "Record"
  lines <- if (length(popup_cols)) {
    paste(sprintf("<b>%s:</b> %s", popup_cols, ifelse(is.na(r[popup_cols]), "", r[popup_cols])), collapse = "<br>")
  } else ""
  paste0("<b>", title, "</b>", if (nzchar(lines)) paste0("<br>", lines) else "")
}
pts_df$popup_html <- apply(pts_df[, c(intersect(popup_cols, names(pts_df)), "taxon_name"), drop = FALSE], 1, make_popup)

# Nevada counties overlay
nv_counties <- tigris::counties(state = "NV", year = 2023, cb = TRUE, progress_bar = FALSE) |>
  sf::st_transform(4326)

# Map extent
bb <- unname(as.numeric(sf::st_bbox(pts_sf))) # xmin, ymin, xmax, ymax

# --- Version-safe basemap builder -----------------------------------------------
avail <- names(providers)  # what your leaflet knows about
# candidates (we'll keep the ones you have)
candidates <- c(
  "CartoDB.Positron",
  "OpenStreetMap",
  "Esri.WorldTopoMap",
  "Esri.WorldImagery",
  "Stamen.Terrain",           # common Stamen name
  "Stamen.TonerLite",
  "Esri.WorldShadedRelief",
  "OpenTopoMap"
)
to_add <- intersect(candidates, avail)

# Pretty labels for the control (fallback to the provider key if no pretty found)
labels <- c(
  "CartoDB.Positron"      = "Light (Positron)",
  "OpenStreetMap"         = "OSM Streets",
  "Esri.WorldTopoMap"     = "Esri Topographic",
  "Esri.WorldImagery"     = "Esri Imagery",
  "Stamen.Terrain"        = "Stamen Terrain",
  "Stamen.TonerLite"      = "Stamen Toner Lite",
  "Esri.WorldShadedRelief"= "Esri Shaded Relief",
  "OpenTopoMap"           = "OpenTopoMap"
)
group_names <- unname(ifelse(to_add %in% names(labels), labels[to_add], to_add))

# Build the map
m <- leaflet(options = leafletOptions(minZoom = 5))

# Add base layers that exist on your system
for (i in seq_along(to_add)) {
  m <- addProviderTiles(m, providers[[to_add[i]]], group = group_names[i])
}

# Overlays: counties + points (and optional clusters)
m <- m |>
  addPolygons(
    data = nv_counties, group = "Counties",
    weight = 1, color = "#666666", fill = FALSE, opacity = 0.7
  ) |>
  addCircleMarkers(
    data = pts_df, group = "Points",
    lng = ~lon, lat = ~lat,
    radius = 6, weight = 1, stroke = TRUE,
    color = "white", fillColor = "black", fillOpacity = 0.9,
    popup = ~popup_html
  ) |>
  addCircleMarkers(
    data = pts_df, group = "Clusters",
    lng = ~lon, lat = ~lat,
    radius = 5, weight = 1, stroke = TRUE,
    color = "white", fillColor = "black", fillOpacity = 0.9,
    popup = ~popup_html,
    clusterOptions = markerClusterOptions()
  ) |>
  addScaleBar(position = "bottomleft") |>
  addLayersControl(
    baseGroups    = group_names,
    overlayGroups = c("Points", "Clusters", "Counties"),
    options       = layersControlOptions(collapsed = FALSE)
  ) |>
  hideGroup("Clusters") |>
  fitBounds(lng1 = bb[1], lat1 = bb[2], lng2 = bb[3], lat2 = bb[4])

htmlwidgets::saveWidget(m, OUT_HTML, selfcontained = TRUE)
message("✓ Leaflet map saved: ", OUT_HTML)
