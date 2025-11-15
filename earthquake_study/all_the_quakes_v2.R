# ============================================================
# 0) Packages
# ============================================================
packages <- c("sf", "dplyr", "stringr", "lubridate",
              "leaflet", "leaflet.extras", "RColorBrewer",
              "ggplot2", "httr", "arcgislayers", "htmltools", "maps")
to_install <- setdiff(packages, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install, quiet = TRUE)
lapply(packages, library, character.only = TRUE)

# ============================================================
# 1) Parameters
# ============================================================
base_url <- "https://services9.arcgis.com/RHVPKKiFTONKtxq3/arcgis/rest/services/USGS_Seismic_Data_v1/FeatureServer/0/query"
since_utc <- as.POSIXct(Sys.Date()-7, tz="UTC")  # last 7 days

where_sql <- sprintf(
  "eventTime >= TIMESTAMP '%s' AND place LIKE '%%USA%%'",
  format(since_utc, "%Y-%m-%d %H:%M:%S")
)

# ============================================================
# 2) Fetch and convert to sf
# ============================================================
build_url <- function(where_clause) {
  modify_url(base_url,
             query=list(
               where = where_clause,
               outFields = "*",
               outSR = 4326,
               f = "json",
               orderByFields = "eventTime DESC",
               resultRecordCount = 5000
             ))
}

fetch_sf <- function(url) {
  arcgislayers::arc_open(url) |> arcgislayers::arc_select() |> st_as_sf()
}

quakes <- try(fetch_sf(build_url(where_sql)), silent=TRUE)

if(inherits(quakes,"try-error")){
  epoch_ms <- as.integer(as.numeric(since_utc)*1000)
  quakes <- fetch_sf(build_url(sprintf("eventTime >= %d AND place LIKE '%%USA%%'",epoch_ms)))
}

# Ensure WGS84
quakes <- st_transform(quakes, 4326)

# ============================================================
# 3) Parse dates, numeric fields, and state
# ============================================================
as_posix_arc <- function(x){
  if(is.numeric(x)) as.POSIXct(x/1000, origin="1970-01-01", tz="UTC")
  else suppressWarnings(as.POSIXct(x, tz="UTC"))
}

quakes <- quakes %>%
  mutate(
    eventTime_dt = as_posix_arc(eventTime),
    updated_dt   = as_posix_arc(updated),
    mag   = suppressWarnings(as.numeric(mag)),
    depth = suppressWarnings(as.numeric(depth)),
    state_guess = {
      m <- str_match(place, ",\\s*([^,]+)\\s*,\\s*USA\\s*$")
      sg <- ifelse(!is.na(m[,2]),
                   m[,2],
                   str_trim(word(place, -1, sep=fixed(","))))
      ifelse(is.na(sg), "Unknown", sg)
    }
  )

# ============================================================
# 4) State-level summaries (drop geometry for joining)
# ============================================================
state_summary <- quakes %>%
  group_by(state_guess) %>%
  summarize(
    count = n(),
    avg_mag = mean(mag, na.rm=TRUE),
    avg_depth = mean(depth, na.rm=TRUE)
  ) %>% st_set_geometry(NULL)  # DROP MULTIPOINT GEOMETRY

print(state_summary)

# ============================================================
# 5) Color bins for map
# ============================================================
pal_mag <- colorBin("YlOrRd", domain=quakes$mag, bins=c(0,1,2,3,4,5,6,10), na.color="#cccccc")
pal_state <- colorNumeric("Blues", domain=state_summary$count, na.color="#eeeeee")

# ============================================================
# 6) Leaflet map with clusters, heatmap, and choropleth
# ============================================================
# US states polygons
us_states <- st_as_sf(map("state", plot=FALSE, fill=TRUE)) %>%
  mutate(state_guess = str_to_title(ID)) %>%
  left_join(state_summary, by="state_guess")

leaflet(quakes, options=leafletOptions(minZoom=2)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # State choropleth
  addPolygons(data=us_states,
              fillColor = ~pal_state(count),
              fillOpacity = 0.4,
              color="black",
              weight=1,
              label = ~paste0(state_guess, ": ", ifelse(is.na(count), 0, count), " quakes")) %>%
  
  # Marker cluster
  addCircleMarkers(
    radius = ~scales::rescale(mag, to=c(3,12), from=range(mag, na.rm=TRUE)),
    color = ~pal_mag(mag),
    fillOpacity=0.85,
    stroke=FALSE,
    clusterOptions = markerClusterOptions(),
    popup = ~sprintf("<b>%s</b><br>Mag: %.1f (%s)<br>Depth: %s km<br>Time: %s UTC<br>%s",
                     id,
                     mag,
                     magType,
                     ifelse(is.na(depth),"–",sprintf("%.1f",depth)),
                     ifelse(is.na(eventTime_dt),"–",format(eventTime_dt,"%Y-%m-%d %H:%M")),
                     htmlEscape(place))
  ) %>%
  
  # Heatmap layer
  addHeatmap(
    lng = ~st_coordinates(geometry)[,1],
    lat = ~st_coordinates(geometry)[,2],
    intensity = ~mag,
    blur = 20,
    max = 0.05,
    radius = 15
  ) %>%
  
  addLegend(pal=pal_mag, values=~mag, title="Magnitude", opacity=0.9)

# ============================================================
# 7) ggplot: magnitude histogram
# ============================================================
ggplot(quakes, aes(x=mag)) +
  geom_histogram(binwidth=0.5, fill="orange", color="black") +
  theme_minimal() +
  labs(title="Distribution of Earthquake Magnitudes (USA)",
       x="Magnitude", y="Count")

# ============================================================
# 8) ggplot: depth vs magnitude
# ============================================================
ggplot(quakes, aes(x=mag, y=depth)) +
  geom_point(alpha=0.5, color="blue") +
  theme_minimal() +
  labs(title="Depth vs Magnitude", x="Magnitude", y="Depth (km)")
