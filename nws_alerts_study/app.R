# app.R
# NOAA WWA Alerts Dashboard — Full-featured with CSV export, time slider, DBSCAN hotspots

# ---------------------------
# 0) Packages (install if missing)
# ---------------------------
packages <- c(
  "shiny","sf","httr","jsonlite","dplyr","tigris",
  "leaflet","leaflet.extras","RColorBrewer","ggplot2","DT","lubridate","dbscan"
)
to_install <- setdiff(packages, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org", quiet = TRUE)
lapply(packages, library, character.only = TRUE)

options(tigris_use_cache = TRUE)

# ---------------------------
# Helper: fetch & prepare alerts (returns list with alerts, states, counties)
# ---------------------------
fetch_alerts_with_counties <- function() {
  arcgis_url <- "https://mapservices.weather.noaa.gov/eventdriven/rest/services/WWA/watch_warn_adv/MapServer/1/query"
  params <- list(where = "1=1", outFields = "*", f = "geojson")
  resp <- httr::GET(arcgis_url, query = params)
  httr::stop_for_status(resp)
  geojson_txt <- httr::content(resp, "text", encoding = "UTF-8")
  
  alerts_sf <- try(sf::st_read(geojson_txt, quiet = TRUE), silent = TRUE)
  if (inherits(alerts_sf, "try-error")) stop("Failed to read GeoJSON from ArcGIS service.")
  alerts_sf <- sf::st_transform(alerts_sf, 4326)
  
  # Filter out obvious low-priority/general items (customize as needed)
  alerts_sf <- alerts_sf %>%
    filter(!prod_type %in% c("Special Weather Statement", "Special Marine Warning"))
  
  # Load states and counties (cached)
  states_sf <- tigris::states(cb = TRUE) %>% sf::st_transform(4326)
  counties_sf <- tigris::counties(cb = TRUE) %>% sf::st_transform(4326)
  
  # Join states & counties (alerts may produce multiple county rows)
  alerts_st <- sf::st_join(alerts_sf, states_sf["STUSPS"], join = sf::st_intersects)
  alerts_county <- sf::st_join(alerts_st, counties_sf[, c("STATEFP","COUNTYFP","NAME")], join = sf::st_intersects)
  
  # Parse times robustly
  alerts_county <- alerts_county %>%
    mutate(
      issuance = suppressWarnings(lubridate::ymd_hms(issuance, tz = "UTC")),
      expiration = suppressWarnings(lubridate::ymd_hms(expiration, tz = "UTC")),
      onset = suppressWarnings(lubridate::ymd_hms(onset, tz = "UTC"))
    )
  
  # Compute duration hours
  alerts_county <- alerts_county %>%
    mutate(duration_h = as.numeric(difftime(expiration, issuance, units = "hours")))
  
  # Alert level classification
  alerts_county <- alerts_county %>%
    mutate(alert_level = case_when(
      grepl("Warning", prod_type, ignore.case = TRUE) ~ "Warning",
      grepl("Watch", prod_type, ignore.case = TRUE)   ~ "Watch",
      grepl("Advisory", prod_type, ignore.case = TRUE) ~ "Advisory",
      TRUE ~ "Other"
    ))
  
  list(alerts = alerts_county, states = states_sf, counties = counties_sf)
}

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  titlePanel("NOAA WWA Alerts — Dashboard"),
  sidebarLayout(
    sidebarPanel(
      actionButton("refresh", "Refresh data (live)", icon = icon("sync")),
      br(), br(),
      sliderInput("time_window", "Issued within last X hours:",
                  min = 1, max = 48, value = 24, step = 1),
      checkboxGroupInput("levels", "Alert levels to show (map & heatmap)",
                         choices = c("Warning","Watch","Advisory","Other"),
                         selected = c("Warning","Watch","Advisory")),
      numericInput("min_duration", "Min duration (hours) to include (histogram)", value = 0, min = 0),
      checkboxInput("active_only", "Only currently active alerts (issuance ≤ now ≤ expiration)", value = TRUE),
      br(),
      helpText("Data source: NOAA WWA ArcGIS MapServer (updated ~every 5 minutes). Counties from TIGER/US Census.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("County Heatmap",
                 downloadButton("export_county", "Download County Counts CSV"),
                 br(), br(),
                 leafletOutput("heatmap", height = 600)
        ),
        tabPanel("Top alert types by state", DTOutput("top_table")),
        tabPanel("Warnings Map", leafletOutput("warnings_map", height = 600)),
        tabPanel("Duration histogram", plotOutput("duration_hist", height = 400)),
        tabPanel("Spatial Hotspots (DBSCAN)", leafletOutput("cluster_map", height = 600)),
        tabPanel("Raw data (sample)", DTOutput("sample"))
      )
    )
  )
)

# ---------------------------
# Server
# ---------------------------
server <- function(input, output, session) {
  
  # 1) reactive dataset that fetches on start and when refresh pressed
  alerts_data <- eventReactive(input$refresh, {
    showNotification("Fetching alerts & county data...", type = "message", duration = 2)
    fetch_alerts_with_counties()
  }, ignoreNULL = FALSE)
  
  # 2) filtered alerts reactive (applies time window, active-only, and level filters)
  filtered_alerts <- reactive({
    dat <- alerts_data()$alerts
    
    # time window filter (issued within last X hours)
    cut_time <- Sys.time() - lubridate::hours(input$time_window)
    dat <- dat %>% filter(is.na(issuance) | issuance >= cut_time)
    
    # active-only filter
    if (isTRUE(input$active_only)) {
      now <- Sys.time()
      dat <- dat %>% filter(!is.na(issuance) & !is.na(expiration) & issuance <= now & now <= expiration)
    }
    
    # levels filter
    if (length(input$levels) > 0) {
      dat <- dat %>% filter(alert_level %in% input$levels)
    } else {
      dat <- dat %>% slice(0)
    }
    
    dat
  })
  
  # 3) county aggregation reactive (for heatmap & CSV)
  county_counts <- reactive({
    dat <- filtered_alerts()
    counties <- alerts_data()$counties
    if (nrow(dat) == 0) {
      counties %>% mutate(alert_count = 0)
    } else {
      agg <- dat %>%
        st_drop_geometry() %>%
        group_by(STATEFP, COUNTYFP) %>%
        summarise(alert_count = n(), .groups = "drop")
      counties %>%
        left_join(agg, by = c("STATEFP","COUNTYFP")) %>%
        mutate(alert_count = ifelse(is.na(alert_count), 0, alert_count))
    }
  })
  
  # ---------------------------
  # County heatmap & CSV export
  # ---------------------------
  output$heatmap <- renderLeaflet({
    counties <- county_counts()
    palc <- colorNumeric("YlOrRd", domain = counties$alert_count, na.color = "transparent")
    
    m <- leaflet(counties) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~palc(alert_count),
        color = "#444444",
        weight = 0.5,
        fillOpacity = 0.8,
        label = ~paste0(NAME, ", ", STATEFP, ": ", alert_count, " alert(s)"),
        highlightOptions = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = palc, values = ~alert_count, title = "Alerts per county")
    m
  })
  
  output$export_county <- downloadHandler(
    filename = function() paste0("noaa_county_alert_counts_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- st_drop_geometry(county_counts()) %>%
        select(STATEFP, COUNTYFP, NAME, alert_count)
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # ---------------------------
  # Top alert types by state table
  # ---------------------------
  output$top_table <- renderDT({
    dat <- filtered_alerts() %>% st_drop_geometry()
    if (nrow(dat) == 0) {
      datatable(data.frame(note = "No data for selected filters"), options = list(dom = 't'))
    } else {
      top_by_state <- dat %>%
        group_by(STUSPS, prod_type) %>%
        summarise(count = n(), .groups = "drop") %>%
        arrange(STUSPS, desc(count)) %>%
        group_by(STUSPS) %>%
        slice_max(order_by = count, n = 5) %>%
        ungroup()
      datatable(top_by_state, options = list(pageLength = 10), rownames = FALSE)
    }
  })
  
  # ---------------------------
  # Warnings-only map with toggleable prod_type layers
  # ---------------------------
  output$warnings_map <- renderLeaflet({
    dat <- filtered_alerts() %>% filter(alert_level == "Warning")
    if (nrow(dat) == 0) {
      return(leaflet() %>% addProviderTiles("CartoDB.Positron"))
    }
    
    types <- unique(dat$prod_type)
    pal_warn <- colorFactor(brewer.pal(min(8, length(types)), "Set1"), domain = types)
    m <- leaflet(dat) %>% addProviderTiles("CartoDB.Positron")
    
    split_list <- split(dat, dat$prod_type)
    for (nm in names(split_list)) {
      m <- m %>% addPolygons(
        data = split_list[[nm]],
        color = ~pal_warn(prod_type),
        weight = 2, fillOpacity = 0.5,
        group = nm,
        label = ~paste0(prod_type, " — ", NAME, ", ", STUSPS),
        popup = ~paste0("<b>", prod_type, "</b><br/>County: ", NAME, "<br/>State: ", STUSPS,
                        "<br/>Issued: ", issuance, "<br/>Expires: ", expiration,
                        "<br/><a href='", url, "' target='_blank'>More</a>")
      )
    }
    
    m <- m %>%
      addLayersControl(overlayGroups = names(split_list), options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend("bottomright", pal = pal_warn, values = types, title = "Warning types")
    
    # fit bounds
    b <- sf::st_bbox(dat)
    m <- m %>% fitBounds(b["xmin"], b["ymin"], b["xmax"], b["ymax"])
    m
  })
  
  # ---------------------------
  # Duration histogram
  # ---------------------------
  output$duration_hist <- renderPlot({
    dat <- filtered_alerts() %>% st_drop_geometry() %>%
      filter(!is.na(duration_h) & duration_h >= input$min_duration)
    if (nrow(dat) == 0) {
      ggplot() + geom_blank() + ggtitle("No data to show")
    } else {
      ggplot(dat, aes(x = duration_h, fill = alert_level)) +
        geom_histogram(position = "stack", bins = 40, alpha = 0.8, color = "black") +
        scale_x_continuous(name = "Duration (hours)") +
        scale_fill_brewer(type = "qual", palette = "Set1") +
        theme_minimal() +
        ggtitle("Distribution of alert durations (hours)")
    }
  })
  
  # ---------------------------
  # DBSCAN clustering: centroids -> project to meters -> dbscan -> hulls
  # ---------------------------
  cluster_data <- reactive({
    dat <- filtered_alerts()
    if (nrow(dat) == 0) return(NULL)
    
    # get centroids and transform to web mercator (meters)
    cent <- sf::st_centroid(dat$geometry)
    cent_m <- sf::st_transform(sf::st_as_sf(data.frame(geometry = cent)), 3857)
    coords <- sf::st_coordinates(cent_m)
    
    # tuneable params (could expose in UI later)
    eps_m <- 50000   # 50 km radius
    min_pts <- 5
    
    if (nrow(coords) < min_pts) {
      dat$cluster_id <- 0L
      return(list(points = dat, hulls = NULL))
    }
    
    db <- dbscan::dbscan(coords, eps = eps_m, minPts = min_pts)
    
    dat$cluster_id <- db$cluster
    
    # build hulls for clusters > 0
    clusters <- unique(db$cluster[db$cluster > 0])
    hulls <- list()
    for (cid in clusters) {
      idx <- which(dat$cluster_id == cid)
      if (length(idx) < 3) next
      sub <- dat[idx, ]
      # union and hull in original CRS (use projected then back)
      hull_proj <- sf::st_convex_hull(sf::st_union(sf::st_transform(sub$geometry, 3857)))
      hull_geo <- sf::st_transform(hull_proj, 4326)
      hulls[[length(hulls) + 1]] <- sf::st_sf(cluster = cid, n_alerts = length(idx), geometry = hull_geo)
    }
    hulls_sf <- if (length(hulls) > 0) do.call(rbind, hulls) else NULL
    
    list(points = dat, hulls = hulls_sf)
  })
  
  output$cluster_map <- renderLeaflet({
    cc <- cluster_data()
    if (is.null(cc)) {
      return(leaflet() %>% addProviderTiles("CartoDB.Positron"))
    }
    pts <- cc$points
    hulls <- cc$hulls
    
    palc <- colorFactor(brewer.pal(9, "Set1"), domain = unique(pts$cluster_id))
    
    m <- leaflet() %>% addProviderTiles("CartoDB.Positron")
    
    if (!is.null(hulls) && nrow(hulls) > 0) {
      m <- m %>% addPolygons(data = hulls,
                             fillColor = "blue", color = "darkblue",
                             weight = 2, fillOpacity = 0.25,
                             label = ~paste0("Cluster ", cluster, " (", n_alerts, " alerts)"))
    }
    
    m <- m %>% addCircleMarkers(
      data = pts,
      radius = 4,
      fillOpacity = 0.8,
      color = ~palc(cluster_id),
      stroke = FALSE,
      label = ~paste0(prod_type, "<br/>Cluster: ", cluster_id)
    )
    m
  })
  
  # ---------------------------
  # raw sample table
  # ---------------------------
  output$sample <- renderDT({
    dat <- filtered_alerts() %>% st_drop_geometry() %>%
      select(issuance, expiration, prod_type, alert_level, STUSPS, NAME, url) %>%
      arrange(desc(issuance))
    if (nrow(dat) == 0) dat <- data.frame(note = "No rows for selected filters")
    datatable(dat, options = list(pageLength = 10), rownames = FALSE)
  })
  
}

# ---------------------------
# Run app
# ---------------------------
shinyApp(ui, server)
