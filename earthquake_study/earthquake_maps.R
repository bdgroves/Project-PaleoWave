pal <- colorBin("YlOrRd", domain = quakes$mag, bins = c(3,4,5,6,10), na.color = "#bbbbbb")

leaflet(quakes, options = leafletOptions(minZoom = 2)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    radius = ~ scales::rescale(mag, to = c(4, 12), from = range(mag, na.rm = TRUE)),
    color  = ~ pal(mag),
    fillOpacity = 0.8, stroke = FALSE,
    popup = ~ sprintf(
      "<b>%s</b><br/>Mag: %.1f (%s)<br/>Depth: %s km<br/>Time: %s UTC<br/>%s",
      id, mag, magType, ifelse(is.na(depth), "–", sprintf("%.1f", depth)),
      ifelse(is.na(eventTime_dt), "–", format(eventTime_dt, "%Y-%m-%d %H:%M")),
      htmltools::htmlEscape(place)
    )
  ) %>%
  addLegend(pal = pal, values = ~mag, title = "Magnitude", opacity = 0.9)
