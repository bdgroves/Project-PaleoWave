library(dplyr)

# Count alerts by prod_type
alerts_summary_type <- alerts_with_counties %>%
  st_drop_geometry() %>% 
  group_by(prod_type) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

alerts_summary_type

# Count alerts by state
alerts_summary_state <- alerts_with_counties %>%
  st_drop_geometry() %>%
  group_by(STUSPS) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

alerts_summary_state

# Count alerts by county
alerts_summary_county <- alerts_with_counties %>%
  st_drop_geometry() %>%
  group_by(STUSPS, NAME) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Aggregate alerts by county
county_alerts <- alerts_with_counties %>%
  st_drop_geometry() %>%
  group_by(STATEFP, COUNTYFP) %>%
  summarise(alert_count = n())

# Join back to county polygons
county_sf <- counties_sf %>%
  left_join(county_alerts, by = c("STATEFP", "COUNTYFP")) %>%
  mutate(alert_count = ifelse(is.na(alert_count), 0, alert_count))

# Leaflet choropleth
pal_county <- colorNumeric("YlOrRd", domain = county_sf$alert_count)

leaflet(county_sf) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal_county(alert_count),
    color = "gray",
    weight = 1,
    fillOpacity = 0.7,
    label = ~paste0(NAME, ", ", STUSPS, ": ", alert_count, " alerts")
  ) %>%
  addLegend("bottomright", pal = pal_county, values = ~alert_count, title = "Alerts per County")


