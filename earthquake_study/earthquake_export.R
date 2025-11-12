# GeoPackage for GIS workflows
st_write(quakes, "usgs_m3_plus_since_2024_10_15.gpkg", layer = "events", delete_layer = TRUE)

# Tables
readr::write_csv(events_by_mag, "events_by_magnitude.csv")
readr::write_csv(by_state,      "events_by_state_guess.csv")
readr::write_csv(ts_day,        "daily_counts.csv")
