events_by_mag <- quakes %>%
  filter(!is.na(mag_bin)) %>%
  count(mag_bin, name = "events") %>%
  arrange(mag_bin)

depth_stats <- quakes %>%
  summarise(
    n          = n(),
    mag_min    = min(mag, na.rm = TRUE),
    mag_median = median(mag, na.rm = TRUE),
    mag_max    = max(mag, na.rm = TRUE),
    depth_med_km = median(depth, na.rm = TRUE),
    .groups = "drop"
  )

by_state <- quakes %>%
  count(state_guess, name = "events") %>%
  arrange(desc(events))

# Time series by date (UTC)
ts_day <- quakes %>%
  mutate(date = as.Date(eventTime_dt)) %>%
  count(date, name = "events") %>%
  arrange(date)

print(events_by_mag)
print(depth_stats)
head(by_state, 10)
head(ts_day, 10)
