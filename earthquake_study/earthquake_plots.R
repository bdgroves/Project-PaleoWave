# Magnitude histogram
ggplot(quakes, aes(mag)) +
  geom_histogram(binwidth = 0.2, boundary = 0, closed = "left") +
  scale_x_continuous(breaks = seq(3, max(quakes$mag, na.rm = TRUE), 0.5)) +
  labs(title = "Magnitude distribution (M≥3, USA)", x = "Magnitude", y = "Events")

# Depth vs. Magnitude (depth increasing downward)
ggplot(quakes, aes(mag, depth)) +
  geom_point(alpha = 0.6) +
  scale_y_reverse() +
  labs(title = "Depth vs. Magnitude", x = "Magnitude", y = "Depth (km; inverted)")

# Daily counts since 2024-10-15
ggplot(ts_day, aes(date, events)) +
  geom_col() +
  labs(title = "Daily earthquake counts (M≥3, USA)", x = "Date (UTC)", y = "Events/day")
