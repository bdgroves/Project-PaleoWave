# Convert ArcGIS date fields (usually epoch ms) to POSIXct if numeric
as_posix_maybe <- function(x) {
  if (is.numeric(x)) as.POSIXct(x / 1000, origin = "1970-01-01", tz = "UTC")
  else suppressWarnings(as.POSIXct(x, tz = "UTC"))
}

quakes <- quakes %>%
  mutate(
    eventTime_dt = as_posix_maybe(eventTime),
    updated_dt   = as_posix_maybe(updated),
    mag          = suppressWarnings(as.numeric(mag)),
    depth        = suppressWarnings(as.numeric(depth)),
    # Parse a "state-ish" token from 'place' (last token before ", USA")
    state_guess = {
      m <- stringr::str_match(place, ",\\s*([^,]+)\\s*,\\s*USA\\s*$")
      # If not matched, try last token after last comma (still noisy but useful)
      sg <- ifelse(!is.na(m[,2]), m[,2], stringr::str_trim(stringr::word(place, -1, sep = fixed(","))))
      ifelse(is.na(sg), "Unknown", sg)
    },
    # Magnitude bins for summary
    mag_bin = cut(
      mag,
      breaks = c(3, 4, 5, 6, 10),  # [3,4), [4,5), [5,6), [6,10)
      right  = FALSE,
      labels = c("3.0–3.9", "4.0–4.9", "5.0–5.9", "6.0+")
    )
  ) %>%
  st_as_sf(crs = 4326)  # ensure geometry tagged as WGS84
