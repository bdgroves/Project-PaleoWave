#!/usr/bin/env Rscript
# Project PaleoWave — PBDB vs Triassic Geology overlay QA (fixed unit label coalesce)
# - Loads PBDB points + filtered geology polygons
# - (Optionally) buffers polygons slightly to account for mapping/rounding
# - Spatial join: which polygon does each point fall within?
# - Summaries + CSV export + GPKG layers for QGIS inspection

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
})

# ============================== CONFIG =========================================
PTS_DSN   <- Sys.getenv("PW_PTS_DSN",   unset = "data/processed/pbdb_ichthyosaurs_nv.gpkg")
PTS_LAYER <- Sys.getenv("PW_PTS_LAYER", unset = "occurrences")

POLY_DSN   <- Sys.getenv("PW_POLY_DSN",   unset = "data/processed/triassic_marine.gpkg")
POLY_LAYER <- Sys.getenv("PW_POLY_LAYER", unset = "triassic_marine")

OUT_GPKG <- Sys.getenv("PW_OUT_GPKG", unset = "data/processed/pbdb_points_vs_triassic.gpkg")
OUT_CSV  <- Sys.getenv("PW_OUT_CSV",  unset = "data/processed/pbdb_points_in_triassic_by_unit.csv")

# Optional: buffer polygons (in meters) to catch near-edge misses (0 = off)
POLY_BUFFER_M <- as.numeric(Sys.getenv("PW_POLY_BUFFER_M", unset = "500"))  # e.g., 50

# ============================== LOAD DATA ======================================
message("→ Reading points: ", PTS_DSN, " [", PTS_LAYER, "]")
pts <- st_read(PTS_DSN, layer = PTS_LAYER, quiet = TRUE)

message("→ Reading polygons: ", POLY_DSN, " [", POLY_LAYER, "]")
pol <- st_read(POLY_DSN, layer = POLY_LAYER, quiet = TRUE)

stopifnot(inherits(pts, "sf"), nrow(pts) > 0)
stopifnot(inherits(pol, "sf"), nrow(pol) > 0)

# Ensure common CRS (WGS84 lon/lat)
if (is.na(st_crs(pts))) st_crs(pts) <- 4326
if (is.na(st_crs(pol))) st_crs(pol) <- 4326
pts <- st_transform(pts, 4326)
pol <- st_transform(pol, 4326)

# Validity
pts <- st_make_valid(pts)
pol <- st_make_valid(pol)

# ============================== OPTIONAL BUFFER ================================
if (!is.na(POLY_BUFFER_M) && POLY_BUFFER_M > 0) {
  message("… buffering polygons by ", POLY_BUFFER_M, " m (EPSG:3857) for edge tolerance")
  pol_3857 <- st_transform(pol, 3857)
  pol_3857 <- st_buffer(pol_3857, dist = POLY_BUFFER_M)
  pol <- st_transform(pol_3857, 4326)
}

# ============================== ADD POLY ID ====================================
keep_cols <- intersect(c("name","geologicHi","lithology","descriptio","unit_name","age_txt","lith_txt"), names(pol))
pol <- pol |>
  mutate(poly_id = dplyr::row_number()) |>
  select(poly_id, any_of(keep_cols), dplyr::everything())

# ============================== SPATIAL JOIN ===================================
hits <- st_join(pts, pol, join = st_within, left = TRUE)

# ============================== SUMMARIES ======================================
n_in  <- sum(!is.na(hits$poly_id))
n_all <- nrow(hits)
message("PBDB points inside filtered polygons: ", n_in, " / ", n_all)

cat("\nInside/Outside counts:\n")
print(
  hits |>
    st_drop_geometry() |>
    mutate(in_poly = !is.na(poly_id)) |>
    count(in_poly)
)

# ============================== UNIT LABELS (no NSE, no do.call) ===============
# Build a robust, NA/blank-safe label with priority:
#    name > unit_name > geologicHi > age_txt > lithology > lith_txt > poly_id
inside_df <- hits |>
  st_drop_geometry() |>
  filter(!is.na(poly_id)) |>
  mutate(unit_label = as.character(poly_id))  # fallback

# helper to overwrite with a candidate column if present and non-empty
overwrite_with <- function(df, col) {
  if (!(col %in% names(df))) return(df)
  v <- df[[col]]
  v <- if (is.character(v)) trimws(v) else v
  df$unit_label <- ifelse(!is.na(v) & v != "", as.character(v), df$unit_label)
  df
}

# apply in increasing priority so later calls win
for (col in c("lith_txt","lithology","age_txt","geologicHi","unit_name","name")) {
  inside_df <- overwrite_with(inside_df, col)
}

top_tbl <- inside_df |>
  count(unit_label, sort = TRUE)

cat("\nTop formations/units intersected (up to 20):\n")
print(as.data.frame(dplyr::slice_head(top_tbl, n = 20)), row.names = FALSE)

# ============================== EXPORTS ========================================
# 1) CSV of counts by unit/label
readr::write_csv(top_tbl, OUT_CSV)
message("✓ wrote ", OUT_CSV)

# 2) GPKG: inside vs outside layers for QGIS
if (file.exists(OUT_GPKG)) unlink(OUT_GPKG)
st_write(hits |> filter(!is.na(poly_id)), OUT_GPKG, layer = "points_in_triassic",    quiet = TRUE)
st_write(hits |> filter( is.na(poly_id)), OUT_GPKG, layer = "points_outside_triassic", quiet = TRUE, append = TRUE)
message("✓ wrote ", OUT_GPKG, " [layers: points_in_triassic, points_outside_triassic]")

# 3) Append a light copy of polygons used (context)
st_write(pol |> select(poly_id, any_of(keep_cols)), OUT_GPKG, layer = "triassic_polys_used", quiet = TRUE, append = TRUE)
message("✓ appended polygons to ", OUT_GPKG, " [layer: triassic_polys_used]")

cat("\nDone.\n")
