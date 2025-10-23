#!/usr/bin/env Rscript
# Project PaleoWave — 05_dem_fetch_metrics_tiled.R (geometry-safe)
# Tile-aware DEM fetch with elevatr → mosaic → terrain metrics (terra).

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
})

# ------------------------------- CONFIG ----------------------------------------
PTS_DSN      <- Sys.getenv("PW_PTS_DSN",      unset = "data/processed/pbdb_ichthyosaurs_nv.gpkg")
PTS_LAYER    <- Sys.getenv("PW_PTS_LAYER",    unset = "occurrences")

POLY_DSN     <- Sys.getenv("PW_POLY_DSN",     unset = "data/processed/triassic_marine.gpkg")
POLY_LAYER   <- Sys.getenv("PW_POLY_LAYER",   unset = "triassic_marine")

OUT_DIR      <- Sys.getenv("PW_DEM_OUTDIR",   unset = "data/processed/dem")
AOI_MODE     <- Sys.getenv("PW_AOI_MODE",     unset = "polys")     # "polys" | "points"
BUFFER_KM    <- as.numeric(Sys.getenv("PW_BUFFER_KM", unset = "10"))
ELEV_ZOOM    <- as.integer(Sys.getenv("PW_ELEV_Z",   unset = "13")) # 12≈30–40m, 13≈10–20m
MASK_TO_POLY <- tolower(Sys.getenv("PW_MASK_TO_POLYS", unset = "true")) %in% c("true","1","yes")

# tiling controls
MAX_TILE_MB  <- as.numeric(Sys.getenv("PW_MAX_TILE_MB",  unset = "250"))  # soft target per tile
SLEEP_SEC    <- as.numeric(Sys.getenv("PW_TILE_SLEEP_S", unset = "1"))    # throttle API
GRID_MAX     <- as.integer(Sys.getenv("PW_GRID_MAX",     unset = "12"))   # max tiles per side

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("elevatr", quietly = TRUE)) {
  message("Installing 'elevatr' (one-time)...")
  install.packages("elevatr", repos = "https://cloud.r-project.org")
}
library(elevatr)

# ------------------------------- LOAD DATA -------------------------------------
message("→ Reading points: ", PTS_DSN, " [", PTS_LAYER, "]")
pts <- sf::st_read(PTS_DSN, layer = PTS_LAYER, quiet = TRUE)

message("→ Reading polygons: ", POLY_DSN, " [", POLY_LAYER, "]")
pol <- sf::st_read(POLY_DSN, layer = POLY_LAYER, quiet = TRUE)

stopifnot(inherits(pts, "sf"), nrow(pts) > 0)
stopifnot(inherits(pol, "sf"), nrow(pol) > 0)

if (is.na(sf::st_crs(pts))) sf::st_crs(pts) <- 4326
if (is.na(sf::st_crs(pol))) sf::st_crs(pol) <- 4326
pts <- sf::st_transform(pts, 4326)
pol <- sf::st_transform(pol, 4326)

# ------------------------------- BUILD AOI -------------------------------------
if (AOI_MODE == "points") {
  message("→ AOI from PBDB points + ", BUFFER_KM, " km buffer")
  aoi_ll <- sf::st_union(pts)
} else {
  message("→ AOI from Triassic polygons + ", BUFFER_KM, " km buffer")
  aoi_ll <- sf::st_union(pol)
}
aoi <- aoi_ll |> sf::st_transform(3857) |> sf::st_buffer(BUFFER_KM * 1000) |> sf::st_transform(4326)
aoi <- sf::st_as_sf(aoi)

# -------------------- SIZE ESTIMATOR & GRID BUILDER ----------------------------
estimate_mb <- function(bbox_ll, z) {
  bb  <- sf::st_bbox(bbox_ll)
  bbm <- sf::st_as_sfc(bb) |> sf::st_transform(3857) |> sf::st_bbox()
  width_m  <- as.numeric(bbm["xmax"] - bbm["xmin"])
  height_m <- as.numeric(bbm["ymax"] - bbm["ymin"])
  lat0 <- (bb["ymin"] + bb["ymax"])/2
  res_m <- 156543.03392 * cos(lat0 * pi/180) / (2^z)  # m/px
  ncol  <- max(1, floor(width_m  / res_m))
  nrow  <- max(1, floor(height_m / res_m))
  bytes <- as.numeric(ncol) * as.numeric(nrow) * 4
  bytes / 1024^2
}

pick_grid <- function(aoi_geom, z, max_mb, grid_max) {
  aoi_bb <- sf::st_bbox(aoi_geom)
  est_total <- estimate_mb(aoi_bb, z)
  parts   <- ceiling(est_total / max_mb)
  n_side  <- max(1, min(grid_max, ceiling(sqrt(parts))))
  n_side
}

n_side <- pick_grid(aoi, ELEV_ZOOM, MAX_TILE_MB, GRID_MAX)
message("→ Tiling AOI: ", n_side, " x ", n_side, " (target ≤ ~", MAX_TILE_MB, " MB/tile at z=", ELEV_ZOOM, ")")

grid <- sf::st_make_grid(aoi, n = c(n_side, n_side), what = "polygons", square = TRUE) |>
  sf::st_as_sf()

# compute intersects OUTSIDE mutate(), then filter
int_mat <- sf::st_intersects(grid, aoi, sparse = FALSE)
grid$intersects <- as.logical(int_mat[, 1])
grid <- grid |> dplyr::filter(intersects)
grid$intersects <- NULL                # <- keep geometry; just drop helper col
grid$tile_id <- seq_len(nrow(grid))    # <- sequential ids

message("→ Tiles to fetch: ", nrow(grid))

# --------------------------- FETCH PER TILE + MOSAIC ---------------------------
tile_rasters <- list()
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  # small dilation to avoid seam gaps
  g_pad <- g |> sf::st_transform(3857) |> sf::st_buffer(200) |> sf::st_transform(4326)
  est_mb <- estimate_mb(sf::st_bbox(g_pad), ELEV_ZOOM)
  message(sprintf("   - tile %d/%d (est ~%.1f MB)", i, nrow(grid), est_mb))
  
  r <- try(
    elevatr::get_elev_raster(
      locations = g_pad,
      z = ELEV_ZOOM,
      clip = "locations",
      override_size_check = TRUE
    ),
    silent = TRUE
  )
  if (inherits(r, "try-error")) {
    warning("      ! fetch failed for tile ", i, "; skipping.")
    next
  }
  tile_rasters[[length(tile_rasters) + 1]] <- terra::rast(r)
  if (SLEEP_SEC > 0) Sys.sleep(SLEEP_SEC)
}

stopifnot(length(tile_rasters) > 0)
message("→ Mosaicking ", length(tile_rasters), " tiles")
dem <- tile_rasters[[1]]
if (length(tile_rasters) > 1) {
  dem <- do.call(terra::mosaic, tile_rasters)
}
names(dem) <- "elev_m"

# crop/mask to AOI
dem <- terra::crop(dem, terra::vect(aoi))
dem <- terra::mask(dem,  terra::vect(aoi))

# --------------------------- TERRAIN METRICS -----------------------------------
message("→ Computing terrain metrics (slope/aspect/TRI/TPI/roughness)")
slope_deg  <- terra::terrain(dem, v = "slope", unit = "degrees")
aspect_deg <- terra::terrain(dem, v = "aspect", unit = "degrees")
tri        <- terra::terrain(dem, v = "TRI")
tpi        <- terra::terrain(dem, v = "TPI")
rough      <- terra::terrain(dem, v = "roughness")

if (MASK_TO_POLY) {
  message("→ Masking rasters to Triassic polygons")
  pol_v <- terra::vect(pol)
  dem       <- terra::mask(dem,       pol_v)
  slope_deg <- terra::mask(slope_deg, pol_v)
  aspect_deg<- terra::mask(aspect_deg,pol_v)
  tri       <- terra::mask(tri,       pol_v)
  tpi       <- terra::mask(tpi,       pol_v)
  rough     <- terra::mask(rough,     pol_v)
}

# ------------------------------- WRITE RASTERS ---------------------------------
writeRasterSafe <- function(r, path) {
  terra::writeRaster(r, path, overwrite = TRUE)
  message("✓ wrote ", path)
}
writeRasterSafe(dem,       file.path(OUT_DIR, "dem_m.tif"))
writeRasterSafe(slope_deg, file.path(OUT_DIR, "slope_deg.tif"))
writeRasterSafe(aspect_deg,file.path(OUT_DIR, "aspect_deg.tif"))
writeRasterSafe(tri,       file.path(OUT_DIR, "tri.tif"))
writeRasterSafe(tpi,       file.path(OUT_DIR, "tpi.tif"))
writeRasterSafe(rough,     file.path(OUT_DIR, "roughness.tif"))

# -------------------------- POLYGON-LEVEL SUMMARIES ----------------------------
message("→ Summarizing metrics per polygon")
if (!("poly_id" %in% names(pol))) pol$poly_id <- dplyr::row_number()

summarize_r <- function(r, nm) {
  fun <- function(x) c(mean = mean(x, na.rm=TRUE),
                       p90  = as.numeric(stats::quantile(x, 0.90, na.rm=TRUE)))
  ex  <- terra::extract(r, terra::vect(pol), fun = fun, na.rm = TRUE)
  ex  <- as.data.frame(ex)
  names(ex) <- c("poly_id", paste0(nm, c("_mean","_p90")))
  ex
}

ex_slope <- summarize_r(slope_deg, "slope")
ex_tri   <- summarize_r(tri,       "tri")
ex_tpi   <- summarize_r(tpi,       "tpi")
ex_rough <- summarize_r(rough,     "rough")

stats_df <- ex_slope |>
  dplyr::left_join(ex_tri,   by = "poly_id") |>
  dplyr::left_join(ex_tpi,   by = "poly_id") |>
  dplyr::left_join(ex_rough, by = "poly_id")

pol_stats <- pol |> dplyr::left_join(stats_df, by = "poly_id")

csv_out  <- file.path(OUT_DIR, "triassic_poly_terrain_stats.csv")
gpkg_out <- file.path(OUT_DIR, "triassic_poly_terrain_stats.gpkg")
readr::write_csv(sf::st_drop_geometry(pol_stats), csv_out)
if (file.exists(gpkg_out)) unlink(gpkg_out)
sf::st_write(pol_stats, gpkg_out, layer = "triassic_poly_terrain_stats", quiet = TRUE)

message("✓ wrote ", csv_out)
message("✓ wrote ", gpkg_out, " [layer: triassic_poly_terrain_stats]")
message("\nDone. Tiled DEM + metrics ready for suitability scoring.\n")
