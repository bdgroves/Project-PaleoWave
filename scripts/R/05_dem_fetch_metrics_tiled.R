#!/usr/bin/env Rscript
# Project PaleoWave — 05_dem_fetch_metrics_tiled.R (robust: retries + resample)
# Tile-aware DEM fetch with elevatr → resample → mosaic → terrain metrics (terra).

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
})

# =========================== CONFIG (env overrides) =============================
PTS_DSN      <- Sys.getenv("PW_PTS_DSN",      unset = "data/processed/pbdb_ichthyosaurs_nv.gpkg")
PTS_LAYER    <- Sys.getenv("PW_PTS_LAYER",    unset = "occurrences")

POLY_DSN     <- Sys.getenv("PW_POLY_DSN",     unset = "data/processed/triassic_marine.gpkg")
POLY_LAYER   <- Sys.getenv("PW_POLY_LAYER",   unset = "triassic_marine")

OUT_DIR_DEF  <- "P:/GISTeam/Brooks/dem"
OUT_DIR_FALL <- "data/processed/dem"
OUT_DIR      <- Sys.getenv("PW_DEM_OUTDIR", unset = OUT_DIR_DEF)

AOI_MODE     <- Sys.getenv("PW_AOI_MODE",     unset = "polys")     # "polys" | "points"
BUFFER_KM    <- as.numeric(Sys.getenv("PW_BUFFER_KM", unset = "5"))
ELEV_ZOOM    <- as.integer(Sys.getenv("PW_ELEV_Z",   unset = "12"))  # 12≈30–40m
MASK_TO_POLY <- tolower(Sys.getenv("PW_MASK_TO_POLYS", unset = "true")) %in% c("true","1","yes")

# tiling controls (smaller files by default)
MAX_TILE_MB  <- as.numeric(Sys.getenv("PW_MAX_TILE_MB",  unset = "120"))
SLEEP_SEC    <- as.numeric(Sys.getenv("PW_TILE_SLEEP_S", unset = "1"))
GRID_MAX     <- as.integer(Sys.getenv("PW_GRID_MAX",     unset = "16"))
RETRIES      <- as.integer(Sys.getenv("PW_RETRIES",      unset = "4"))   # retries per tile

# local temp workspace (fast scratch on C:)
LOCAL_TEMP   <- Sys.getenv("PW_TEMP_DIR", unset = file.path(tempdir(), "paleo_dem_cache"))
dir.create(LOCAL_TEMP, recursive = TRUE, showWarnings = FALSE)

# Force temp to LOCAL_TEMP (helps curl write to disk)
old_tmpdir <- list(TMPDIR = Sys.getenv("TMPDIR"), TEMP = Sys.getenv("TEMP"), TMP = Sys.getenv("TMP"))
Sys.setenv(TMPDIR = LOCAL_TEMP, TEMP = LOCAL_TEMP, TMP = LOCAL_TEMP)
on.exit({
  Sys.setenv(TMPDIR = old_tmpdir$TMPDIR, TEMP = old_tmpdir$TEMP, TMP = old_tmpdir$TMP)
}, add = TRUE)

# Try to make OUT_DIR; fallback if needed
ok <- try(dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE), silent = TRUE)
if (!dir.exists(OUT_DIR)) {
  message("… could not create '", OUT_DIR, "'. Falling back to '", OUT_DIR_FALL, "'.")
  OUT_DIR <- OUT_DIR_FALL
  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
}
message("→ Output directory: ", OUT_DIR)

terraOptions(memfrac = 0.8, tempdir = LOCAL_TEMP, progress = 1)

# =============================== DEPENDENCY ====================================
if (!requireNamespace("elevatr", quietly = TRUE)) {
  message("Installing 'elevatr' (one-time)…")
  install.packages("elevatr", repos = "https://cloud.r-project.org")
}
library(elevatr)

# ================================ LOAD DATA ====================================
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

# ================================ BUILD AOI ====================================
if (AOI_MODE == "points") {
  message("→ AOI from PBDB points + ", BUFFER_KM, " km buffer")
  aoi_ll <- sf::st_union(pts)
} else {
  message("→ AOI from Triassic polygons + ", BUFFER_KM, " km buffer")
  aoi_ll <- sf::st_union(pol)
}
aoi <- aoi_ll |> sf::st_transform(3857) |> sf::st_buffer(BUFFER_KM * 1000) |> sf::st_transform(4326)
aoi <- sf::st_as_sf(aoi)

# =================== SIZE ESTIMATOR & TILE GRID BUILDER ========================
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
  est_total <- estimate_mb(sf::st_bbox(aoi_geom), z)
  parts   <- ceiling(est_total / max_mb)
  n_side  <- max(1, min(grid_max, ceiling(sqrt(parts))))
  n_side
}
n_side <- pick_grid(aoi, ELEV_ZOOM, MAX_TILE_MB, GRID_MAX)
message("→ Tiling AOI: ", n_side, " x ", n_side, " (≤~", MAX_TILE_MB, " MB/tile @ z=", ELEV_ZOOM, ")")

grid <- sf::st_make_grid(aoi, n = c(n_side, n_side), what = "polygons", square = TRUE) |>
  sf::st_as_sf()
grid$intersects <- lengths(sf::st_intersects(grid, aoi)) > 0
grid <- dplyr::filter(grid, intersects)
grid$intersects <- NULL
grid$tile_id <- seq_len(nrow(grid))
message("→ Tiles to fetch: ", nrow(grid))

# ========================= FETCH PER TILE (with retries) =======================
fetch_tile <- function(gpad, z, retries = 3, sleep_s = 1) {
  for (k in seq_len(retries)) {
    r <- try(
      elevatr::get_elev_raster(
        locations = gpad, z = z, clip = "locations",
        override_size_check = TRUE
      ),
      silent = TRUE
    )
    if (!inherits(r, "try-error")) return(terra::rast(r))
    # backoff
    wt <- sleep_s * (2^(k-1))
    message(sprintf("      ! fetch failed (attempt %d/%d); retrying in %.1fs …", k, retries, wt))
    Sys.sleep(wt)
  }
  return(NULL)
}

tile_paths <- character(0)
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  g_pad <- g |> sf::st_transform(3857) |> sf::st_buffer(200) |> sf::st_transform(4326)
  est_mb <- estimate_mb(sf::st_bbox(g_pad), ELEV_ZOOM)
  message(sprintf("   - tile %d/%d (est ~%.1f MB)", i, nrow(grid), est_mb))
  
  r_t <- fetch_tile(g_pad, ELEV_ZOOM, retries = RETRIES, sleep_s = SLEEP_SEC)
  if (is.null(r_t)) {
    warning("      ! tile ", i, " permanently failed; skipping.")
    next
  }
  # write each tile to fast local temp, compressed
  t_path <- file.path(LOCAL_TEMP, sprintf("tile_%03d.tif", i))
  terra::writeRaster(
    r_t, t_path, overwrite = TRUE,
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
  tile_paths <- c(tile_paths, t_path)
}

stopifnot(length(tile_paths) > 0)
message("→ Mosaicking ", length(tile_paths), " tiles")

# load tiles
r_list <- lapply(tile_paths, terra::rast)

# RESAMPLE all tiles to the first tile's grid (fixes "resolution does not match")
template <- r_list[[1]]
r_list_rs <- lapply(r_list, function(x) {
  if (!all(terra::res(x) == terra::res(template)) ||
      !all(round(terra::ext(x), 6) == round(terra::ext(template), 6)) ||
      terra::nlyr(x) != terra::nlyr(template) ||
      sf::st_crs(x) != sf::st_crs(template)) {
    terra::resample(x, template, method = "bilinear")
  } else {
    x
  }
})

# mosaic
dem <- do.call(terra::mosaic, r_list_rs)
names(dem) <- "elev_m"

# crop/mask to AOI
dem <- terra::crop(dem, terra::vect(aoi))
dem <- terra::mask(dem,  terra::vect(aoi))

# ============================= TERRAIN METRICS =================================
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

# ============================== WRITE RASTERS ==================================
writeRasterSafe <- function(r, path) {
  terra::writeRaster(
    r, path, overwrite = TRUE,
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
  message("✓ wrote ", path)
}

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
writeRasterSafe(dem,       file.path(OUT_DIR, "dem_m.tif"))
writeRasterSafe(slope_deg, file.path(OUT_DIR, "slope_deg.tif"))
writeRasterSafe(aspect_deg,file.path(OUT_DIR, "aspect_deg.tif"))
writeRasterSafe(tri,       file.path(OUT_DIR, "tri.tif"))
writeRasterSafe(tpi,       file.path(OUT_DIR, "tpi.tif"))
writeRasterSafe(rough,     file.path(OUT_DIR, "roughness.tif"))

# ========================= POLYGON-LEVEL SUMMARIES =============================
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
message("\nDone. Tiled DEM + metrics written to: ", OUT_DIR, "\n")
