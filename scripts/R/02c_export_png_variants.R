#!/usr/bin/env Rscript
# Project PaleoWave — Export simple PNGs (small points, no tiles)

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(tigris)      # counties
  options(tigris_use_cache = TRUE)
})

IN_GPKG <- "data/processed/pbdb_ichthyosaurs_nv.gpkg"
LAYER   <- "occurrences"
OUT_DIR <- "data/outputs"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# --- Load data ---
pts <- sf::st_read(IN_GPKG, layer = LAYER, quiet = TRUE)
stopifnot(nrow(pts) > 0)
if (is.na(sf::st_crs(pts))) sf::st_crs(pts) <- 4326
pts <- sf::st_transform(pts, 4326)

nv_counties <- tigris::counties(state = "NV", year = 2023, cb = TRUE, progress_bar = FALSE) |>
  sf::st_transform(4326)

# Plot extent (pad a bit)
bb <- sf::st_bbox(nv_counties)
pad <- 0.35
xlim <- c(bb["xmin"] - pad, bb["xmax"] + pad)
ylim <- c(bb["ymin"] - pad, bb["ymax"] + pad)

# --- Styling (smaller, simple points) ---
pt_size   <- 1.3  # small & clean
pt_fill   <- "black"
pt_outline<- "white"
title_txt <- "PBDB Ichthyosaurs — Nevada"

base_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank()
  )

# 1) Points only (white background + county outlines)
p1 <- ggplot() +
  geom_sf(data = nv_counties, fill = NA, color = "grey60", linewidth = 0.35) +
  geom_sf(data = pts, shape = 21, fill = pt_fill, color = pt_outline, size = pt_size, alpha = 0.95) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  base_theme +
  ggtitle(title_txt)

ggsave(file.path(OUT_DIR, "pbdb_points_points_only.png"), p1, width = 10, height = 7, dpi = 220)

# 2) Counties only (subtle light fill + same points)
p2 <- ggplot() +
  geom_sf(data = nv_counties, fill = "#f5f5f5", color = "grey60", linewidth = 0.35) +
  geom_sf(data = pts, shape = 21, fill = pt_fill, color = pt_outline, size = pt_size, alpha = 0.95) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  base_theme +
  ggtitle(title_txt)

ggsave(file.path(OUT_DIR, "pbdb_points_counties_only.png"), p2, width = 10, height = 7, dpi = 220)

message("✓ Saved:",
        "\n  - data/outputs/pbdb_points_points_only.png",
        "\n  - data/outputs/pbdb_points_counties_only.png",
        "\nPoints plotted: ", nrow(pts))
