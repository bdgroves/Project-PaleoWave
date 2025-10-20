#!/usr/bin/env Rscript
# Project PaleoWave — Geology ingestion & Triassic-marine filter (NBMG 500k tuned)

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(tigris)
  options(tigris_use_cache = TRUE)
})

# ============================== CONFIG =========================================
# Your zipped shapefile path:
IN_PATH  <- Sys.getenv("PW_GEO_IN",    unset = "data/raw/geology/nevada_geology.zip")
IN_LAYER <- Sys.getenv("PW_GEO_LAYER", unset = NA_character_)  # not used for SHP/ZIP

# Column overrides based on your probe results:
#   unit / formation: name  (+ backup: descriptio)
#   age:              geologicHi
#   lithology:        lithology (+ backup: descriptio)
UNIT_COL  <- Sys.getenv("PW_GEO_UNIT", unset = "name")
AGE_COL   <- Sys.getenv("PW_GEO_AGE",  unset = "geologicHi")
LITH_COL  <- Sys.getenv("PW_GEO_LITH", unset = "lithology")
DESC_COL  <- Sys.getenv("PW_GEO_DESC", unset = "descriptio")  # text field with lots of info

OUT_FULL <- "data/processed/triassic_marine.gpkg"
OUT_SLIM <- "data/processed/triassic_marine_slim.gpkg"
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# ============================== HELPERS =========================================
norm_path <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)

norm_chr <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  stringr::str_squish(tolower(x))
}

read_geology <- function(path, layer = NA_character_) {
  path <- norm_path(path)
  # ZIP → unzip & read first .shp
  if (grepl("\\.zip$", tolower(path))) {
    message("→ Detected ZIP: ", path)
    zip_base <- tools::file_path_sans_ext(basename(path))
    out_dir  <- file.path(dirname(path), paste0(zip_base, "_unzipped"))
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    utils::unzip(path, exdir = out_dir)
    shp <- list.files(out_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
    if (!length(shp)) stop("No .shp found inside ZIP at: ", path)
    if (length(shp) > 1) message("… multiple .shp files found; taking first: ", basename(shp[1]))
    return(sf::st_read(shp[1], quiet = TRUE))
  }
  if (grepl("\\.(shp|gpkg)$", tolower(path))) return(sf::st_read(path, quiet = TRUE))
  if (grepl("\\.gdb$", tolower(path))) {
    if (is.na(layer)) stop("GDB requires PW_GEO_LAYER to be set.")
    return(sf::st_read(path, layer = layer, quiet = TRUE))
  }
  stop("Unsupported input: ", path)
}

# ============================== LOAD & CLIP =====================================
message("→ Reading geology from: ", IN_PATH, if (!is.na(IN_LAYER)) paste0(" [layer=", IN_LAYER, "]") else "")
g <- read_geology(IN_PATH, IN_LAYER)
stopifnot(inherits(g, "sf"), nrow(g) > 0)

# Ensure CRS → WGS84
if (is.na(sf::st_crs(g))) {
  message("No CRS detected; assuming EPSG:3857 (Pseudo-Mercator) per probe and transforming to 4326.")
  sf::st_crs(g) <- 3857
}
g <- sf::st_transform(g, 4326)

# Nevada mask (counties union)
nv_counties <- tigris::counties(state = "NV", year = 2023, cb = TRUE, progress_bar = FALSE) |>
  sf::st_transform(4326)
nv_outline  <- sf::st_union(nv_counties)

# quick crop then precise clip
g <- sf::st_crop(g, sf::st_bbox(nv_outline))
g <- suppressWarnings(sf::st_intersection(sf::st_make_valid(g), sf::st_make_valid(nv_outline)))

# ============================== PREP TEXT FIELDS ================================
# Make safe text vectors even if a column is missing
get_chr <- function(df, nm) if (nm %in% names(df)) norm_chr(df[[nm]]) else rep("", nrow(df))

u_txt <- get_chr(g, UNIT_COL)     # name
a_txt <- get_chr(g, AGE_COL)      # geologicHi
l_txt <- get_chr(g, LITH_COL)     # lithology
d_txt <- get_chr(g, DESC_COL)     # descriptio (rich free text)

# ============================== FILTER LOGIC ===================================
# Triassic tokens (and stages):
tri_rx <- "(\\btriassic\\b|anisian|ladinian|carnian|norian|rhaetian|lower triassic|middle triassic|upper triassic)"
is_tri <- grepl(tri_rx, a_txt) | grepl(tri_rx, u_txt) | grepl(tri_rx, d_txt)

# Marine + carbonate/shelf indicators (search lithology + description + name)
marine_rx <- paste(c(
  "marine","shelf","subtidal","basinal","pelagic","reef",
  "limestone","dolomite","carbonate","wackestone","packstone",
  "grainstone","boundstone","mudstone","chert","shale","siltstone"
), collapse = "|")
is_marine <- grepl(marine_rx, l_txt) | grepl(marine_rx, d_txt) | grepl(marine_rx, u_txt)

# Known NV ichthyosaur-bearing units (whitelist) — check all text fields
form_whitelist_rx <- "(\\bluning\\b|\\bgabbs\\b|\\bprida\\b|\\bstar\\s+peak\\b)"
is_whitelist <- grepl(form_whitelist_rx, u_txt) | grepl(form_whitelist_rx, d_txt)

keep <- (is_tri & is_marine) | is_whitelist
gm <- g[keep, ]
message("Filtered polygons: ", nrow(gm), " / ", nrow(g))

# ============================== OUTPUTS ========================================
# Full attributes
if (file.exists(OUT_FULL)) unlink(OUT_FULL)
sf::st_write(gm, OUT_FULL, layer = "triassic_marine", quiet = TRUE)
message("✓ wrote ", OUT_FULL, " [layer=triassic_marine]")

# Slim attributes (unit/age/lith only — but keep what exists)
gm_slim <- gm %>%
  mutate(
    unit_name = if (UNIT_COL %in% names(gm)) .data[[UNIT_COL]] else NA_character_,
    age_txt   = if (AGE_COL  %in% names(gm)) .data[[AGE_COL]]  else NA_character_,
    lith_txt  = if (LITH_COL %in% names(gm)) .data[[LITH_COL]] else NA_character_
  ) %>%
  dplyr::select(any_of(c("unit_name","age_txt","lith_txt")), geometry)

if (file.exists(OUT_SLIM)) unlink(OUT_SLIM)
sf::st_write(gm_slim, OUT_SLIM, layer = "triassic_marine_slim", quiet = TRUE)
message("✓ wrote ", OUT_SLIM, " [layer=triassic_marine_slim]")

# Quick console peek
if ("unit_name" %in% names(gm_slim)) {
  message("\nTop units kept:")
  print(gm_slim |>
          sf::st_drop_geometry() |>
          count(unit_name, sort = TRUE) |>
          head(12))
}
