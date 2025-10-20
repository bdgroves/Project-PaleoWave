#!/usr/bin/env Rscript
# Project PaleoWave — geology field probe (diagnose column names & contents)

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(readr)
})

# -------- config (points to your zipped shapefile) --------
IN_ZIP <- "data/raw/geology/nevada_geology.zip"  # adjust if needed

# -------- unzip if needed; find the .shp --------
norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
zip_in  <- norm(IN_ZIP)
unz_dir <- file.path(dirname(zip_in), paste0(tools::file_path_sans_ext(basename(zip_in)), "_unzipped"))
if (!dir.exists(unz_dir)) {
  dir.create(unz_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zip_in, exdir = unz_dir)
}
shps <- list.files(unz_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
if (!length(shps)) stop("No .shp found in: ", unz_dir)
if (length(shps) > 1) message("Multiple .shp files found; using first: ", basename(shps[1]))
SHP <- shps[1]

# -------- read layer --------
g <- sf::st_read(SHP, quiet = TRUE)
cat("\nRows:", nrow(g), "\n")
cat("CRS :", sf::st_crs(g)$input, "\n\n")

# column names
cat("Column names (", length(names(g)), "):\n", paste(names(g), collapse = ", "), "\n\n", sep="")

# peek first few rows (drop geometry)
cat("Sample rows (first 5):\n")
print(utils::head(sf::st_drop_geometry(g), 5))

# -------- scan character columns for likely fields --------
is_char <- sapply(g, function(x) inherits(x, "character") || inherits(x, "factor"))
chars   <- names(g)[is_char]

# helper normalize
norm_chr <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  str_squish(tolower(x))
}

tokens_tri  <- "(triassic|anisian|ladinian|carnian|norian|rhaetian|lower triassic|middle triassic|upper triassic)"
tokens_form <- "(luning|gabbs|prida|star peak)"
tokens_lith <- "(limestone|dolomite|carbonate|wackestone|packstone|grainstone|boundstone|mudstone|chert|shale|siltstone|reef|marine|shelf|subtidal|basinal|pelagic)"

score_col <- function(col) {
  v <- norm_chr(g[[col]])
  c(
    n_tri  = sum(str_detect(v, tokens_tri)),
    n_form = sum(str_detect(v, tokens_form)),
    n_lith = sum(str_detect(v, tokens_lith))
  )
}

sc <- lapply(chars, score_col)
sc <- do.call(rbind, sc)
sc <- tibble::as_tibble(sc, rownames = "column") %>%
  arrange(desc(n_tri + n_form + n_lith))

cat("\nHeuristic hit counts per character column (top 15):\n")
print(utils::head(sc, 15))

# also write a quick preview CSV of character columns (first 200 rows)
preview <- sf::st_drop_geometry(g)[, chars, drop = FALSE]
preview <- head(preview, 200)
readr::write_csv(preview, "data/processed/geology_field_preview.csv")
cat("\n✓ Wrote: data/processed/geology_field_preview.csv (first 200 rows of text fields)\n")
