#!/usr/bin/env Rscript
# Project PaleoWave — PBDB Harvester (fixed rename + clean CSV)
# Pulls Ichthyosauria occurrences (Triassic, Nevada), cleans fields,
# writes a tidy CSV + GPKG.

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(stringr)
  library(sf)
  library(readr)
  library(tidyr)
})

# ----------------------------- Config -----------------------------
TAXON      <- Sys.getenv("PW_TAXON",    unset = "Ichthyosauria")
INTERVAL   <- Sys.getenv("PW_INTERVAL", unset = "Triassic")
COUNTRY    <- Sys.getenv("PW_COUNTRY",  unset = "US")
STATE      <- Sys.getenv("PW_STATE",    unset = "Nevada")

OUT_DIR    <- "data/processed"
CSV_OUT    <- file.path(OUT_DIR, "pbdb_ichthyosaurs_nv.csv")
GPKG_OUT   <- file.path(OUT_DIR, "pbdb_ichthyosaurs_nv.gpkg")
GPKG_LAYER <- "occurrences"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ----------------------------- Helpers -----------------------------
coalesce_first <- function(cands, nm) {
  hit <- intersect(cands, nm)
  if (length(hit)) hit[1] else NA_character_
}
msg_top <- function(df, col, n=10) {
  if (!col %in% names(df)) return(invisible())
  cat("\nTop ", n, " by ", col, ":\n", sep="")
  print(df %>% count(.data[[col]], sort = TRUE) %>% head(n))
}

# ----------------------------- 1) Fetch -----------------------------
base_url <- "https://paleobiodb.org/data1.2/occs/list.json"
url <- paste0(
  base_url, "?",
  "base_name=", URLencode(TAXON, reserved = TRUE),
  "&interval=", URLencode(INTERVAL, reserved = TRUE),
  "&cc=", URLencode(COUNTRY, reserved = TRUE),
  "&state=", URLencode(STATE, reserved = TRUE),
  "&show=coords,loc,strat,lith,env,time,id,ref"
)
message("→ Fetching PBDB occurrences: ", url)
raw <- fromJSON(url)
stopifnot(!is.null(raw$records), NROW(raw$records) > 0)
occ <- tibble::as_tibble(raw$records)
message("Fetched records: ", nrow(occ))

# ----------------------------- 2) Rename shortcodes → readable -----------------------------
# Map: new_name = old_name
rename_map <- c(
  occurrence_no   = "oid",
  collection_no   = "cid",
  identified_name = "idn",
  taxon_name      = "tna",
  taxon_rank      = "rnk",
  taxon_id        = "tid",
  early_interval  = "oei",
  max_ma          = "eag",
  min_ma          = "lag",
  reference_no    = "rid",
  country         = "cc2",
  state           = "stp",
  county          = "cny",
  coord_precision = "prc",
  altitude        = "altv",
  altitude_unit   = "altu",
  geog_scale      = "gsc",
  geog_comments   = "ggc",
  formation       = "sfm",
  lithology1      = "lt1",
  environment     = "env",
  collection_ix   = "cxi",
  member          = "smb",
  lith_desc       = "ldc",
  lithify1        = "lf1",
  lithomod1       = "lm1",
  lithology2      = "lt2",
  lithify2        = "lf2",
  stage           = "oli",
  group_name      = "sgr",
  taxon_detail    = "tdf",
  identifier_id   = "iid",
  identifier_role = "idr"
)

# Helper to safely rename using the map (old -> new) only when present
rename_from_map <- function(df, map) {
  old <- unname(map); new <- names(map)
  present <- old %in% names(df)
  names(df)[match(old[present], names(df))] <- new[present]
  df
}

occ <- occ %>%
  # ensure coordinate fields exist first
  rename(
    lat = !!coalesce_first(c("lat","latitude"), names(.)),
    lng = !!coalesce_first(c("lng","longitude"), names(.))
  )

# apply our safe renamer
occ <- rename_from_map(occ, rename_map)


# ----------------------------- 3) Clean + derive -----------------------------
occ <- occ %>%
  mutate(
    lat = suppressWarnings(as.numeric(lat)),
    lng = suppressWarnings(as.numeric(lng))
  ) %>%
  filter(!is.na(lat), !is.na(lng))

if ("lithology1" %in% names(occ)) {
  occ$lithology1 <- str_remove_all(occ$lithology1, '^"+|"+$')
}

# Derive convenience field
if (!"interval" %in% names(occ)) {
  occ$interval <- occ$early_interval %||% NA_character_
}

# ----------------------------- 4) Reorder + drop all-NA columns -----------------------------
preferred_order <- c(
  "occurrence_no","collection_no","taxon_name","identified_name","taxon_rank",
  "early_interval","stage","max_ma","min_ma",
  "formation","member","group_name","lithology1","lithology2","environment",
  "lat","lng","country","state","county",
  "reference_no","geog_scale","geog_comments","lith_desc",
  "coord_precision","altitude","altitude_unit",
  "taxon_id","taxon_detail","identifier_id","identifier_role","collection_ix",
  "interval"
)

# Keep preferred columns that exist, then the rest
occ <- occ %>% dplyr::select(any_of(preferred_order), dplyr::everything())

# Drop columns that are entirely NA or empty strings
all_na_or_empty <- function(x) all(is.na(x) | (is.character(x) & trimws(x) == ""))
occ <- occ[, !vapply(occ, all_na_or_empty, logical(1)), drop = FALSE]

# Optional: strip "occ:" and "col:" prefixes (comment out if you want to keep them)
if ("occurrence_no" %in% names(occ)) occ$occurrence_no <- sub("^occ:", "", occ$occurrence_no)
if ("collection_no" %in% names(occ)) occ$collection_no <- sub("^col:", "", occ$collection_no)
if ("reference_no"  %in% names(occ)) occ$reference_no  <- sub("^ref:", "", occ$reference_no)

# ----------------------------- 5) Safe dedupe -----------------------------
if ("occurrence_no" %in% names(occ) && any(!is.na(occ$occurrence_no) & occ$occurrence_no != "")) {
  n_before <- nrow(occ)
  occ <- occ %>% distinct(occurrence_no, .keep_all = TRUE)
  message("Deduped by occurrence_no: ", n_before, " → ", nrow(occ))
} else {
  n_before <- nrow(occ)
  keys <- intersect(c("lat","lng","taxon_name","reference_no","early_interval"), names(occ))
  occ <- occ %>% distinct(across(all_of(keys)), .keep_all = TRUE)
  message("Deduped by lat/lng + taxon/ref/interval: ", n_before, " → ", nrow(occ))
}

# ----------------------------- 6) Write CSV (tidy types) -----------------------------
# Convert logical columns with some non-NA to proper types; write NA as blank
occ_out <- occ %>%
  mutate(across(where(is.logical), as.character))  # avoid readr inferring all-NA logical

write_csv(occ_out, CSV_OUT, na = "")
message("✓ CSV written: ", CSV_OUT)

# ----------------------------- 7) Write GeoPackage -----------------------------
occ_sf <- st_as_sf(occ, coords = c("lng","lat"), crs = 4326, remove = FALSE)
if (file.exists(GPKG_OUT)) unlink(GPKG_OUT)
st_write(occ_sf, GPKG_OUT, layer = GPKG_LAYER, quiet = TRUE)
message("✓ GPKG written: ", GPKG_OUT, " (layer='", GPKG_LAYER, "')")

# ----------------------------- 8) Summary -----------------------------
message("\n— PaleoWave PBDB Harvest —")
message("Records (final): ", nrow(occ))
msg_top(occ, "formation")
msg_top(occ, "member")
msg_top(occ, "environment")
