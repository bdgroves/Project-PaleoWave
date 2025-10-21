library(sf); library(dplyr)

pts  <- st_read("data/processed/pbdb_points_vs_triassic.gpkg", layer="points_outside_triassic", quiet=TRUE)
pol  <- st_read("data/processed/triassic_marine.gpkg", layer="triassic_marine", quiet=TRUE)

# Distance to nearest Triassic polygon edge (meters)
pts_m  <- st_transform(pts, 3857)
pol_m  <- st_transform(pol, 3857)
dists  <- st_distance(pts_m, pol_m)           # matrix [n_out x n_polys]
min_m  <- apply(dists, 1, min)                # nearest distance per point

out_d <- st_drop_geometry(pts) |>
  mutate(nearest_triassic_m = as.numeric(min_m)) |>
  arrange(nearest_triassic_m)

readr::write_csv(out_d, "data/processed/pbdb_outside_dist_to_triassic_m.csv")
