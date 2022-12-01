# Input ----

# Variables
in_tiles <- c(
  R1C1 = "R8C38",
  R1C2 = "R8C39",
  R2C1 = "R9C38",
  R2C2 = "R9C39"
)

crs_sf <- 26917
crs_sp <- "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"

res = 5

# Input paths
in_root_dir <- "H:/dh_vaughan2019"
in_dirs <- list(
  ortho = file.path(in_root_dir, "data_intermediate/ortho/2019/tiles"),
  ndsm  = file.path(in_root_dir, "data_intermediate/ndsm/tiles"),
  dem   = file.path(in_root_dir, "data_intermediate/dem/tiles"),
  las   = file.path(in_root_dir, "data_raw/lidar_2019")
)

# Output paths
out_data_names <- c("ortho", "ndsm", "dem", "las", "tilescheme")
out_root_dir   <- "D:/Projects/libraries/misterRS/test/testthat/test_data"
out_dirs       <- lapply(setNames(out_data_names,out_data_names), function(data_name) file.path(out_root_dir, data_name))
out_paths <- list(
  tilescheme_shp = file.path(out_dirs$tilescheme, "tile_scheme.shp"),
  tilescheme_rds = file.path(out_dirs$tilescheme, "tile_scheme.rds")
)

for(out_dir in out_dirs) dir.create(out_dir, showWarnings = FALSE)


# Tile Scheme ----

# Get maximum extent of selected tiles
ras_exts <- sapply(in_tiles, function(tile_name) as.vector(raster::extent(raster::raster(file.path(in_dirs$ndsm, paste0(tile_name, ".tif"))))))
ts_ext <- raster::extent(c(min(x[1,]), max(x[2,]), min(x[3,]), max(x[4,])))

# Generate tile scheme
ts_main <- TileManager::tileScheme(ts_ext, tiledim = 500, buffer = 40, origin = c(0,0), round = 1, removeEmpty = FALSE, bufferspill = FALSE, crs = crs_sp)

# Save
TileManager::tileSave(ts_main, out_paths$tilescheme_shp, overwrite = TRUE)
saveRDS(ts_main, out_paths$tilescheme_rds)

# Resample rasters ----

for(data_name in c("ortho", "ndsm", "dem")){
  for(tile_name in names(in_tiles)){

    in_path <- file.path(in_dirs[[data_name]], paste0(in_tiles[tile_name], ".tif"))
    out_path <- file.path(out_dirs[[data_name]], "tiles", paste0(tile_name, ".tif"))
    in_ras <- terra::rast(in_path)
    out_ras <- terra::aggregate(in_ras, fact = 20)
    terra::writeRaster(out_ras, out_path)

  }
}

# Subset LAS files ----

# Create grid for LAS that is offset from the ordinary tiles
ts_las <- TileManager::tileScheme(ts_ext + 50, tiledim = 750, buffer = 0, origin = c(-350,80), round = 1, removeEmpty = FALSE, bufferspill = FALSE)

# Get LAS files that intersect
ts_las_sf <- sf::st_as_sf(ts_las[["tiles"]])
in_cat <- lidR::catalog(in_dirs$las)
las_grid <- in_cat$geometry
lidR::st_crs(las_grid) <- sf::st_crs(ts_las_sf)
las_intrsc <- lengths(sf::st_intersects(las_grid, ts_las_sf)) > 0
las_files <- in_cat@data$filename[las_intrsc]

# Read LAS files and select random subset of points
for(tile_name in ts_las$tileName){

  # Out path
  out_file <- file.path(out_dirs$las, paste0("las_tile_", tile_name, ".laz"))

  # Set extent filter
  tile      <- ts_las[tile_name][["tiles"]]
  tile_xt   <- raster::extent(tile)
  tile_filt <- paste("-keep_xy", tile_xt@xmin, tile_xt@ymin, tile_xt@xmax, tile_xt@ymax)

  # Set random filter
  random_filt <- "-keep_random_fraction 0.01"

  # Read file
  in_las    <- lidR::readLAS(las_files, filter = paste(tile_filt, random_filt))

  lidR::writeLAS(in_las, out_file)
}
