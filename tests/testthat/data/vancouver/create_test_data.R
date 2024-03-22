# NOTE: This is from the Vancouver 2022 dataset

source("G:/dh_vancouver2022/R/van2022_3_data.R")

# Choose a tile
tile_name <- 'R19C27'

# Get tile from tile scheme
tile <- misterRS:::.get_tilescheme()[tile_name]

# Read in LAS data
las <- misterRS:::.read_las_tile(las_cat, tile = tile, select = "xyzciRGB")

# Get ortho, DEM, seg
ortho <- terra::rast(misterRS:::.get_rsds_tilepaths(rs$ortho)[tile_name])
dem   <- terra::rast(misterRS:::.get_rsds_tilepaths(rs$dem)[tile_name])
segs  <- terra::rast(misterRS:::.get_rsds_tilepaths(rs$rufsegsras)[tile_name])

# Create extent of AOI
xt <- terra::ext(493645, 493645 +30, 5453425, 5453424 + 30)
xt_sf <- sf::st_as_sf(terra::as.polygons(xt))
sf::st_crs(xt_sf) <- lidR::crs(las)

# Crop
las_sub   <- lidR::clip_roi(las, xt_sf)
ortho_sub <- terra::crop(ortho, xt)
dem_sub   <- terra::crop(dem, xt)
segs_sub  <- terra::crop(segs, xt)



setwd("D:/Projects/libraries/misterRS/tests/testthat/test_segs")

terra::writeRaster(ortho_sub, "vancouver_ortho.tif", datatype = "INT1U", overwrite =T)
terra::writeRaster(dem_sub, "vancouver_dem.tif")
terra::writeRaster(segs_sub, "vancouver_segs.tif", datatype = "INT2U", overwrite =T)

lidR::writeLAS(las_sub, "vancouver_las.laz")
