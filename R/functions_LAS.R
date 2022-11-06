
.readLAStile <- function(in_cat, tile, select, classes = NULL){

  # Tile buffer
  buff_sf <- sf::st_as_sf(tile[["buffs"]])

  # LAS catalog geometry
  las_grid <- in_cat$geometry

  if(is.na(sf::st_crs(in_cat))) stop("Can't select LAS tiles since this LAS Catalog has no projection info")

  # Reproject grid to tile
  las_grid <- sf::st_transform(las_grid, sf::st_crs(buff_sf))

  # Get intersection between RSDS tile and LAS catalog
  las_intrsc <- lengths(sf::st_intersects(las_grid, buff_sf)) > 0

  if(all(!las_intrsc)) return(NULL)

  # Get LAS files
  las_files <- in_cat@data$filename[las_intrsc]

  if(any(!file.exists(las_files))) stop("Missing LAS files")

  # Create extent filter from buffer extent
  buff_xt   <- raster::extent(buff_sf)
  buff_filt <- paste("-keep_xy", buff_xt@xmin, buff_xt@ymin, buff_xt@xmax, buff_xt@ymax)

  # Create class filter
  class_filt <- if(!is.null(classes)) paste(c("-keep_class", classes), collapse = " ")

  # Read LAS files
  inLAS <- lidR::readLAS(las_files, select = select, filter = c(buff_filt, class_filt))

  if(lidR::is.empty(inLAS)) return(NULL) else return(inLAS)

}

.normalizeLAS <- function(inLAS, DEMpath, zMin, zMax){

  if(!file.exists(DEMpath)) stop("Could not find DEM file '", DEMpath, "'")

  # Read segments and DEM
  DEM <- terra::rast(DEMpath)

  # Remove points that aren't on DEM


  DEM_mask <- !is.na(DEM)
  inLAS <- lidR::merge_spatial(inLAS, DEM_mask,attribute = "onDEM")
  inLAS <- lidR::filter_poi(inLAS, onDEM == TRUE)

  # No LAS points over DEM file
  if(lidR::is.empty(inLAS)) return(NULL)

  # Normalize
  inLAS <- lidR::normalize_height(inLAS, DEM)

  # Filter LAS by height
  outLAS <- lidR::filter_poi(inLAS, Z <= zMax & Z > zMin)

  # No LAS points within min/max Z bounds over DEM file
  if(lidR::is.empty(outLAS)) return(NULL)


  return(outLAS)

}




