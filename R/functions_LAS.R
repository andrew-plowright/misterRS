
.readLAStile <- function(in_cat, tile, select, classes = NULL){

  buff <- tile[["buffs"]]

  # Get grid
  las_grid <- sp::SpatialPolygonsDataFrame(
    sp::SpatialPolygons(in_cat@polygons, proj4string = in_cat@proj4string),
    data.frame(ID = sapply(in_cat@polygons, slot, "ID"), stringsAsFactors = F)
  )

  # Reproject grid to tile
  las_grid <- sp::spTransform(las_grid, buff@proj4string)

  # Get intersection between RSDS tile and LAS catalog
  las_tiles <- las_grid[buff,]

  if(length(las_tiles) == 0) return(NULL)

  # Get LAS files
  las_files <- in_cat@data[as.integer(las_tiles[["ID"]]), "filename"]

  if(any(!file.exists(las_files))) stop("Missing LAS files")

  # Create extent filter from buffer extent
  buff_xt   <- raster::extent(buff)
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
  DEM <- raster::raster(DEMpath)

  # Remove points that aren't on DEM
  inLAS <- lidR::merge_spatial(inLAS, !is.na(DEM), attribute = "onDEM")
  inLAS <- lidR::filter_poi(inLAS, onDEM == 1)

  # No LAS points over DEM file
  if(lidR::is.empty(inLAS)) return(NULL)

  # Normalize
  inLAS <- lidR::normalize_height(inLAS, DEM)

  # Filter LAS by height
  inLAS <- lidR::filter_poi(inLAS, Z <= zMax & Z > zMin)

  # No LAS points within min/max Z bounds over DEM file
  if(lidR::is.empty(inLAS)) return(NULL)

  return(inLAS)

}




