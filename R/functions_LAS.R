
.readLAStile <- function(LAS_RSDS, tile, select, classes = NULL){

  buff <- tile[["buffs"]]

  # Get intersection between LAS dataset and selected tile
  LAS_tiles <- LAS_RSDS@tileScheme[["tiles"]][buff,]

  if(length(LAS_tiles) == 0) return(NULL)

  # Get LAS files
  LAS_files <- LAS_RSDS@tilePaths[ LAS_tiles$tileName]

  if(any(!file.exists(LAS_files))) stop("Missing LAS files")

  # Create extent filter from buffer extent
  buff_xt   <- raster::extent(buff)
  buff_filt <- paste("-keep_xy", buff_xt@xmin, buff_xt@ymin, buff_xt@xmax, buff_xt@ymax)

  # Create class filter
  class_filt <- if(!is.null(classes)) paste(c("-keep_class", classes), collapse = " ")

  # Read LAS files
  inLAS <- lidR::readLAS(LAS_files, select = select, filter = c(buff_filt, class_filt))

  if(lidR::is.empty(inLAS)) return(NULL) else return(inLAS)

}

.normalizeLAS <- function(inLAS, DEMpath, zMin, zMax){

  if(!file.exists(DEMpath)) stop("Could not find DEM file '", DEMpath, "'")

  # Read segments and DEM
  DEM <- raster::raster(DEMpath)

  # Remove points that aren't on DEM
  inLAS <- lidR::lasmergespatial(inLAS, !is.na(DEM), attribute = "onDEM")
  inLAS <- lidR::lasfilter(inLAS, onDEM == 1)

  # No LAS points over DEM file
  if(lidR::is.empty(inLAS)) return(NULL)

  # Normalize
  inLAS <- lidR::lasnormalize(inLAS, DEM)

  # Filter LAS by height
  inLAS <- suppressMessages(lidR::lasfilter(inLAS, Z <= zMax & Z > zMin))

  # No LAS points within min/max Z bounds over DEM file
  if(lidR::is.empty(inLAS)) return(NULL)

  return(inLAS)

}




