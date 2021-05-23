#' Mosaic a RS Dataset
#'
#' @export

Mosaic <- function(RSDS, overlap = "nbuffs", outFile = NULL, overwrite = FALSE){

  tim <- .headline("MOSAIC")

  # Get tiles
  ts <- .get_tilescheme()

  # Get output file
  if(is.null(outFile)) outFile <- .get_RSDS_mosaicpath(RSDS)

  if(!dir.exists(dirname(outFile))) stop("Output directory not found")

  if(file.exists(outFile)){
    if(overwrite) unlink(outFile) else stop("Output file already exists")
  }

  # # Get CRS
  crs <- getOption("misterRS.crs")

  # Get tiles
  ts <- .get_tilescheme()

  # Get extension of input RSDS
  ext <- RSDS@ext

  # Merge rasters
  if(ext %in% c("tif")){

    # Generate VRT mosaic
    cat("  Making VRT", "\n")
    tempVRT <- .mosaicVRT(RSDS, ts,  overlap)

    # Merge
    cat("  Merging VRT to TIFF", "\n")
    gpal2::gdal_translate(
      co = c("BIGTIFF=YES", "COMPRESS=LZW"),
      tempVRT,
      outFile
    )

    if(!file.exists(outFile)) stop("Failed to merge VRT into single mosaic file")

    # Pyramids
    cat("  Generating pyramids", "\n")
    gpal2::gdaladdo(
      r = "average",
      ro = TRUE,
      outFile,
      c(2,4,8,16,32,64)
    )

  # Merge shape files
  }else if(ext %in% c("shp")){

    in_paths <- .get_RSDS_tilepaths(RSDS)

    # NOTE: This doesn't seem to work. It might be because the
    # 'in_paths' is too long for a command line call

    gpal2::ogrmerge(
      o            = c(outFile, in_paths),
      single       = TRUE,
      overwrite_ds = overwrite
    )

  }else stop("Unrecognized file extension for input RSDS: '", ext, "'")

  .conclusion(tim)

}


.mosaicVRT <- function(RSDS, ts, overlap, tileNames = NULL){

  overlapTypes <- c("buffs", "tiles", "nbuffs")
  if(!overlap %in% overlapTypes) stop("'overlap' argument should be: ", paste(overlapTypes, collapse = ", "))

  # Create temporary directory
  tempDir <- file.path(tempdir(), "mosaicVRT")
  dir.create(tempDir)
  withr::defer(unlink(tempDir, recursive = TRUE), envir = parent.frame(1))

  # Paths for temporary files
  tempList  <- file.path(tempDir, "tileList.txt")
  tempVRT   <- file.path(tempDir, "mosaic.vrt")

  # Get tile paths
  tilePaths <- .get_RSDS_tilepaths(RSDS)
  if(!is.null(tileNames)){

    if(any(!tileNames %in% names(tilePaths))) stop("Invalid selection for 'tileNames'")
    tilePaths <- tilePaths[tileNames]

  }

  # Create list of temporary VRT tiles
  tempTiles <- setNames(file.path(tempDir, paste0(names(tilePaths), ".vrt")), names(tilePaths))

  # Create VRT tiles with non-overlapping buffer
  for(tileName in names(tilePaths)){

    tile <- ts[tileName,]

    gpal2::gdalbuildvrt(
      te = raster::extent(tile[[overlap]]),
      tempTiles[tileName],
      tilePaths[tileName]
    )
  }

  # Write list of temporary VRT tiles
  write(tempTiles, tempList)

  # Create mosaic of tiled VRTs
  gpal2::gdalbuildvrt(
    input_file_list = tempList,
    tempVRT
  )

  return(tempVRT)
}
