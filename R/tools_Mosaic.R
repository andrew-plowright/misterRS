#' Mosaic a RS Dataset
#'
#' @export

Mosaic <- function(RSDS, overlap = "nbuffs", outFile = NULL, overwrite = FALSE){

  tim <- .headline("MOSAIC")

  .check_complete_input(RSDS)

  if(is.null(outFile)){
    if(is.na(RSDS@mosaic)) stop("No output file specified")
    outFile <- RSDS@mosaic
  }

  outFile <- suppressPackageStartupMessages(R.utils::getAbsolutePath(outFile))

  if(!dir.exists(dirname(outFile))) stop("Output directory not found")

  if(file.exists(outFile)){
    if(overwrite) unlink(outFile) else stop("Output file already exists")
  }

  # Generate VRT mosaic
  cat("  Making VRT", "\n")
  tempVRT <- .mosaicVRT(RSDS, overlap)

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

  .conclusion(tim)

}


.mosaicVRT <- function(RSDS, overlap, tileNames = NULL){

  overlapTypes <- c("buffs", "tiles", "nbuffs")
  if(!overlap %in% overlapTypes) stop("'overlap' argument should be: ", paste(overlapTypes, collapse = ", "))

  # Create temporary directory
  tempDir <- file.path(tempdir(), "mosaicVRT")
  dir.create(tempDir)

  # Paths for temporary files
  tempList  <- file.path(tempDir, "tileList.txt")
  tempVRT   <- file.path(tempDir, "mosaic.vrt")

  # Get tile paths
  tilePaths <- if(is.null(tileNames)){

    RSDS@tilePaths

  }else{

    if(any(!tileNames %in% names(RSDS@tilePaths))) stop("Invalid selection for 'tileNames'")
    RSDS@tilePaths[tileNames]

  }

  # Create list of temporary VRT tiles
  tempTiles <- setNames(file.path(tempDir, paste0(names(tilePaths), ".vrt")), names(tilePaths))

  # Create VRT tiles with non-overlapping buffer
  for(tileName in names(tilePaths)){

    gpal2::gdalbuildvrt(
      te = raster::extent(RSDS@tileScheme[tileName][[overlap]]),
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
