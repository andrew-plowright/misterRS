#' Mask
#'
#' @export

Mask <- function(in_RSDS, out_RSDS, mask_RSDS,
                 maskNA = FALSE, tileNames = NULL,
                 overwrite = FALSE, useNeighbours = FALSE){


  tim <- .headline("MASK RSDS")

  ### INPUT CHECKS ----

  # Check that all RSDS have same tileScheme
  .check_same_ts(in_RSDS, out_RSDS, mask_RSDS)

  # Check extensions
  .check_extension(in_RSDS,   "tif")
  .check_extension(out_RSDS,  "tif")
  .check_extension(mask_RSDS, "tif")

  # Check that inputs are complete
  .check_complete_input(in_RSDS,   tileNames)
  .check_complete_input(mask_RSDS, tileNames)


  ### CREATE WORKER ----

  # Run process
  worker <- function(tileName){

    # Set output file
    outFile <- out_RSDS@tilePaths[tileName]

    # Read input raster
    inRas   <- raster::raster(in_RSDS@tilePaths[tileName])

    # Read mask file
    maskRas <- if(useNeighbours){

      maskPaths <- mask_RSDS@tilePaths[.tileNeighbours(mask_RSDS@tileScheme, tileName)$tileName]
      masks <- lapply(maskPaths, raster::raster)
      maskRas <- do.call(raster::mosaic, c(unname(masks), list(fun = max)))
      raster::crop(maskRas, mask_RSDS@tileScheme[tileName][["buffs"]])

    }else{

      raster::raster(mask_RSDS@tilePaths[tileName])

    }

    # Apply mask
    inRas[maskRas != 1] <- NA
    if(maskNA) inRas[is.na(maskRas)] <- NA

    # Save output
    raster::writeRaster(inRas, outFile, overwrite = overwrite)

    if(file.exists(outFile)) "Success" else stop("Failed to create output")

  }

  ### APPLY WORKER ----

  # Get tiles for processing
  procTiles <- .processing_tiles(out_RSDS, overwrite, tileNames)

  # Process
  status <- .doitlive(procTiles, worker)

  # Report
  .statusReport(status)

  # Conclude
  .conclusion(tim)

}
