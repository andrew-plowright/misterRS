#' Mask
#'
#' @export

Mask <- function(in_RSDS, out_RSDS, mask_RSDS,
                 maskNA = FALSE, tileNames = NULL,
                 overwrite = FALSE, useNeighbours = FALSE){


  tim <- .headline("MASK RSDS")

  ### INPUT CHECKS ----

  # Check extensions
  .check_extension(in_RSDS,   "tif")
  .check_extension(out_RSDS,  "tif")
  .check_extension(mask_RSDS, "tif")

  # Check that inputs are complete
  .check_complete_input(in_RSDS,   tileNames)
  .check_complete_input(mask_RSDS, tileNames)

  # Get paths
  in_paths   <- .get_RSDS_tilepaths(in_RSDS)
  out_paths  <- .get_RSDS_tilepaths(out_RSDS)
  mask_paths <- .get_RSDS_tilepaths(mask_RSDS)

  # Get tile scheme
  ts <- .get_tilescheme()

  ### CREATE WORKER ----

  # Run process
  worker <- function(tileName){

    in_path   <- in_paths[tileName]
    out_path  <- out_paths[tileName]

    # Read mask file(s)
    maskRas <- if(useNeighbours){

      neibNames <- .tileNeighbours(ts, tileName)$tileName

      masks <- lapply(mask_paths[neibNames], raster::raster)

      maskRas <- do.call(raster::mosaic, c(unname(masks), list(fun = max)))

      raster::crop(maskRas, ts[tileName][["buffs"]])

    }else{

      raster::raster(mask_paths[tileName])

    }


    # Read input raster
    inRas   <- raster::raster(in_path)

    # Apply mask
    inRas[maskRas != 1] <- NA
    if(maskNA) inRas[is.na(maskRas)] <- NA

    # Save output
    raster::writeRaster(inRas, out_path, overwrite = overwrite)

    if(file.exists(out_path)) "Success" else stop("Failed to create output")

  }

  ### APPLY WORKER ----

  # Get tiles for processing
  procTiles <- .processing_tiles(out_paths, overwrite, tileNames)

  # Process
  status <- .doitlive(procTiles, worker)

  # Report
  .statusReport(status)

  # Conclude
  .conclusion(tim)

}
