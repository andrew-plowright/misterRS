#' Variable window filter
#' @export

Difference <- function(A_RSDS, B_RSDS, out_RSDS,
                      tileNames = NULL, overwrite = FALSE){

  tim <- .headline("NORMALIZED DIGITAL SURFACE MODEL")

  ### INPUT CHECKS ----

  .check_same_ts(A_RSDS, B_RSDS, out_RSDS)

  .check_complete_input(A_RSDS, tileNames)
  .check_complete_input(B_RSDS, tileNames)

  out_files <- .get_RSDS_tilepaths(out_RSDS)
  A_files   <- .get_RSDS_tilepaths(A_RSDS)
  B_files   <- .get_RSDS_tilepaths(B_RSDS)

  ### CREATE WORKER ----

  worker <- function(tileName){

    A <- raster::raster(A_files[tileName])
    B <- raster::raster(B_files[tileName])

    C <- A - B

    raster::writeRaster(C, out_files[tileName], overwrite = overwrite)

    if(file.exists(out_files[tileName])){
      return("Success")
    }else{
      stop("Failed to create tile")
    }
  }


  ### APPLY WORKER ----

  # Get tiles for processing
  procTiles <- .processing_tiles(out_files, overwrite, tileNames)

  # Process
  status <- .doitlive(procTiles, worker)

  # Report
  .statusReport(status)

  # Conclude
  .conclusion(tim)

}

