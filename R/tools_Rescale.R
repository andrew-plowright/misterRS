#' Rescale raster dataset
#'
#' @export

Rescale <- function(in_RSDS, out_RSDS, from, to = c(0, 254), NAtoZero = TRUE, dataType = "INT1U", tileNames = NULL, clusters = 1, overwrite = FALSE){

  tim <- .headline("RESCALE DATASET")

  ### INPUT CHECKS ----

  .check_same_ts(in_RSDS, out_RSDS)

  .check_complete_input(in_RSDS, tileNames)


  ### CREATE WORKER ----

  worker <- function(tileName){

    in_file  <- in_RSDS@tilePaths[tileName]
    out_file <- out_RSDS@tilePaths[tileName]

    # Read raster
    ras  <- raster::raster(in_file)

    if(NAtoZero) ras[is.na(ras)] <- 0

    # Scale elevation raster
    ras <- .rescale(ras, from = from, to = to)

    # Save re-scaled CHM
    raster::writeRaster(ras, out_file, datatype = dataType, overwrite = overwrite)

    if(file.exists(out_file)){
      return("Success")
    }else{
      stop("Failed to create tile")
    }
  }


  ### APPLY WORKER ----

  # Get tiles for processing
  procTiles <- .processing_tiles(out_RSDS, overwrite, tileNames)

  # Process
  status <- .doitlive(procTiles, clusters, worker)

  # Report
  .statusReport(status)

  # Conclude
  .conclusion(tim)

}


# Function for rescaling
.rescale <- function(x, from, to){
  out <- (x - from[1]) / (from[2] - from[1]) * (to[2] - to[1]) + to[1]
  out[out<to[1]] <- to[1]
  out[out>to[2]] <- to[2]
  return(out)
}
