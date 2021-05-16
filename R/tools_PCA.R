#' Principal Component Analysis
#'
#' @export

PCA <- function(ortho_RSDS, out_RSDS, nComp = 2,
                tileNames = NULL, overwrite = FALSE){

  tim <- .headline("PRINCIPAL COMPONENT ANALYSIS")

  ### INPUT CHECKS ----

  .check_complete_input(ortho_RSDS, tileNames)

  # Get tiles
  ts <- .get_tilescheme()

  # Get file paths
  out_files   <- .get_RSDS_tilepaths(out_RSDS)
  ortho_files <- .get_RSDS_tilepaths(ortho_RSDS)

  ### PROCESS ----

  worker <- function(tileName){

    in_file  <- ortho_files[tileName]
    out_file <- out_files[tileName]

    # NOTE: Don't include the fourth band (the alpha band) or it'll mess things up
    in_ras <- raster::brick(in_file)[[1:3]]

    # Generate PCA
    PCAras <- RStoolbox::rasterPCA(in_ras, nComp = nComp, spca = TRUE)

    # Write PCA
    raster::writeRaster(PCAras$map, out_file, overwrite = overwrite)

    if(file.exists(out_file)){
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
