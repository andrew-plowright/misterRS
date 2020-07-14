#' Principal Component Analysis
#'
#' @export

PCA <- function(ortho_RSDS, out_RSDS, nComp = 2, tileNames = NULL, clusters = 1, overwrite = FALSE){

  tim <- .headline("PRINCIPAL COMPONENT ANALYSIS")

  ### INPUT CHECKS ----

  .check_same_ts(ortho_RSDS, out_RSDS)

  .check_complete_input(ortho_RSDS, tileNames)

  ### INPUT CHECKS ----

  worker <- function(tileName){

    in_file  <- ortho_RSDS@tilePaths[tileName]
    out_file <- out_RSDS@tilePaths[tileName]

    in_ras <- raster::brick(in_file)

    # Generate PCA
    PCAras <- RStoolbox::rasterPCA(in_ras, nComp = nComp, spca = TRUE)

    # Write PCA
    raster::writeRaster(PCAras$map, out_file)

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
