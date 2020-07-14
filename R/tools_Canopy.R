#' Create canopy mask
#'
#' @param canopyClasses names of raster classes that are included in the canopy
#' @param openings the number of times that a morphological opening will be applied
#' @param openingRadius radius of morphological opening window
#'
#' @export

CanopyMask <- function(segClassRas_RSDS, canopyMask_RSDS, canopyClasses, openings = 1, openingRadius = 0.5,
                           tileNames = NULL, clusters = 1, overwrite = FALSE){


  tim <- .headline("CANOPY MASK")

  ### INPUT CHECKS ----

  # Check that all RSDS have same tileScheme
  .check_same_ts(segClassRas_RSDS, canopyMask_RSDS)

  # Check extensions
  .check_extension(segClassRas_RSDS,  "tif")
  .check_extension(canopyMask_RSDS,   "tif")

  # Check that inputs are complete
  .check_complete_input(segClassRas_RSDS, tileNames)

  if(!is.numeric(openings) || openings < 0) stop("Invalid input for 'openings':", openings)


  ### CREATE WORKER ----

  # Run process
  worker <- function(tileName){

    # Get unclassified raster segments
    segClassRas <- raster::raster(segClassRas_RSDS@tilePaths[tileName])

    # Out file
    outfile <- canopyMask_RSDS@tilePaths[tileName]

    # Check if raster is classified
    if(length(segClassRas@data@attributes) == 0) stop("'segClassRas_RSDS' was an unclassified input")
    rasClasses <- segClassRas@data@attributes[[1]]

    # Create matrix for converting clsses into binary canopy mask
    convertMatrix <- matrix(c(rasClasses$ID, as.numeric(rasClasses$category %in% canopyClasses)),ncol = 2)

    # Reclassify raster
    canopyRas <- raster::reclassify(segClassRas, convertMatrix)

    if(openings > 0){

      # Create moving window matrix for openings canopy edges
      mat <- raster::focalWeight(canopyRas, openingRadius)
      mat[mat > 0] <- 1
      mat[mat == 0] <- NA

      while(openings > 0){

        # Apply morphological opening
        canopyRas <- suppressWarnings({
          canopyRas %>%
            raster::focal(mat, min, na.rm = TRUE, pad = TRUE) %>% # Erode
            raster::focal(mat, max, na.rm = TRUE, pad = TRUE)     # Dilate
        })

        openings <- openings - 1
      }
    }

    # Save output
    raster::writeRaster( canopyRas, outfile, overwrite = overwrite, datatype = "INT1U")

    if(file.exists(outfile)) "Success" else stop("Failed to create output")

  }

  ### APPLY WORKER ----

  # Get tiles for processing
  procTiles <- .processing_tiles(canopyMask_RSDS, overwrite, tileNames)

  # Process
  status <- .doitlive(procTiles, clusters, worker)

  # Report
  .statusReport(status)

  # Conclude
  .conclusion(tim)

}
