
#' Digital Elevation Model
#'
#' @export

MakeDEM <- function(LAS_RSDS, out_RSDS, LASselect = "xyzc", DEMres =  1,
                    tileNames = NULL, clusters = 1, overwrite = FALSE){

  tim <- .headline("DIGITAL ELEVATION MODEL")

  ### INPUT CHECKS ----

    .check_extension(LAS_RSDS, "las")
    .check_extension(out_RSDS, "tif")

    .check_complete_input(LAS_RSDS)

    ### CREATE WORKER ----

      # Run process
      worker <- function(tileName){

        # Set output file
        out_file <- out_RSDS@tilePaths[tileName]

        # Get tile
        tile <- out_RSDS@tileScheme[tileName,]

        # Read LAS file
        LAStile <- .readLAStile(LAS_RSDS = LAS_RSDS, tile = tile, select = LASselect)

        # Filter duplicates
        LAStile <- if(!is.null(LAStile)) lidR::lasfilterduplicates(LAStile)

        # Create output layout
        DEMlayout <- raster::raster(raster::extent(tile[["buffs"]]), res = DEMres, crs = out_RSDS@tileScheme@crs)

        # If LAStile is NULL or contains insufficient points, return a NA file
        DEM <- if(is.null(LAStile) | sum(LAStile$Classification == 2) <= 3){

          raster::setValues(DEMlayout, NA)

        # Otherwise, triangulate DEM
        }else{

          suppressWarnings(lidR::grid_terrain(
            LAStile,
            res         = DEMlayout,
            algorithm   = lidR::tin(),
            keep_lowest = FALSE,
            use_class   = c(2,9)))
        }

        # Save
        raster::writeRaster(DEM, out_file, overwrite = TRUE)

        if(file.exists(out_file)) "Success" else "FAILED"
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



#' Canopy Height Model
#'
#' @export

MakeNDSM <- function(LAS_RSDS, DEM_RSDS, out_RSDS,
                     nDSMres, zMin, zMax,
                     maxEdge =c(0, 1), subCircle = 0,
                     thresholds = c(0, 2, 5, 10, 15, 20, 25, 30, 35, 40),
                     LASselect = "xyzcr", LASclasses = NULL,
                     tileNames = NULL, clusters = 1, overwrite = FALSE){

  tim <- .headline("NORMALIZED DSM")

  ### INPUT CHECKS ----

  .check_same_ts(DEM_RSDS, out_RSDS)

  .check_extension(LAS_RSDS, "las")
  .check_extension(out_RSDS, "tif")

  .check_complete_input(DEM_RSDS, tileNames)
  .check_complete_input(LAS_RSDS)


  ### CYCLE THROUGH TILES ----

  worker <- function(tileName){

    # Get tile
    tile <- out_RSDS@tileScheme[tileName,]

    # Out file
    out_file <- out_RSDS@tilePaths[tileName]

    # Out raster layout
    tile_ext <- raster::extent(tile[["buffs"]])
    nDSMlayout <- raster::raster(tile_ext, res = nDSMres, crs = out_RSDS@tileScheme@crs)

    # Read LAS tile
    LAStile <- .readLAStile(LAS_RSDS = LAS_RSDS, tile = tile, select = LASselect, classes = LASclasses)

    # Normalize LAS tile
    LAStile <- if(!is.null(LAStile)) .normalizeLAS(LAStile, DEMpath = DEM_RSDS@tilePaths[tileName], zMin, zMax)

    if(is.null(LAStile)){

      # Create blank nDSM
      nDSM <- raster::setValues(nDSMlayout, NA)

    }else{

      ### IMPORTANT NOTE:
      #
      # These settings produced some ugly nDSM artifacts
      # The 'Max Edge' setting might need to be changed
      # View coordinates c(529709.302, 5444776.672) in the Langley nDSM
      # The 'pitfree' algorithm is probably best, but do more tests next time to figure out best parameters
      #
      # Use invisible and capture.output to suppress annoying progress bar

      invisible(capture.output({

        nDSM <- lidR::grid_canopy(
          LAStile,
          res = nDSMlayout,
          algorithm = lidR::pitfree(
            thresholds = thresholds,
            max_edge   = maxEdge,
            subcircle  = subCircle)
        )
      }))

      if(is.null(nDSM)) stop("Failed to create nDSM file")

      # Extent to size of tile
      #nDSM <- raster::extend(nDSM, tile_ext)

      # Add random layer to eliminate adjacent cells with identical values
      nDSM <- nDSM + runif(raster::ncell(nDSM), min = 0, max = 0.0001)

      # Fill in gaps
      nDSM[is.na(nDSM)] <- 0

    }

    # Set projection
    #raster::crs(nDSM) <- tile@crs

    # Write nDSM
    raster::writeRaster(nDSM, out_file, overwrite = TRUE)

    if(file.exists(out_file)) "Success" else "FAILED"
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


#' Make Normalized Digital Surface Model
#'
#' @export

MakeNDSM2 <- function(DSM_RSDS, DEM_RSDS, out_RSDS, tileNames = NULL, clusters = 1, overwrite = FALSE){

  tim <- .headline("NORMALIZED DIGITAL SURFACE MODEL")

  ### INPUT CHECKS ----

  .check_same_ts(DSM_RSDS, DEM_RSDS, out_RSDS)

  .check_complete_input(DSM_RSDS, tileNames)
  .check_complete_input(DEM_RSDS, tileNames)


  ### CREATE WORKER ----

  worker <- function(tileName){

    DEM <- raster::raster(DEM_RSDS@tilePaths[tileName])
    DSM <- raster::raster(DSM_RSDS@tilePaths[tileName])

    nDSM <- DSM - DEM

    raster::writeRaster(nDSM, out_RSDS@tilePaths[tileName], overwrite = overwrite)

    if(file.exists(out_RSDS@tilePaths[tileName])){
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
