
#' Digital Elevation Model
#'
#' @export

MakeDEM <- function(in_cat, out_RSDS, LASselect = "xyzc", DEMres =  1,
                    tileNames = NULL, overwrite = FALSE){

  tim <- .headline("DIGITAL ELEVATION MODEL")

  ### INPUT CHECKS ----

    .check_extension(out_RSDS, "tif")

    # Get tiles
    ts <- .get_tilescheme()

    # Get output file paths
    out_files <- .get_RSDS_tilepaths(out_RSDS)

    # # Get CRS
    crs <- getOption("misterRS.crs")

    ### CREATE WORKER ----

      # Run process
      worker <- function(tileName){

        # Get tile
        tile <- ts[tileName,]

        # Set output file
        out_file <- out_files[tileName]

        # Read LAS file
        LAStile <- .readLAStile(in_cat = in_cat, tile = tile, select = LASselect)

        # Filter duplicates
        LAStile <- if(!is.null(LAStile)) lidR::filter_duplicates(LAStile)

        # Create output layout
        DEMlayout <- raster::raster(raster::extent(tile[["buffs"]]), res = DEMres, crs = crs)

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
      procTiles <- .processing_tiles(out_files, overwrite, tileNames)

      # Process
      status <- .doitlive(procTiles, worker)

      # Report
      .statusReport(status)

      # Conclude
      .conclusion(tim)
}



#' Make Normalized Digital Surface Model
#'
#' This version will create an nDSM directly from the point cloud (no existing DSM required)
#'
#' @export

MakeNDSM <- function(in_cat, DEM_RSDS, out_RSDS,
                     nDSMres, zMin, zMax,
                     maxEdge =c(0, 1), subCircle = 0,
                     thresholds = c(0, 2, 5, 10, 15, 20, 25, 30, 35, 40),
                     LASselect = "xyzcr", LASclasses = NULL,
                     tileNames = NULL, overwrite = FALSE){

  tim <- .headline("NORMALIZED DIGITAL SURFACE MODEL")

  ### INPUT CHECKS ----

  .check_extension(DEM_RSDS, "tif")
  .check_extension(out_RSDS, "tif")

  .check_complete_input(DEM_RSDS, tileNames)

  # Get tiles
  ts <- .get_tilescheme()

  # Get file paths
  DEM_files <- .get_RSDS_tilepaths(DEM_RSDS)
  out_files <- .get_RSDS_tilepaths(out_RSDS)

  # # Get CRS
  crs <- getOption("misterRS.crs")

  ### CYCLE THROUGH TILES ----

  worker <- function(tileName){

    # Get tile
    tile <- ts[tileName,]

    # File paths
    out_file <- out_files[tileName]
    DEM_file <- DEM_files[tileName]

    # Out raster layout
    tile_ext <- raster::extent(tile[["buffs"]])
    nDSMlayout <- raster::raster(tile_ext, res = nDSMres, crs =  crs)

    # Read LAS tile
    LAStile <- .readLAStile(in_cat = in_cat, tile = tile, select = LASselect, classes = LASclasses)

    # Normalize LAS tile
    LAStile <- if(!is.null(LAStile)) .normalizeLAS(LAStile, DEMpath = DEM_file, zMin, zMax)

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
      nDSM <- raster::extend(nDSM, tile_ext)

      # Add random layer to eliminate adjacent cells with identical values
      nDSM <- nDSM + runif(raster::ncell(nDSM), min = 0, max = 0.0001)

      # Fill in gaps
      nDSM[is.na(nDSM)] <- 0

    }

    # Write nDSM
    raster::writeRaster(nDSM, out_file, overwrite = TRUE)

    if(file.exists(out_file)) "Success" else "FAILED"
  }


  ### APPLY WORKER ----

  # Get tiles for processing
  procTiles <- .processing_tiles(out_files, overwrite, tileNames)

  # Process
  status <- .doitlive(procTiles,  worker)

  # Report
  .statusReport(status)

  # Conclude
  .conclusion(tim)

}


#' Make Normalized Digital Surface Model
#'
#' This version will subtract the DEM from an existing DSM
#'
#' @export

MakeNDSM2 <- function(DSM_RSDS, DEM_RSDS, out_RSDS,
                      tileNames = NULL, overwrite = FALSE){

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
  procTiles <- .processing_tiles(out_files, overwrite, tileNames)

  # Process
  status <- .doitlive(procTiles, worker)

  # Report
  .statusReport(status)

  # Conclude
  .conclusion(tim)

}


#' Hillshade
#'
#' @export

Hillshade <- function(RSDS){

  tim <- .headline("HILLSHADE")

  in_file <- .get_RSDS_mosaicpath(RSDS)

  if(!file.exists(in_file)) stop("No mosaic file found")

  out_file <- gsub("\\.tif$",  "_hillshade.tif", in_file)

  if(file.exists(out_file)){

    if(overwrite){

      del_files <- c(APfun::APrasterFiles(in_file), paste0(in_file, ".ovr"))
      unlink(del_files)

    }else stop("Output file exists. Set 'overwrite' to TRUE")

  }

  cat("  Generating hillshade", "\n")

  gpal2::gdaldem(
    "hillshade",
    in_file,
    out_file,
    co = "COMPRESS=LZW"
  )

  cat("  Building pyramids", "\n")

  gpal2::gdaladdo(
    r = "average",
    ro = TRUE,
    out_file,
    c(2,4,8,16,32,64)
  )

  .conclusion(tim)

}
