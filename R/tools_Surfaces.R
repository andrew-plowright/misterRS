
#' Digital Elevation Model
#'
#' @export

MakeDEM <- function(in_cat, out_RSDS, LAS_select = "xyzc", res =  1,
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
        LAStile <- .readLAStile(in_cat = in_cat, tile = tile, select = LAS_select)

        # Filter duplicates
        LAStile <- if(!is.null(LAStile)) lidR::filter_duplicates(LAStile)

        # If LAStile is NULL or contains insufficient points, return a NA file
        DEM <- if(is.null(LAStile) | sum(LAStile$Classification == 2) <= 3){

          terra::rast(terra::ext( tile[["buffs"]]@bbox[c(1,3,2,4)]), res = res, crs = paste("epsg:", crs), vals = NA)

        # Otherwise, triangulate DEM
        }else{

          # NOTE:
          # I would prefer that the 'res' argument take a template for creating the rasterized terrain, but for mysterious reasons that's not currently working

          lidR::rasterize_terrain(
            LAStile,
            res         = res,
            algorithm   = lidR::tin(),
            keep_lowest = FALSE,
            use_class   = c(2,9)
          )


        }

        # Save
        terra::writeRaster(DEM, out_file, overwrite = TRUE)


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



#' Make Digital Surface Model
#'
#' Generate a DSM. If \code{DEM_RSDS} is provided, it will generate a normalized DSM (nDSM)
#'
#' @export

MakeDSM <- function(in_cat, DEM_RSDS = NULL, out_RSDS,
                     res = 0.25, zMin = 0, zMax = 80,
                     max_edge =c(0, 1), subcircle = 0,
                     thresholds = c(0, 2, 5, 10, 15, 20, 25, 30, 35, 40),
                     LAS_select = "xyzcr", LAS_classes = NULL,
                     tileNames = NULL, overwrite = FALSE){

  tim <- .headline("DIGITAL SURFACE MODEL")

  ### INPUT CHECKS ----

  # Switch for generating nDSM
  is_nDSM <- !is.null(DEM_RSDS)

  # Get tiles
  ts <- .get_tilescheme()

  # # Get CRS
  crs <- getOption("misterRS.crs")

  # Check inputs
  .check_extension(out_RSDS, "tif")
  if(is_nDSM){
    .check_extension(DEM_RSDS, "tif")
    .check_complete_input(DEM_RSDS, tileNames)
  }

  # Get file paths
  out_files <- .get_RSDS_tilepaths(out_RSDS)
  if(is_nDSM) DEM_files <- .get_RSDS_tilepaths(DEM_RSDS)

  ### CYCLE THROUGH TILES ----

  worker <- function(tileName){

    # Get tile
    tile <- ts[tileName,]

    # File paths
    out_file <- out_files[tileName]

    # Out raster layout
    out_template <- terra::rast(terra::ext(tile[["buffs"]]@bbox[c(1,3,2,4)]), res = res, crs = paste("epsg:", crs))

    # Read LAS tile
    LAStile <- .readLAStile(in_cat = in_cat, tile = tile, select = LAS_select, classes = LAS_classes)

    if(is.null(LAStile)){

      # Blank nDSM
      out_DSM <- terra::setValues(out_template, NA)

    }else{

      # Normalize LAS tile
      if(is_nDSM){
        DEM_file <- DEM_files[tileName]
        LAStile <- .normalizeLAS(LAStile, DEMpath = DEM_file, zMin, zMax)
      }

      ### IMPORTANT NOTE:
      #
      # These settings produced some ugly DSM artifacts
      # The 'Max Edge' setting might need to be changed
      # View coordinates c(529709.302, 5444776.672) in the Langley DSM
      # The 'pitfree' algorithm is probably best, but do more tests next time to figure out best parameters

      # Set algorithm
      alg <- lidR::pitfree(thresholds = thresholds, max_edge = max_edge, subcircle = subcircle)

      # Generate surface
      out_DSM <- lidR::rasterize_canopy(LAStile, res = out_template, algorithm = alg)

      if(is.null(out_DSM)) stop("Failed to create DSM file")

      # Add random layer to eliminate adjacent cells with identical values
      out_DSM <- out_DSM + terra::setValues(out_template,  runif(terra::ncell(out_DSM), min = 0, max = 0.0001))

      # Fill in gaps
      out_DSM[is.na(out_DSM)] <- 0

    }

    # Write DSM
    terra::writeRaster(out_DSM, out_file, overwrite = TRUE)

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
