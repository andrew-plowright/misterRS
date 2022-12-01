
#' Digital Elevation Model
#'
#' @export

surface_dem <- function(in_cat, out_rsds, LAS_select = "xyzc", res =  1,
                    tile_names = NULL, overwrite = FALSE){

  process_timer <- .headline("DIGITAL ELEVATION MODEL")

  ### INPUT CHECKS ----

    .check_extension(out_rsds, "tif")

    # Get tiles
    ts <- .get_tilescheme()

    # Get output file paths
    out_files <- .get_rsds_tilepaths(out_rsds)

    # # Get CRS
    crs <- getOption("misterRS.crs")

  ### CREATE WORKER ----

    # Run process
      tile_worker <-function(tile_name){

        # Get tile
        tile <- ts[tile_name,]

        # Set output file
        out_file <- out_files[tile_name]

        # Read LAS file
        LAStile <- .read_las_tile(in_cat = in_cat, tile = tile, select = LAS_select)

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
      queued_tiles <- .tile_queue(out_files, overwrite, tile_names)

      # Process
      process_status <- .exe_tile_worker(queued_tiles, tile_worker)

      # Report
      .print_process_status(process_status)

      # Conclude
      .conclusion(process_timer)
}


#' Digital Surface Model
#'
#' Generate a DSM. If \code{dem_rsds} is provided, it will generate a normalized DSM (nDSM)
#'
#' @export

surface_dsm <- function(in_cat, dem_rsds = NULL, out_rsds,
                     res = 0.25, z_min = 0, z_max = 80,
                     max_edge =c(0, 1), subcircle = 0,
                     thresholds = c(0, 2, 5, 10, 15, 20, 25, 30, 35, 40),
                     LAS_select = "xyzcr", LAS_classes = NULL,
                     tile_names = NULL, overwrite = FALSE){

  process_timer <- .headline("DIGITAL SURFACE MODEL")

  ### INPUT CHECKS ----

  # Switch for generating nDSM
  is_nDSM <- !is.null(dem_rsds)

  # Get tiles
  ts <- .get_tilescheme()

  # # Get CRS
  crs <- getOption("misterRS.crs")

  # Check inputs
  .check_extension(out_rsds, "tif")
  if(is_nDSM){
    .check_extension(dem_rsds, "tif")
    .check_complete_input(dem_rsds, tile_names)
  }

  # Get file paths
  out_files <- .get_rsds_tilepaths(out_rsds)
  if(is_nDSM) DEM_files <- .get_rsds_tilepaths(dem_rsds)

  ### CYCLE THROUGH TILES ----

  tile_worker <-function(tile_name){

    # Get tile
    tile <- ts[tile_name,]

    # File paths
    out_file <- out_files[tile_name]

    # Out raster layout
    out_template <- terra::rast(terra::ext(tile[["buffs"]]@bbox[c(1,3,2,4)]), res = res, crs = paste("epsg:", crs))

    # Read LAS tile
    LAStile <- .read_las_tile(in_cat = in_cat, tile = tile, select = LAS_select, classes = LAS_classes)

    if(is.null(LAStile)){

      # Blank nDSM
      out_DSM <- terra::setValues(out_template, NA)

    }else{

      # Normalize LAS tile
      if(is_nDSM){
        DEM_file <- DEM_files[tile_name]
        LAStile <- .normalize_las(LAStile, DEMpath = DEM_file, z_min, z_max)
      }

      ### IMPORTANT NOTE:
      #
      # These settings produced some ugly DSM artifacts
      # The 'Max Edge' setting might need to be changed
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
  queued_tiles <- .tile_queue(out_files, overwrite, tile_names)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)

}


#' Hillshade
#'
#' @export

hillshade <- function(rsds){

  process_timer <- .headline("HILLSHADE")

  in_file <- .get_rsds_mosaicpath(rsds)

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

  .conclusion(process_timer)
}
