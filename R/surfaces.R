
#' Digital Elevation Model
#'
#' @export

surface_dem <- function(in_cat, out_rts, las_select = "xyzc", res = 1, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("DIGITAL ELEVATION MODEL")

  ### INPUT CHECKS ----

  # Get tiles
  ts <- .tilescheme()

  # # Get CRS
  crs <- getOption("misterRS.crs")

  ### CREATE WORKER ----

  # Run process
  tile_worker <-function(tile_name){

    # Get tile
    tile <- ts[tile_name]

    # Set output file
    out_file <- out_rts$tile_path(tile_name)

    # Read LAS file
    las_tile <- .read_las_tile(in_cat = in_cat, tile = tile, select = las_select)

    # Filter duplicates
    las_tile <- if(!is.null(las_tile)) lidR::filter_duplicates(las_tile)

    # If las_tile is NULL or contains insufficient points, return a NA file
    dem <- if(is.null(las_tile) | sum(las_tile$Classification == 2) <= 3){

      terra::rast(terra::ext( tile[["buffs"]]), res = res, crs = paste("epsg:", crs), vals = NA)

    # Otherwise, triangulate DEM
    }else{

      # NOTE:
      # I would prefer that the 'res' argument take a template for creating the rasterized terrain, but for mysterious reasons that's not currently working

      lidR::rasterize_terrain(
        las_tile,
        res         = res,
        algorithm   = lidR::tin(),
        keep_lowest = FALSE,
        use_class   = c(2,9)
      )
    }

    # Save
    terra::writeRaster(dem, out_file, overwrite = TRUE)

    if(file.exists(out_file)) "Success" else "FAILED"
  }


  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_rts)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)
}


#' Digital Surface Model
#'
#' Generate a DSM. If \code{dem_rts} is provided, it will generate a normalized DSM (nDSM)
#'
#' @export

surface_dsm <- function(in_cat, dem_rts = NULL, out_rts, alg,
                     res = 0.25, z_min = 0, z_max = 80,
                     las_select = "xyzcr", las_classes = NULL, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("DIGITAL SURFACE MODEL")

  ### INPUT CHECKS ----

  # Switch for generating nDSM
  is_nDSM <- !is.null(dem_rts)

  # Get tiles
  ts <- .tilescheme()

  # # Get CRS
  crs <- getOption("misterRS.crs")

  # Check inputs
  if(is_nDSM) .complete_input(dem_rts)

  # Get file paths

  ### CYCLE THROUGH TILES ----

  tile_worker <-function(tile_name){

    # Get tile
    tile <- ts[tile_name]

    # File paths
    out_file <- out_rts$tile_path(tile_name)

    # Out raster layout
    out_template <- terra::rast(terra::ext(tile[["buffs"]]), res = res, crs = paste("epsg:", crs))

    # Read LAS tile
    las_tile <- .read_las_tile(in_cat = in_cat, tile = tile, select = las_select, classes = las_classes)

    # Normalize LAS tile
    if(is_nDSM & !is.null(las_tile)){

      las_tile %<>% .normalize_las(dem_path = dem_rts$tile_path(tile_name), z_min, z_max)
    }

    if(is.null(las_tile)){

      # Blank nDSM
      blank_value <- if(is_nDSM) 0 else NA
      out_DSM <- terra::setValues(out_template, blank_value)

    }else{

      # Generate surface
      out_DSM <- lidR::rasterize_canopy(las_tile, res = out_template, algorithm = alg)

      if(is.null(out_DSM)) stop("Failed to create DSM file")

      # Add random layer to eliminate adjacent cells with identical values
      out_DSM <- out_DSM + terra::setValues(out_template,  runif(terra::ncell(out_DSM), min = 0, max = 0.001))

      # Fill in gaps
      if(is_nDSM){
        out_DSM[is.na(out_DSM)] <- 0
      }
    }

    # Write DSM
    terra::writeRaster(out_DSM, out_file, overwrite = TRUE)

    if(file.exists(out_file)) "Success" else "FAILED"
  }


  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_rts)

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

hillshade <- function(in_rts){

  process_timer <- .headline("HILLSHADE")

  in_file <- in_rts$mosaic_path()

  if(!file.exists(in_file)) stop("No mosaic file found")

  out_file <- gsub("\\.tif$",  "_hillshade.tif", in_file)

  if(file.exists(out_file)){

    if(overwrite){

      unlink(.raster_files(in_file))

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
    out_file
  )

  .conclusion(process_timer)
}
