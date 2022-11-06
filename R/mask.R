#' Mask
#'
#' @param use_neibs logical. Set to TRUE if you wish to use include buffered sections from neighbouring tiles.
#'
#' @export

mask <- function(in_rsds, out_rsds, mask_rsds,
                 mask_na = FALSE, tile_names = NULL,
                 overwrite = FALSE, use_neibs = FALSE){


  process_timer <- .headline("MASK RSDS")

  ### INPUT CHECKS ----

  # Check extensions
  .check_extension(in_rsds,   "tif")
  .check_extension(out_rsds,  "tif")
  .check_extension(mask_rsds, "tif")

  # Check that inputs are complete
  .check_complete_input(in_rsds,   tile_names)
  .check_complete_input(mask_rsds, tile_names)

  # Get paths
  in_paths   <- .get_rsds_tilepaths(in_rsds)
  out_paths  <- .get_rsds_tilepaths(out_rsds)
  mask_paths <- .get_rsds_tilepaths(mask_rsds)

  # Get tile scheme
  ts <- .get_tilescheme()

  ### CREATE WORKER ----

  # Run process
  tile_worker <-function(tile_name){

    in_path   <- in_paths[tile_name]
    out_path  <- out_paths[tile_name]

    # Read mask file(s)
    maskRas <- if(use_neibs){

      neibNames <- .tile_neibs(ts, tile_name)$tile_name

      masks <- lapply(mask_paths[neibNames], raster::raster)

      maskRas <- do.call(raster::mosaic, c(unname(masks), list(fun = max)))

      raster::crop(maskRas, ts[tile_name][["buffs"]])

    }else{

      raster::raster(mask_paths[tile_name])
    }

    # Read input raster
    inRas   <- raster::raster(in_path)

    # Apply mask
    inRas[maskRas != 1] <- NA
    if(mask_na) inRas[is.na(maskRas)] <- NA

    # Save output
    raster::writeRaster(inRas, out_path, overwrite = overwrite)

    if(file.exists(out_path)) "Success" else stop("Failed to create output")

  }

  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_paths, overwrite, tile_names)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)

}
