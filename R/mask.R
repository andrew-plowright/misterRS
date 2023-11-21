#' Mask
#'
#' @param use_neibs logical. Set to TRUE if you wish to use include buffered sections from neighbouring tiles.
#'
#' @export

mask_rs <- function(in_rsds, out_rsds, mask_rsds,
                 mask_na = FALSE, use_neibs = FALSE, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("MASK RSDS")

  ### INPUT CHECKS ----

  # Check extensions
  .check_extension(in_rsds,   "tif")
  .check_extension(out_rsds,  "tif")
  .check_extension(mask_rsds, "tif")

  # Check that inputs are complete
  .check_complete_input(in_rsds)
  .check_complete_input(mask_rsds)

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
    if(use_neibs){

      buff <- sf::st_as_sf(ts[["buffs"]][ts[["buffs"]]$tileName == tile_name,])

      neib_names <- .tile_neibs(ts, tile_name)$tileName
      mask_neibs <- lapply(mask_paths[neib_names], terra::rast)

      # Merge and then crop
      mask_ras <- mask_neibs %>%
        terra::sprc() %>%
        terra::merge() %>%
        terra::crop(terra::ext(buff))

    }else{

      mask_ras <- terra::rast(mask_paths[tile_name])
    }

    # Read input raster
    in_ras  <- terra::rast(in_path)

    # Apply mask
    in_ras[mask_ras != 1] <- NA
    if(mask_na) in_ras[is.na(mask_ras)] <- NA

    # Save output
    terra::writeRaster(in_ras, out_path, overwrite = overwrite)

    if(file.exists(out_path)) "Success" else stop("Failed to create output")

  }

  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_paths)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)

}
