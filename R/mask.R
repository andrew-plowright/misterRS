#' Mask
#'
#' @param use_neibs logical. Set to TRUE if you wish to use include buffered sections from neighbouring tiles.
#'
#' @export

mask_rts <- function(in_rts, out_rts, mask_rts,
                 mask_na = FALSE, use_neibs = FALSE, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("MASK")

  ### INPUT CHECKS ----

  # Check that inputs are complete
  .complete_input(in_rts)
  .complete_input(mask_rts)

  # Get tile scheme
  ts <- .tilescheme()

  ### CREATE WORKER ----

  # Run process
  tile_worker <-function(tile_name){

    in_path   <- in_rts$tile_path(tile_name)
    out_path  <- out_rts$tile_path(tile_name)

    # Read mask file(s)
    if(use_neibs){

      buff <- ts[tile_name][["buffs"]]

      neib_names <- .tile_neibs(tile_name, ts)
      mask_neibs <- lapply(neib_names, function(tile_name) terra::rast(mask_rts$tile_path(tile_name)))

      # Merge and then crop
      mask_ras <- mask_neibs %>%
        terra::sprc() %>%
        terra::merge() %>%
        terra::crop(terra::ext(buff))

    }else{

      mask_ras <- terra::rast(mask_rts$tile_path(tile_name))
    }

    # Read input raster
    in_ras  <- terra::rast(in_path)

    # Apply mask
    in_ras[mask_ras != 1] <- NA
    if(mask_na) in_ras[is.na(mask_ras)] <- NA

    # Save output
    terra::writeRaster(in_ras, out_path, overwrite = TRUE)

    if(file.exists(out_path)) "Success" else stop("Failed to create output")

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
