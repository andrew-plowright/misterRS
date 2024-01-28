#' Difference between two raster
#' @export

ras_diff <- function(A_rts, B_rts, out_rts, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("RASTER DIFFERENCE")

  ### INPUT CHECKS ----

  .check_same_ts(A_rts, B_rts, out_rts)

  .check_complete_input(A_rts)
  .check_complete_input(B_rts)

  out_files <- .rts_tile_paths(out_rts)
  A_files   <- .rts_tile_paths(A_rts)
  B_files   <- .rts_tile_paths(B_rts)

  ### CREATE WORKER ----

  tile_worker <-function(tile_name){

    A <- terra::rast(A_files[tile_name])
    B <- terra::rast(B_files[tile_name])

    C <- A - B

    terra::writeRaster(C, out_files[tile_name], overwrite = overwrite)

    if(file.exists(out_files[tile_name])){
      return("Success")
    }else{
      stop("Failed to create tile")
    }
  }


  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_files)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)

}

