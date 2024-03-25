#' Difference between two raster
#' @export

ras_diff <- function(A_rts, B_rts, out_rts, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("RASTER DIFFERENCE")

  ### INPUT CHECKS ----

  .complete_input(A_rts)
  .complete_input(B_rts)


  ### CREATE WORKER ----

  tile_worker <-function(tile_name){

    A <- terra::rast(A_rts$tile_path(tile_name))
    B <- terra::rast(B_rts$tile_path(tile_name))

    C <- A - B

    terra::writeRaster(C, out_rts$tile_path(tile_name), overwrite = TRUE)

    if(file.exists(out_files[tile_name])){
      return("Success")
    }else{
      stop("Failed to create tile")
    }
  }


  ### APPLY WORKER ----

  out_rts %>%
    .tile_queue() %>%
    .exe_tile_worker(tile_worker) %>%
    .print_process_status()

  # Conclude
  .conclusion(process_timer)

}

