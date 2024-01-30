#' Pseudo image
#'
#' Create a 3-band pseudo image using bands from several different images or surfaces. These can (and should) be rescaled to an 8-bit image (values from 0 to 255)
#'
#' The \code{inputs} argument should be a list with up to three elements, each of which correspond to a band in the output image.
#' Each element should be in a named list, for which the elements are:
#' \describe{
#'   \item{\code{rts}}{Raster tileset}
#'   \item{\code{band}}{The band number from the RS Dataset}
#'   \item{\code{range}}{A numeric vector with two elements indicating the minimum and maximum range of the input RS Dataset band that will be rescaled}
#' }
#'
#' @export

pseudo_img <- function(inputs, out_rts, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("PSEUDO-IMAGE")

  ### INPUT CHECKS ----

  for(input in inputs) .complete_input(input$rts)

  # Get tiles
  ts <- .tilescheme()

  # Get file paths
  out_files  <- .rts_tile_paths(out_rts)
  in_files <- lapply(inputs, function(input){.rts_tile_paths(input$rts)})

  ### CREATE WORKER ----

  tile_worker <-function(tile_name){

    # Output file
    out_file  <- out_files[tile_name]

    # Create raster stack
    ras_stack <- do.call(c, lapply(1:length(inputs), function(i){

      # Read in band
      input <- inputs[[i]]
      in_file <- in_files[[i]][tile_name]
      in_ras <- terra::rast(in_file, lyrs = input$band)

      # Remove NA values
      in_ras[is.na(in_ras)] <- 0

      # Rescale band if needed
      if(!is.null(input$range)){
        in_ras <- .rescale(in_ras, from = input$range, to = c(0,254))
      }

      return(in_ras)

    }))

    # Write output
    terra::writeRaster(ras_stack, filename = out_file, datatype = "INT1U", overwrite = overwrite)

    if(file.exists(out_file)){
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


.rescale <- function(x, from, to){
  out <- (x - from[1]) / (from[2] - from[1]) * (to[2] - to[1]) + to[1]
  out[out<to[1]] <- to[1]
  out[out>to[2]] <- to[2]
  return(out)
}
