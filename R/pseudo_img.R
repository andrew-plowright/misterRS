#' Pseudo image
#'
#' Create a 3-band pseudo image using bands from several different images or surfaces. These can (and should) be rescaled to an 8-bit image (values from 0 to 255)
#'
#' The \code{inputs} argument should be a list with up to three elements, each of which correspond to a band in the output image.
#' Each element should be in a named list, for which the elements are:
#' \describe{
#'   \item{\code{rsds}}{A RS Dataset}
#'   \item{\code{band}}{The band number from the RS Dataset}
#'   \item{\code{range}}{A numeric vector with two elements indicating the minimum and maximum range of the input RS Dataset band that will be rescaled}
#' }
#'
#' @examples
#' \dontrun{
#' pca    <- rsds(dir = "data/pca",    id = "pca",    name = "PCA",    ext = "tif")
#' dsm    <- rsds(dir = "data/dsm",    id = "dsm",    name = "DSM",    ext = "tif")
#' pseudo <- rsds(dir = "data/pseudo", id = "pseudo", name = "Pseudo", ext = "tif")
#'
#' pseudo_img(
#'   inputs = list(
#'     list(rsds = pca, band = 1, range = c(-2.5, 2.5)),
#'     list(rsds = pca, band = 2, range = c(-0.3, 0.3)),
#'     list(rsds = dsm, band = 1, range = (0,30))
#'   ),
#'  out_rsds = pseudo
#' )
#' }
#' @export

pseudo_img <- function(inputs, out_rsds,  tile_names = NULL, overwrite = FALSE){

  process_timer <- .headline("VERTICAL IMAGE")

  ### INPUT CHECKS ----

  for(input in inputs){
    .check_complete_input(input$rsds,  tile_names)
    .check_extension(input$rsds,  "tif")
  }
  .check_extension(out_rsds,  "tif")

  # Get tiles
  ts <- .get_tilescheme()

  # Get file paths
  out_files  <- .get_rsds_tilepaths(out_rsds)
  in_files <- lapply(inputs, function(input){.get_rsds_tilepaths(input$rsds)})

  ### CREATE WORKER ----

  tile_worker <-function(tile_name){

    # Output file
    out_file  <- out_files[tile_name]

    # Create raster stack
    ras_stack <- do.call(raster::stack, lapply(1:length(inputs), function(i){

      # Read in band
      input <- inputs[[i]]
      in_file <- in_files[[i]][tile_name]
      in_ras <- raster::raster(in_file, band = input$band)

      # Remove NA values
      in_ras[is.na(in_ras)] <- 0

      # Rescale band if needed
      if(!is.null(input$range)){
        in_ras <- .rescale(in_ras, from = input$range, to = c(0,254))
      }

      return(in_ras)

    }))

    # Write output
    raster::writeRaster(ras_stack, filename = out_file, datatype = "INT1U", overwrite = overwrite)

    if(file.exists(out_file)){
      return("Success")
    }else{
      stop("Failed to create tile")
    }
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


.rescale <- function(x, from, to){
  out <- (x - from[1]) / (from[2] - from[1]) * (to[2] - to[1]) + to[1]
  out[out<to[1]] <- to[1]
  out[out>to[2]] <- to[2]
  return(out)
}