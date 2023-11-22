#' Convert raster file(s) to tiled RS dataset
#'
#' @export

to_rsds <- function(in_files, out_rsds, res, bands = NULL, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("CONVERT TO RS DATASET")

  ### CHECK INPUTS ----

  # Check input file existence
  if(!all(file.exists(in_files))) stop("Some input files were missing")

  # Get file extension
  in_ext <- tolower(unique(tools::file_ext(unique(in_files))))
  if(length(in_ext) > 1) stop("Cannot use multiple file extensions")

  # Get tiles
  ts <- .get_tilescheme()
  crs <- getOption('misterRS.crs')

  # Get tile names
  out_files <- .get_rsds_tilepaths(out_rsds)

  if(in_ext == "tif"){

    # Set file paths
    temp_list <- tempfile(fileext = ".txt")
    temp_vrt  <- tempfile(fileext = ".vrt")

    # Write list of input files to a text file
    write(in_files, temp_list)

    # Format bands argument
    bands <- if(!is.null(bands)) as.list(setNames(bands, rep("b", length(bands))))

    # List of VRT arguments
    # NOTE: 'srcnodata = "None"' prevents No Data values from being included
    arg_list <- c(list(input_file_list = temp_list), bands, list(srcnodata = "None"), temp_vrt)

    # Generate VRT
    do.call(gpal2::gdalbuildvrt, arg_list)

    if(!file.exists(temp_vrt)) stop("Failed to create VRT")

  }else if(in_ext == "vrt"){

    if(length(in_files) > 1) stop("It may be possible to use multiple VRTs, but this hasn't been tested yet")

    temp_vrt <- in_files

  }else stop("Unusable file extension: '", in_ext, "'")


  ### CREATE WORKER ----

  tile_worker <-function(tile_name){

    # Get tile
    tile <- sf::st_as_sf(ts[tile_name,])

    # Resample
    gpal2::gdalwarp(
      t_srs     = paste0("EPSG:", crs),
      te        = terra::ext(tile[["buffs"]]),
      tr        = c(res, res),
      r         = "bilinear",
      overwrite = overwrite,
      temp_vrt,
      out_files[tile_name]
    )

    if(!file.exists(out_files[tile_name])) stop("Failed to create output for tile '", tile_name, "'")

    return("Success")
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
