#' Retile dataset
#'
#' @export

Retile <- function(in_files, out_RSDS, res, bands = NULL,
                   tileNames = NULL, overwrite = FALSE){

  tim <- .headline("RETILE RS DATASET")

  ### CHECK INPUTS ----

  if(!all(file.exists(in_files))) stop("Some input files were missing")

  # Set file paths
  tempList <- tempfile(fileext = ".txt")
  tempVRT  <- tempfile(fileext = ".vrt")

  # # Make a list of a bunch of VRTs? Why do this?
  # if(makeVRTlist){
  #
  #   tempVRTdir <- file.path(tempdir(), "VRTlist")
  #   dir.create(tempVRTdir)
  #
  #   for(inFile in in_files){
  #     tempFile <- file.path(tempVRTdir, paste0(tools::file_path_sans_ext(basename(inFile)), ".vrt"))
  #     gpal2::gdalbuildvrt(tempFile, inFile)
  #   }
  #   in_files <- list.files(tempVRTdir, full.names = TRUE)
  # }

  # Write list of input files to a text file
  write(in_files, tempList)

  # Format bands argument
  bands <- if(!is.null(bands)) as.list(setNames(bands, rep("b", length(bands))))

  # List of VRT arguments
  # NOTE: 'srcnodata = "None"' prevents No Data values from being included
  argList <- c(list(input_file_list = tempList), bands, list(srcnodata = "None"), tempVRT)

  # Generate VRT
  do.call(gpal2::gdalbuildvrt, argList)

  if(!file.exists(tempVRT)) stop("Failed to create VRT")

  # Get tiles
  ts <- .get_tilescheme()

  # Get tile names
  out_files <- .get_RSDS_tilepaths(out_RSDS)


  ### CREATE WORKER ----

  worker <- function(tileName){

    # Get tile
    tile <- ts[tileName,]

    # Resample
    gpal2::gdalwarp(
      t_srs     = as.character(tile@crs),
      te        = raster::extent(tile[["buffs"]]),
      tr        = c(res, res),
      r         = "bilinear",
      overwrite = overwrite,
      tempVRT,
      out_files[tileName]
    )

    if(!file.exists(out_files[tileName])) stop("Failed to create output for tile '", tileName, "'")

    return("Success")
  }

  ### APPLY WORKER ----

  # Get tiles for processing
  procTiles <- .processing_tiles(out_files, overwrite, tileNames)

  # Process
  status <- .doitlive(procTiles, worker)

  # Report
  .statusReport(status)

  # Conclude
  .conclusion(tim)

}
