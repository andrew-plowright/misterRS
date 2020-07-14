
#' Mask
#'
#' @export

Mask <- function(in_RSDS, out_RSDS, mask_RSDS,
                 tileNames = NULL, clusters = 1, overwrite = FALSE, useNeighbours = FALSE){


  tim <- .headline("MASK RSDS")

  ### INPUT CHECKS ----

  # Check that all RSDS have same tileScheme
  .check_same_ts(in_RSDS, out_RSDS, mask_RSDS)

  # Check extensions
  .check_extension(in_RSDS,   "tif")
  .check_extension(out_RSDS,  "tif")
  .check_extension(mask_RSDS, "tif")

  # Check that inputs are complete
  .check_complete_input(in_RSDS,   tileNames)
  .check_complete_input(mask_RSDS, tileNames)


  ### CREATE WORKER ----

  # Run process
  worker <- function(tileName){

    # Set output file
    outFile <- out_RSDS@tilePaths[tileName]

    # Read input raster
    inRas   <- raster::raster(in_RSDS@tilePaths[tileName])

    # Read mask file
    maskRas <- if(useNeighbours){

      maskPaths <- mask_RSDS@tilePaths[.tileNeighbours(mask_RSDS@tileScheme, tileName)$tileName]
      masks <- lapply(maskPaths, raster::raster)
      maskRas <- do.call(raster::mosaic, c(unname(masks), list(fun = max)))
      raster::crop(maskRas, mask_RSDS@tileScheme[tileName][["buffs"]])

    }else{

      raster::raster(mask_RSDS@tilePaths[tileName])

    }

    # Apply mask
    inRas[maskRas != 1] <- NA

    # Save output
    raster::writeRaster(inRas, outFile, overwrite = overwrite)

    if(file.exists(outFile)) "Success" else stop("Failed to create output")

  }

  ### APPLY WORKER ----

  # Get tiles for processing
  procTiles <- .processing_tiles(out_RSDS, overwrite, tileNames)

  # Process
  status <- .doitlive(procTiles, clusters, worker)

  # Report
  .statusReport(status)

  # Conclude
  .conclusion(tim)

}




#' Hillshade
#'
#' @export

Hillshade <- function(RSDS){

  tim <- .headline("HILLSHADE")

  in_file <- RSDS@mosaic

  if(!file.exists(in_file)) stop("No mosaic file found")

  out_file <- gsub("\\.tif$",  "_hillshade.tif", rsds$DEM_2018@mosaic)

  if(file.exists(out_file)){

    if(overwrite){

      del_files <- c(APfun::APrasterFiles(in_file), paste0(in_file, ".ovr"))
      unlink(del_files)

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
    out_file,
    c(2,4,8,16,32,64)
  )

  .conclusion(tim)

}

#' Make LAX files for a LAS dataset
#'
#' @export

LAX <- function(RSDS){

  # Get initial list of LAS files
  LASfiles <- RSDS@tilePaths[grepl("\\.las$", RSDS@tilePaths)]
  if(length(LASfiles) == 0) stop("Did not find any LAS files in this LAS dataset", call. = FALSE)

  # Get list of LAX files
  LAXfiles <- gsub("\\.las$", "\\.lax", LASfiles)

  # Subset only those LAS files without LAX files
  LASfiles <- LASfiles[!file.exists(LAXfiles)]
  if(length(LASfiles) == 0) stop("All LAX files already created", call. = FALSE)

  pb <- .progbar(length(LASfiles))

  for(LASfile in LASfiles){
    capture.output(rlas::writelax(LASfile), type = "message")
    pb$tick()
  }
}


#' Generate pyramids
#'
#' @export

Pyramids <- function(RSDS, overwrite = FALSE){

  .check_extension(RSDS, "tif")

  tim <- .headline("BUILD PYRAMIDS")

  # Existing RSDS files
  existingFiles <- RSDS@tilePaths[file.exists(RSDS@tilePaths)]

  # Pyramid paths
  OVRfiles <- gsub("\\.tif$", "\\.ovr", existingFiles)

  if(overwrite){
    unlink(OVRfiles[file.exists(OVRfiles)])
    procFiles <- existingFiles
  }else{
    procFiles <- existingFiles[!file.exists(OVRfiles)]
  }

  cat(
    "  Overwrite  : ", overwrite, "\n",
    "  Processing : ", length(procFiles), "/", length(RSDS@tilePaths), " tiles", "\n",
    sep = ""
  )

  # Create progress bar
  pb <- .progbar(length(procFiles))

  # Run process
  for(procFile in procFiles){

    # Resample
    gpal2::gdaladdo(
      r = "average",
      ro = TRUE,
      procFile,
      c(2,4,8,16,32,64)
    )

    pb$tick()
  }

  .conclusion(tim)

}



#' Retile dataset
#'
#' @export

Retile <- function(inFiles, out_RSDS, res, bands = NULL, tileNames = NULL, clusters = 1, overwrite = FALSE, makeVRTlist = FALSE){

  tim <- .headline("RETILE RS DATASET")

  ### CHECK INPUTS ----

    if(!all(file.exists(inFiles))) stop("Some input files were missing")

    # Set file paths
    tempList <- tempfile(fileext = ".txt")
    tempVRT  <- tempfile(fileext = ".vrt")

    if(makeVRTlist){

      tempVRTdir <- file.path(tempdir(), "VRTlist")
      dir.create(tempVRTdir)

      for(inFile in inFiles){
        tempFile <- file.path(tempVRTdir, paste0(tools::file_path_sans_ext(basename(inFile)), ".vrt"))
        gpal2::gdalbuildvrt(tempFile, inFile)
      }
      inFiles <- list.files(tempVRTdir, full.names = TRUE)
    }

    # Write list of input files to a text file
    write(inFiles, tempList)

    # Format bands argument
    bands <- if(!is.null(bands)) as.list(setNames(bands, rep("b", length(bands))))

    # List of VRT arguments
    argList <- c(list(input_file_list = tempList), bands, tempVRT)

    # Generate VRT
    do.call(gpal2::gdalbuildvrt, argList)

    if(!file.exists(tempVRT)) stop("Failed to create VRT")

  ### CREATE WORKER ----

    worker <- function(tileName){

      # Get tile
      tile <- out_RSDS@tileScheme[tileName,]

      # Resample
      gpal2::gdalwarp(
        t_srs     = as.character(tile@crs),
        te        = raster::extent(tile[["buffs"]]),
        tr        = c(res, res),
        r         = "bilinear",
        overwrite = overwrite,
        tempVRT,
        out_RSDS@tilePaths[tileName]
      )

      if(!file.exists(out_RSDS@tilePaths[tileName])) stop("Failed to create output for tile '", tileName, "'")

      return("Success")
    }

  ### APPLY WORKER ----

    # Get tiles for processing
    procTiles <- .processing_tiles(out_RSDS, overwrite, tileNames)

    # Process
    status <- .doitlive(procTiles, clusters, worker)

    # Report
    .statusReport(status)

    # Conclude
    .conclusion(tim)

}

#' Zip a RSDS by chunks
#'
#' @export

ZipChunk <- function(RSDS, deliveryDir, fileprefix, chunkSize = 100, zipExe = "C:/RBuildTools/3.5/bin/zip.exe"){

  deliveryDir <- tools::file_path_as_absolute(deliveryDir)

  # Set workspace (required for zipping)
  # old_wd  <- getwd()
  # setwd(dirname(RSDS@tilePaths[1]))
  # on.exit(setwd(old_wd))

  chunks <- split(RSDS@tilePaths, ceiling(seq_along(RSDS@tilePaths)/chunkSize))

  for(i in 1:length(chunks)){

    out_file <- file.path(deliveryDir, paste0(fileprefix, i, ".zip"))

    zip(
      zipfile = out_file,
      files   = chunks[[i]],
      extras  = "-j -q",
      zip     = zipExe)

  }

}
