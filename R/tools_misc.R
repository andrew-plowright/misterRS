

#' Make LAX files for a LAS dataset
#'
#' @export

LAX <- function(in_cat){

  # Get initial list of LAS files
  LASfiles <- in_cat@data[["filename"]]
  if(length(LASfiles) == 0) stop("Did not find any LAS files in this LAS dataset", call. = FALSE)

  # Get list of LAX files
  LAXfiles <- gsub("\\.las$|\\.laz$", "\\.lax", LASfiles)

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
