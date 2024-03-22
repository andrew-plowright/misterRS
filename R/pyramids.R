#' Generate pyramids
#'
#' @export

pyramids <- function(in_rts, overwrite = FALSE){

  process_timer <- .headline("BUILD PYRAMIDS")

  # Existing RTS files
  existingFiles <- in_rts@tilePaths[file.exists(in_rts@tilePaths)]

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
    "  Processing : ", length(procFiles), "/", length(in_rts@tilePaths), " tiles", "\n",
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
      procFile
    )

    pb$tick()
  }

  .conclusion(process_timer)

}
