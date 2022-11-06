#' Generate pyramids
#'
#' @export

pyramids <- function(rsds, overwrite = FALSE){

  .check_extension(rsds, "tif")

  process_timer <- .headline("BUILD PYRAMIDS")

  # Existing RSDS files
  existingFiles <- rsds@tilePaths[file.exists(rsds@tilePaths)]

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
    "  Processing : ", length(procFiles), "/", length(rsds@tilePaths), " tiles", "\n",
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

  .conclusion(process_timer)

}
