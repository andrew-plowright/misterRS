#' Make LAX files for a LAS dataset
#'
#' @export

lax <- function(in_cat){

  # Get initial list of LAS files
  las_files <- in_cat@data[["filename"]]
  if(length(las_files) == 0) stop("Did not find any LAS files in this LAS dataset", call. = FALSE)

  # Get list of LAX files
  lax_files <- gsub("\\.las$|\\.laz$", "\\.lax", las_files)

  # Subset only those LAS files without lax files
  las_files <- las_files[!file.exists(lax_files)]
  if(length(las_files) == 0) stop("All lax files already created", call. = FALSE)

  pb <- .progbar(length(las_files))

  for(las_file in las_files){
    capture.output(rlas::writelax(las_file), type = "message")
    pb$tick()
  }
}


