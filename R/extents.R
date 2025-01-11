#' Convert frame copy/pasted from QGIS into an extent
#'
#' @export

qgis_frame_to_ext <- function(frame){

  # frame: copy/pasted from QGIS. ex.: "359613.2, 5505729.0 : 360004.0, 5506055.7"

  bounds = as.numeric(unlist(strsplit(strsplit(frame, " : ")[[1]], ", ")))
  xmin = bounds[1]
  xmax = bounds[3]
  ymin = bounds[2]
  ymax = bounds[4]

  return(terra::ext(xmin, xmax, ymin, ymax))
}


#' Convert extent into string for subsetting LAS
#'
#' @export

ext_to_las_filt <- function(ext){

  return(paste0("-keep_xy ", ext$xmin, " ", ext$xmax, " ", ext$ymin, " ", ext$ymax))
}
