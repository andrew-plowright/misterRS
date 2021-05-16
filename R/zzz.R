#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%

# Print messages with package is attached
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Mister Remote Sensing")
}

# Run these functions when package is loaded
.onLoad <- function(libname, pkgname){

  # Detect Orfeo Toolbox path
  pfdirs <- list.dirs("C:/Program Files", recursive = FALSE)
  orfeoPath <- pfdirs[grepl("/OTB", pfdirs)][1]
  if(is.na(orfeoPath)) warning("No directory found for Orfeo Toolbox")

  # Set global options
  op.current <- options()
  op.misterRS <- list(
    misterRS.clusters = 1,
    misterRS.orfeo = orfeoPath
  )
  toset <- !(names(op.misterRS) %in% names(op.current))
  if(any(toset)) options(op.misterRS[toset])
}
