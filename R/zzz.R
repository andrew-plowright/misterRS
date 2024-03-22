#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%

# Print messages with package is attached
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Mister Remote Sensing")
}

# Run these functions when package is loaded
.onLoad <- function(libname, pkgname){

  # Get directory
  lib_dir <- Sys.getenv("MISTERRS")
  if(lib_dir == "") warning(
    "Could not find the MISTERRS system variable. ",
    "Set this variable to the folder that contains ",
    "the 'misterRS_config.json' configuration file.")

  # Load config
  config_file <- file.path(lib_dir, "misterRS_config.json")

  if(file.exists(config_file)){
    misterRS_config(config_file, override = FALSE)
  }else{
    warning("Couldn't find config file. Use misterRS_config() to load configuration.'")
  }

  # Set global options
  op.current <- options()
  op.misterRS <- list(
    misterRS.clusters     = 1,
    misterRS.verbose      = TRUE,
    misterRS.overwrite    = FALSE,
    misterRS.tile_names   = NULL,
    misterRS.crs          = NA,
    misterRS.ts           = NULL,
    lidR.progress         = FALSE,
    lidR.verbose          = FALSE
  )
  toset <- !(names(op.misterRS) %in% names(op.current))
  if(any(toset)) options(op.misterRS[toset])

  # Terra options
  terra::terraOptions(progress=0)
}
