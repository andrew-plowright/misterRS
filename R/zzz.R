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

  # Path for projects
  projects_root = "D:/Projects/clients/diamondhead"
  projects_log = "D:/Projects/clients/diamondhead/project_register.csv"

  syncback_exe = "C:/Program Files (x86)/2BrightSparks/SyncBackFree/SyncBackFree.exe"

  # Set global options
  op.current <- options()
  op.misterRS <- list(
    misterRS.clusters      = 1,
    misterRS.orfeo         = orfeoPath,
    misterRS.verbosity     = TRUE,
    misterRS.crs           = NA,
    misterRS.ts            = NULL,
    misterRS.projects_root = projects_root,
    misterRS.project_register = projects_log,
    misterRS.syncback_exe  = syncback_exe,
    lidR.progress          = FALSE,
    lidR.verbose           = FALSE
  )
  toset <- !(names(op.misterRS) %in% names(op.current))
  if(any(toset)) options(op.misterRS[toset])
}
