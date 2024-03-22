#' @export
misterRS_config <- function(config_file, override = TRUE){

  config <- jsonlite::read_json(config_file)

  # Set global options
  op.current <- options()
  op.misterRS <- list(
    misterRS.orfeo            = config$orfeo_dir,
    misterRS.projects_root    = config$projects_root,
    misterRS.project_register = config$projects_log,
    misterRS.syncback_exe     = config$syncback_exe,
    misterRS.mod_spatialite   = config$mod_spalialite,
    misterRS.winrar           = config$winrar
  )
  if(override){
    options(op.misterRS)
  }else{
    toset <- !(names(op.misterRS) %in% names(op.current))
    if(any(toset)) options(op.misterRS[toset])
  }
}
