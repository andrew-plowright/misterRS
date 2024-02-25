local_misterRS_env <- function(envir = parent.frame()) {

  #dir <- withr::local_tempdir(.local_envir = envir)

  withr::local_options(list(
    misterRS.clusters   = 1,
    misterRS.verbosity  = FALSE,
    misterRS.crs        = 26917,
    misterRS.ts = readRDS("test_rsds/tilescheme/tile_scheme.rds")
  ),
  .local_envir = envir)

}
