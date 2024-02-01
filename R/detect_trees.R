#' Detect trees
#'
#' Uses the variable window filter (VWF) algorithm
#'
#' @export

detect_trees <- function(chm_rts, ttops_vts, win_fun, min_hgt, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("DETECT TREES")

  # Override parallel processing
  withr::local_options("misterRS.clusters" = 1)

  ### INPUT CHECKS ----

  # Check that inputs are complete
  .complete_input(chm_rts)

  # Get tile scheme
  ts <- .tilescheme()

  # Get projection
  proj <- getOption("misterRS.crs")

  # Get file paths
  CHM_paths <- .rts_tile_paths(chm_rts)

  # Write function
  win_fun_text <- deparse(win_fun)[2]
  win_fun_text_path <- file.path(R.utils::getAbsolutePath(ttops_vts@dir), "win_fun.txt")
  write(win_fun_text, win_fun_text_path)

  ### CREATE WORKER ----

  tile_worker <-function(tile_name){

    # Get tile and buff
    tile <- sf::st_as_sf(ts[tile_name][["nbuffs"]])

    # Read CHM
    CHM  <- terra::rast(CHM_paths[tile_name])

    # Get CHM range
    CHM_rng <- terra::minmax(CHM, compute = TRUE)[,1, drop = TRUE]

    # Check if CHM has usable values
    if(any(!is.finite(CHM_rng))){

      # Empty treetops with values
      det_ttops <- sf::st_sf(
        geometry = sf::st_sfc(crs = sf::st_crs( proj)),
        list(treeID = integer(), height = numeric(), winRadius = numeric())
      )

    }else{

      # Detect new treetops
      det_ttops <- ForestTools::vwf(CHM, win_fun, min_hgt)

      # Subset treetops
      det_ttops <- det_ttops[lengths(sf::st_intersects(det_ttops, tile)) > 0,]

    }

    .vts_write(in_sf = det_ttops, out_vts = ttops_vts, tile_name = tile_name, overwrite = overwrite)

    return("Success")
  }


  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(ttops_vts)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)
}
