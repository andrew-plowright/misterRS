#' Detect trees
#'
#' Uses the variable window filter (VWF) algorithm
#'
#' @export

detect_trees <- function(chm_rsds, ttops_rsds, win_fun, min_hgt, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("DETECT TREES")

  ### INPUT CHECKS ----

  # Check extensions
  .check_extension(chm_rsds,   "tif")
  .check_extension(ttops_rsds, "shp")

  # Check that inputs are complete
  .check_complete_input(chm_rsds)

  # Get tile scheme
  ts <- .get_tilescheme()

  # Get projection
  proj <- getOption("misterRS.crs")

  # Get file paths
  CHM_paths <- .get_rsds_tilepaths(chm_rsds)
  out_paths <- .get_rsds_tilepaths(ttops_rsds)

  # Write function
  win_fun_text <- deparse(win_fun)[2]
  win_fun_text_path <- file.path(R.utils::getAbsolutePath(ttops_rsds@dir), "win_fun.txt")
  write(win_fun_text, win_fun_text_path)

  ### CREATE WORKER ----

  tile_worker <-function(tile_name){

    # Paths
    CHM_path  <- CHM_paths[tile_name]
    out_path  <- out_paths[tile_name]

    # Get tile and buff
    tile <- ts[tile_name][["tiles"]]
    buff <- ts[tile_name][["buffs"]]

    # Read raster
    CHM  <- terra::rast(CHM_path)

    # Function for creating blank treetop SHP file
    no_ttops <- sf::st_sf(
      geometry = sf::st_sfc(crs = sf::st_crs( proj)),
      list(treeID = integer(), height = numeric(), winRadius = numeric())
    )

    # Detect new treetops
    det_ttops <- tryCatch(ForestTools::vwf(CHM, win_fun, min_hgt), error = function(e) no_ttops)

    # Save output
    sf::st_write(det_ttops, out_path, delete_dsn = overwrite, quiet = TRUE)

    if(file.exists(out_path)){
      return("Success")
    }else{
      stop("Failed to create tile")
    }
  }


  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_paths)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)
}
