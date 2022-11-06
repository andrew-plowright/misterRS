#' Detect trees
#'
#' Uses the variable window filter (VWF) algorithm
#'
#' @export

detect_trees <- function(chm_rsds, ttops_rsds, win_fun, min_hgt,
                tile_names = NULL, overwrite = FALSE){

  process_timer <- .headline("DETECT TREES")

  ### INPUT CHECKS ----

  # Check extensions
  .check_extension(chm_rsds,   "tif")
  .check_extension(ttops_rsds, "shp")

  # Check that inputs are complete
  .check_complete_input(chm_rsds, tile_names)

  # Get tile scheme
  ts <- .get_tilescheme()

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
    CHM  <- raster::raster(CHM_path)

    # Function for creating blank treetop SHP file
    noTrees <- function(e){
      sp::SpatialPointsDataFrame(
        sp::SpatialPoints(data.frame(x = 0, y = 0), proj4string = raster::crs(CHM))[-1,],
        data.frame(height = numeric(), winRadius = numeric(), treeID = integer())
      )}

    # Detect new treetops
    det_ttops <- tryCatch(ForestTools::vwf(CHM, win_fun, min_hgt), error = noTrees)

    # Save output
    APfun::APSHPsave( det_ttops, out_path, overwrite = overwrite)

    if(file.exists(out_path)){
      return("Success")
    }else{
      stop("Failed to create tile")
    }
  }


  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- misterRS:::.tile_queue(out_paths, overwrite, tile_names)

  # Process
  process_status <- misterRS:::.exe_tile_worker(queued_tiles, tile_worker)

  # Report
  misterRS:::.print_process_status(process_status)

  # Conclude
  misterRS:::.conclusion(process_timer)
}
