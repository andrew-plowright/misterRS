#' Detect trees
#'
#' Uses the variable window filter (VWF) algorithm
#'
#' @export

detect_trees <- function(chm_rts, ttops_vts, win_fun, min_hgt, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("DETECT TREES")

  # Do not attempt to write to Geopackage using multiple clusters
  withr::local_options("misterRS.cluster" = 1)

  ### INPUT CHECKS ----

  # Check that inputs are complete
  .complete_input(chm_rts)

  # Get tile scheme
  ts <- .tilescheme()

  # Get projection
  proj <- getOption("misterRS.crs")

  # Write function
  win_fun_text <- deparse(win_fun)[2]
  win_fun_text_path <- file.path(R.utils::getAbsolutePath(ttops_vts$dir), "win_fun.txt")
  write(win_fun_text, win_fun_text_path)

  ttops_vts$connect()

  # Add height field
  ttops_vts$add_field("height", "REAL")

  ttops_vts$disconnect()

  ### CREATE WORKER ----

  tile_worker <-function(tile_name){

    # Get tile and buff
    tile <- ts[tile_name][["nbuffs"]]

    # Read CHM
    CHM  <- terra::rast(chm_rts$tile_path(tile_name))

    # Get CHM range
    CHM_rng <- terra::minmax(CHM, compute = TRUE)[,1, drop = TRUE]

    # Check if CHM has usable values
    if(any(!is.finite(CHM_rng))){

      # Empty treetops with values
      det_ttops <- sf::st_sf(
        geom = sf::st_sfc(crs = sf::st_crs( proj)),
        list(tree_id = integer(), height = numeric())
      )

    }else{

      # Detect new treetops
      det_ttops <- ForestTools::vwf(CHM, win_fun, min_hgt, IDfield = "tree_id")

      # Set geometry column name
      sf::st_geometry(det_ttops) <- "geom"

      # Drop 'winRadius' field
      det_ttops <- det_ttops[,setdiff(names(det_ttops), "winRadius")]

      # Subset treetops
      det_ttops <- det_ttops[lengths(sf::st_intersects(det_ttops, tile)) > 0,]

    }

    ttops_vts$write_geom_tile(det_ttops, tile_name)

    return("Success")
  }

  ### APPLY WORKER ----
  ttops_vts %>%
    .tile_queue("geom") %>%
    .exe_tile_worker(tile_worker, cluster_vts = "ttops_vts") %>%
    .print_process_status()

  # Create index
  ttops_vts$index()

  # Conclude
  .conclusion(process_timer)
}
