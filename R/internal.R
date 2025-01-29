.complete_input <- function(xts, buffered = FALSE, attribute = NULL, selected_tiles = getOption("misterRS.tile_names")){

  xts_class <- class(xts)

  ts <- .tilescheme()

  # Get selected tiles
  if(is.null(selected_tiles)){

    # If set to NULL, process all tiles
    selected_tiles <- ts[["tile_name"]]

  }else{

    # If specific tiles have been selected, verify that they exist in the tile scheme
    tiles_dont_exist <- !selected_tiles %in% ts[["tile_name"]]
    if(any(tiles_dont_exist)) stop("Following tile names do not exist:\n  ", paste(selected_tiles[tiles_dont_exist], collapse = "\n  "))

    if(buffered) selected_tiles <- .tile_neibs(selected_tiles, ts)
  }

  if("vts" %in% xts_class){

    if(is.null(attribute)) stop("Define an attribute for VTS")

    if(!xts$complete(attribute, tile_names = selected_tiles)) stop("Input VTS '", xts$name, "' is incomplete", call. = FALSE)

  }else if("rts" %in% xts_class){

    if(!xts$complete(tile_names = selected_tiles)) stop("Input RTS '", xts$name, "' is incomplete", call. = FALSE)

  }else stop("Invalid input")

}

# Text to print at the end of a process (i.e.: how long the process took)

.conclusion <- function(time){

  total_time <- as.numeric(difftime(Sys.time(), time, units="secs"))

  total_time_str <- sprintf("%02d:%02d:%02d", total_time %% 86400 %/% 3600, total_time %% 3600 %/% 60, total_time %% 60 %/% 1)

  if(total_time >= 86400){

    n_days <- total_time %/% 86400

    total_time_str <- paste0(total_time_str, " + ", n_days, " day(s)")
  }

  cat(
    "\n",
    "Finished at : ", format(Sys.time(), "%Y-%m-%d %X"), "\n",
    "Total time  : ", total_time_str,
    "\n\n", sep = "")
}


#' Set local options

.env_misterRS <- function(options){

  if(length(options) > 0){

    names(options) <- paste0("misterRS.", names(options))

    withr::local_options(options, .local_envir = parent.frame())
  }
}


#' Run a worker in parallel or serial
#'
#'
#' @importFrom foreach %do%
#' @importFrom foreach %dopar%

.exe_tile_worker <-function(tile_names, worker, cluster_vts = NULL, clusters = getOption("misterRS.clusters")){

  # ASsign the "overwite" value to the parent frame, which in turn gets passed to the worker functions
  # assign("overwrite", getOption("misterRS.overwrite"), env = parent.frame())

  if(length(tile_names) == 0){

    return(NULL)

  }else{

    # Make progress bar
    pb <- .progbar(length(tile_names))

    # If only running one cluster or if only one tile needs processing, all tiles are executed in serial
    if(clusters == 1 | length(tile_names) == 1){

      # Wrap worker in tryCatch
      worker_ser <- function(tile_name){
        r <- tryCatch({ worker(tile_name) }, error = function(e){e})
        pb$tick()
        return(r)
      }

      # Execute function at the start of the process
      #if(!is.null(cluster_vts)) get(cluster_vts, env = parent.frame())$connect()

      # Generate 'foreach' statement
      fe_ser <- foreach::foreach(tile_name = tile_names, .errorhandling = 'pass')

      # Execute in serial
      results <- fe_ser %do% worker_ser(tile_name)

      # Execute functions at the end of the process
      #if(!is.null(cluster_vts)) get(cluster_vts, env = parent.frame())$disconnect()

    # Otherwise, execute in parallel
    }else{

      # Wrap worker in tryCatch
      worker_par <- function(tile_name){
        r <- tryCatch({ worker(tile_name) }, error = function(e){e})
        return(r)
      }

      # Register clusters
      cl <- parallel::makeCluster(clusters)
      doSNOW::registerDoSNOW(cl)

      # Force the evaluation of all arguments given in the parent frame
      # NOTE: Because R uses 'lazyload', some arguments (or "promises") given in the original function
      # are not evaluated immediately. For whatever reason, %dopar% doesn't like this, and is unable
      # to evaluate these promises. The two lines below "forces" their evaluation.

      # Also note that the worker function, whose environment is sys.frame(-1), has access to these
      # variables
      func_args <- setdiff(names(formals(sys.function(-3))), "...")
      for(func_args in func_args) eval(parse(text = func_args),  sys.frame(-3))

      #func_args <- names(formals(sys.function(-1))) %>% setdiff("...")
      #parallel::clusterExport(cl, ls(envir = sys.frame(-1)), envir = sys.frame(-1))

      # if(!is.null(cluster_vts)){

        # Export cluster VTS
        # parallel::clusterExport(cl, cluster_vts, envir= parent.frame())

        # Connect VTS
        #parallel::clusterCall(cl, function(){get(cluster_vts, envir = parent.frame(1))$connect(); return(NULL)})

        # parallel::clusterEvalQ(cl, {DBI::dbIsValid(in_vts$con)})

        # Avoid namespace collisions by removing the 'cluster_vts' parent enfironment, which is where it would normally be retrieved
        # from. Instead, the worker function will then "look" into the cluster environment.
        # This is probably not a best practice
      #   rm(list=cluster_vts, envir= parent.frame())
      # }

      # Generate 'foreach' statement
      fe_par <- foreach::foreach(tile_name = tile_names, .errorhandling = 'pass', .options.snow = list(progress = function(n) pb$tick()))

      # Execute in parallel
      results <- fe_par %dopar% worker_par(tile_name)

      # if(!is.null(cluster_vts)){
      #
      #   parallel::clusterCall(cl, function(){get(cluster_vts, envir = parent.frame(1))$disconnect(); return(NULL)})
      # }

      parallel::stopCluster(cl)
    }

    results <- setNames(results, tile_names)

    return(results)
  }
}


.headline <- function(headline){

  cat(
    headline, "\n",
    "Started at : ", format(Sys.time(), "%Y-%m-%d %X"), "\n",
    "\n",sep = "")

  Sys.time()
}

.is_las_rgb <- function(in_las){

  # Are there R, G, B fields?
  has_rgb <- !is.null(in_las$R) & !is.null(in_las$G) & !is.null(in_las$B)

  if(has_rgb){

    # Do they contain only 0s? (Only check R to save time)
    return(lidR:::fast_countover(in_las$R, 0L) > 0)

  }else{
    return(FALSE)
  }
}


.progbar <- function(n, width = 80){

  if(n == 0){

    return(NULL)

  }else{

    pb <- progress::progress_bar$new(
      format = "  Progress [:bar] Tile :current/:total. ETA: :eta",
      total = n,
      width = width,
      show_after = 0)
    pb$tick(0)

    return(pb)
  }

}


.tilescheme <- function(ts = getOption("misterRS.ts")){

  if(is.null(ts)) stop("No tile scheme has been set. Use 'options('misterRS.ts')' to set one")

  return(ts)
}

.tile_neibs <- function(tile_names, ts, case = "queen"){

  mats <- if(case=="queen"){
    list(
      c(-1,-1), c(-1,0), c(-1,1),
      c( 0,-1), c( 0,0), c( 0,1),
      c( 1,-1), c( 1,0), c( 1,1)
    )
  }else{
    list(
      c(-1,0),
      c( 0,-1), c( 0,0), c( 0,1),
      c( 1,0)
    )
  }

  existing_tiles <- ts[["tile_name"]]

  selected_tiles_rc <- sf::st_drop_geometry(ts@sf)[existing_tiles %in% tile_names, c("row", "col")]

  potential_tiles <- lapply(1:nrow(selected_tiles_rc), function(i){
    lapply(mats, function(mat) selected_tiles_rc[i,] + mat)
  }) %>%
    unlist(recursive=FALSE) %>%
    do.call(rbind, .) %>%
    dplyr::distinct() %>%
    dplyr::mutate(tile_name = paste0("R", row, "C", col)) %>%
    `[[`("tile_name")

  neib_tiles <- potential_tiles[potential_tiles %in% existing_tiles]

  return(sort(neib_tiles))

}

.tile_queue <- function(xts, attribute_set_name = NULL,
                        overwrite = getOption("misterRS.overwrite"),
                        selected_tiles = getOption("misterRS.tile_names"),
                        clusters = getOption("misterRS.clusters"),
                        verbose = getOption("misterRS.verbose")){

  in_class <- class(xts)

  ts <- .tilescheme()

  # Get selected tiles
  if(is.null(selected_tiles)){

    # If set to NULL, process all tiles
    selected_tiles <- ts[["tile_name"]]

  }else{

    # If specific tiles have been selected, verify that they exist in the tile scheme
    tiles_dont_exist <- !selected_tiles %in% ts[["tile_name"]]
    if(any(tiles_dont_exist)) stop("Following tile names do not exist:\n  ", paste(selected_tiles[tiles_dont_exist], collapse = "\n  "))

  }

  # Choose which tiles to process
  proc_tiles <- selected_tiles

  # If not overwriting, subset only non-existent tiles
  if(length(proc_tiles) > 0 && !overwrite){

      # Arguments for detecting which tiles exist
      args <- list(tile_names = proc_tiles)
      if("vts" %in% in_class){ args[["attribute"]] <- attribute_set_name }

      # Determine which tiles existing
      has_tiles <-  do.call(xts$has_tiles, args)

      proc_tiles <- names(has_tiles)[!has_tiles]
  }

  if(verbose){
    cat(
      "  Overwrite        : ", overwrite, "\n",
      "  Clusters         : ", clusters, "\n",
      "  Total tiles      : ", length(ts), "\n",
      "  Selected tiles   : ", length(selected_tiles),  "\n",
      "  Queued tiles     : ", length(proc_tiles),      "\n",
      sep = ""
    )
  }

  return(proc_tiles)
}


.raster_files <- function(file_path){

  raster_extensions <- list(
    "tif" = c("tif.aux.xml", "aux.xml", "ovr", "tif.ovr", "tfw")
  )

  possible_files <- file_path

  in_ext <- tools::file_ext(file_path)

  if(in_ext %in% names(raster_extensions)){

    file_ext <- raster_extensions[[in_ext]]

    extended_files <- paste0(tools::file_path_sans_ext(file_path), ".", file_ext)

    possible_files <- c(possible_files, extended_files)

  }


  existing_files <- possible_files[file.exists(possible_files)]


  return(existing_files)
}

.read_poly_attributes <- function(path){

  fileext <- toupper(tools::file_ext(path))

  if(fileext == "GPKG"){

    gpkg_lyr <- sf::st_layers(path)$name[1]

    con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = path)
    qur <- RSQLite::dbSendQuery(con, sprintf("SELECT * FROM %s", gpkg_lyr))
    res <- RSQLite::dbFetch(qur)
    RSQLite::dbClearResult(qur)
    RSQLite::dbDisconnect(con)

    res <- res[,names(res) != "geom"]

  }else if(fileext == "SHP"){

    DBFpath <- gsub("\\.shp$", "\\.dbf", path)

    res <- foreign::read.dbf(DBFpath, as.is = TRUE)

  }else stop("Unrecognized file extension: '", fileext, "'")

  return(res)

}


.print_process_status <- function(status){

  if(length(status) == 0){

    cat(crayon::yellow("  Processed no tiles\n", sep = ""))

  }else{

    errors  <- sapply(status, function(s) "error" %in% class(s))
    success <- sapply(status, function(s) is.character(s) && s == "Success")
    warn    <- !errors & !success

    if(any(success)){

      cat(crayon::green("  Successful tiles : ", length(success[success]), "\n", sep = ""))
    }

    if(any(warn)){

      status_warn <- unlist(status[warn])
      unique_warn <- unique(status_warn)

      for(w in unique_warn){
        cat(crayon::yellow("\n  ", w, " :\n    ", paste(names(status_warn[status_warn == w]), collapse = "\n    "), "\n", sep = ""))
      }
    }

    if(any(errors)){

      status_err <- sapply(status[errors], function(e) e$message)
      unique_err <- unique(status_err)

      for(e in unique_err){
        cat(crayon::red("\n  ", e, " :\n    ", paste(names(status_err[status_err == e]), collapse = "\n    "), "\n", sep = ""))
      }
    }
  }
}


.compress <- function(src_files, dest_file){

  if(length(src_files) > 1) stop("Compressing multiple files is not supported")

  src_dir <- dirname(src_files)
  src_file <- basename(src_files)

  # Return to previous work directory
  return_dir <- getwd()
  withr::defer(setwd(return_dir))

  # Set work directory to source path
  setwd(src_dir)

  # Execute command
  #command <- sprintf('zip -j "%s" "%s"', dest_file, paste(src_files, collapse='"  "'))
  command <- sprintf('tar -czvf  "%s" "%s"', dest_file, src_file)

  system(command, ignore.stdout = TRUE, ignore.stderr = TRUE)

}


.normalize_las <- function(in_las, dem_path, z_min, z_max){

  if(!file.exists(dem_path)) stop("Could not find DEM file '", dem_path, "'")

  # Read segments and DEM
  dem <- terra::rast(dem_path)

  # Remove points that aren't on DEM

  dem_mask <- !is.na(dem)
  in_las <- lidR::merge_spatial(in_las, dem_mask,attribute = "onDEM")
  in_las <- lidR::filter_poi(in_las, onDEM == TRUE)

  # No LAS points over DEM file
  if(lidR::is.empty(in_las)) return(NULL)

  # Normalize
  in_las <- lidR::normalize_height(in_las, dem)

  # Filter LAS by height
  outLAS <- lidR::filter_poi(in_las, Z <= z_max & Z > z_min)

  # No LAS points within min/max Z bounds over DEM file
  if(lidR::is.empty(outLAS)) return(NULL)


  return(outLAS)

}



