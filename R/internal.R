.complete_input <- function(xts, buffered = FALSE, selected_tiles = getOption("misterRS.tile_names")){

  xts_class <- class(xts)

  ts <- .tilescheme()

  # Get selected tiles
  if(is.null(selected_tiles)){

    # If set to NULL, process all tiles
    selected_tiles <- ts$tileName

  }else{

    # If specific tiles have been selected, verify that they exist in the tile scheme
    tiles_dont_exist <- !selected_tiles %in% ts$tileName
    if(any(tiles_dont_exist)) stop("Following tile names do not exist:\n  ", paste(selected_tiles[tiles_dont_exist], collapse = "\n  "))

    if(buffered) selected_tiles <- .tile_neibs(selected_tiles, ts)
  }

  if(xts_class == "vts"){

    has_tiles <- .vts_has_tiles(xts, selected_tiles)

    incomplete <- length(has_tiles[!has_tiles]) > 0

  }else if(xts_class == "rts"){

    tile_paths <- .rts_tile_paths(xts)[selected_tiles]

    incomplete <- !all(file.exists(tile_paths))

  }else stop("Invalid input")

  if(incomplete) stop("Input RTS '", xts@name, "' is incomplete", call. = FALSE)
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
#' @param cluster_eval extra code that gets evaluated on each cluster
#'
#' @importFrom foreach %do%
#' @importFrom foreach %dopar%

.exe_tile_worker <-function(tile_names, worker, clusters = getOption("misterRS.clusters"), cluster_eval = NULL){

  # ASsign the "overwite" value to the parent frame, which in turn gets passed to the worker functions
  assign("overwrite", getOption("misterRS.overwrite"), env = parent.frame())

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


      # Execute extra code
      force(cluster_eval)

      # Generate 'foreach' statement
      fe_ser <- foreach::foreach(tile_name = tile_names, .errorhandling = 'pass')

      # Execute in serial
      results <- fe_ser %do% worker_ser(tile_name)

    }else{

      # Wrap worker in tryCatch
      worker_par <- function(tile_name){
        r <- tryCatch({ worker(tile_name) }, error = function(e){e})
        return(r)
      }

      # Register clusters
      cl <- parallel::makeCluster(clusters)
      doSNOW::registerDoSNOW(cl)
      withr::defer({
        parallel::stopCluster(cl)
      })

      # Force the evaluation of all arguments given in the parent frame
      # NOTE: Because R uses 'lazyload', some arguments (or "promises") given in the original function
      # are not evaluated immediately. For whatever reason, %dopar% doesn't like this, and is unable
      # to evaluate these promises. The two lines below "forces" their evaluation.
      func_args <- names(formals(sys.function(-1))) %>% setdiff("...")
      parallel::clusterExport(cl, func_args, envir = sys.frame(-1))
      #for(func_args in func_args) eval(parse(text = func_args),  sys.frame(-1))

      # Execute extra code
      parallel::clusterEvalQ(cl, cluster_eval)

      # Generate 'foreach' statement
      fe_par <- foreach::foreach(tile_name = tile_names, .errorhandling = 'pass', .options.snow = list(progress = function(n) pb$tick()))

      # Execute in parallel
      results <- fe_par %dopar% worker_par(tile_name)

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

.is_las_rgb <- function(inLAS){

  # Are there R, G, B fields?
  has_rgb <- !is.null(inLAS$R) & !is.null(inLAS$G) & !is.null(inLAS$B)

  if(has_rgb){

    # Do they contain only 0s? (Only check R to save time)
    return(lidR:::fast_countover(inLAS$R, 0L) > 0)

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
      c( 1,0),
    )
  }

  tiles <- ts@data[ts@data$tileName %in% tile_names, ]

  potential_tiles <- lapply(1:nrow(tiles), function(i){lapply(mats, function(mat) tiles[i,c("row", "col")] + mat)}) %>%
    unlist(recursive=FALSE) %>%
    do.call(rbind, .) %>%
    dplyr::distinct() %>%
    dplyr::mutate(tileName = paste0("R", row, "C", col)) %>%
    `[[`("tileName")

  existing_tiles <- ts@data[["tileName"]]

  neib_tiles <- potential_tiles[potential_tiles %in% existing_tiles]

  return(neib_tiles)

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
    selected_tiles <- ts$tileName

  }else{

    # If specific tiles have been selected, verify that they exist in the tile scheme
    tiles_dont_exist <- !selected_tiles %in% ts$tileName
    if(any(tiles_dont_exist)) stop("Following tile names do not exist:\n  ", paste(selected_tiles[tiles_dont_exist], collapse = "\n  "))

  }

  # Choose which tiles to process
  proc_tiles <- selected_tiles

  # If not overwriting, subset only non-existent tiles
  if(length(proc_tiles) > 0){

    if(in_class == "vts"){

      if(!overwrite){

        has_tiles <- .vts_has_tiles(xts, proc_tiles, attribute_set_name)

        proc_tiles <- names(has_tiles)[!has_tiles]
      }


    }else if(in_class == "rts"){

      if(!overwrite){

        tile_paths   <- .rts_tile_paths(xts)

        proc_tiles <- proc_tiles[!file.exists(tile_paths[proc_tiles])]
      }

    }else stop("Invalid input")
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

    con <-  RSQLite::dbConnect(RSQLite::SQLite(), dbname = path)
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


.rar <- function(src_files, dest_file, winrar_exe = getOption("misterRS.winrar")){

  command <- sprintf('"%s" a -ep1 -r "%s" "%s"', winrar_exe, dest_file, paste(src_files, collapse='"  "'))
  system(command)
}



.rts_tile_paths <- function(rts){

  # Get tile scheme
  ts <- .tilescheme()

  # Get file paths
  tilePaths <- file.path(rts@dir, "tiles", paste0(ts$tileName, ".", rts@ext))

  # Get absolute path
  tilePaths <- suppressMessages(R.utils::getAbsolutePath(tilePaths))

  # Set names
  tilePaths <- setNames(tilePaths, ts$tileName)

  return(tilePaths)

}


.rts_mosaic_path <- function(rts){

  ext <- if( rts@ext == 'shp') 'gpkg' else rts@ext

  # Get file path
  mosaic_path <- file.path(rts@dir, paste0(rts@id, ".", ext))

  # Get absolute path
  mosaic_path <- suppressMessages(R.utils::getAbsolutePath(mosaic_path))

  return(mosaic_path)
}

.normalize_las <- function(inLAS, DEM_path, z_min, z_max){

  if(!file.exists(DEM_path)) stop("Could not find DEM file '", DEM_path, "'")

  # Read segments and DEM
  DEM <- terra::rast(DEM_path)

  # Remove points that aren't on DEM

  DEM_mask <- !is.na(DEM)
  inLAS <- lidR::merge_spatial(inLAS, DEM_mask,attribute = "onDEM")
  inLAS <- lidR::filter_poi(inLAS, onDEM == TRUE)

  # No LAS points over DEM file
  if(lidR::is.empty(inLAS)) return(NULL)

  # Normalize
  inLAS <- lidR::normalize_height(inLAS, DEM)

  # Filter LAS by height
  outLAS <- lidR::filter_poi(inLAS, Z <= z_max & Z > z_min)

  # No LAS points within min/max Z bounds over DEM file
  if(lidR::is.empty(outLAS)) return(NULL)


  return(outLAS)

}



