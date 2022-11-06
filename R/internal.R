.check_complete_input <- function(RSDS, tile_names = NULL){

  filePaths <- .get_rsds_tilepaths(RSDS)

  if(!is.null(tile_names)){

    notExist <- !tile_names %in% names(filePaths)
    if(any(notExist)) stop("Following tile names do not exist for input '", RSDS@name ,"':\n  ", paste(tile_names[notExist], collapse = "\n  "))

    filePaths <- filePaths[tile_names]
  }


  if(!all(file.exists(filePaths))) stop("Input RSDS '", RSDS@name, "' is in_complete", call. = FALSE)
}


.check_extension <- function(RSDS, extension){


    if(!any(RSDS@ext %in% extension)){

    stop("Input RSDS '", RSDS@name, "' should have a '", paste(extension, collapse = "', '"), "' extension", call. = FALSE)

    }
}

.get_tilescheme <- function(ts = getOption("misterRS.tilesheme")){

  if(is.null(ts)) stop("Could not find default tile scheme")

  return(ts)
}


.get_rsds_tilepaths <- function(rsds){

  # Get tile scheme
  ts <- .get_tilescheme()

  # Get file paths
  tilePaths <- file.path(rsds@dir, "tiles", paste0(ts$tile_name, ".", rsds@ext))

  # Get absolute path
  tilePaths <- suppressMessages(R.utils::getAbsolutePath(tilePaths))

  # Set names
  tilePaths <- setNames(tilePaths, ts$tile_name)

  return(tilePaths)

}

.get_rsds_mosaicpath <- function(rsds){

  # Get file path
  mosaicPath <- file.path(rsds@dir, paste0(rsds@id, ".", rsds@ext))

  # Get absolute path
  mosaicPath <- suppressMessages(R.utils::getAbsolutePath(mosaicPath))

  return(mosaicPath)
}


.headline <- function(headline){

  cat(
    headline, "\n",
    "Started at : ", format(Sys.time(), "%Y-%m-%d %X"), "\n",
    "\n",sep = "")

  Sys.time()
}


.conclusion <- function(time){

  total_time <- as.numeric(difftime(Sys.time(), time, units="secs"))

  cat(
    "\n",
    "Finished at : ", format(Sys.time(), "%Y-%m-%d %X"), "\n",
    "Total time  : ", sprintf("%02d:%02d:%02d", total_time %% 86400 %/% 3600, total_time %% 3600 %/% 60, total_time %% 60 %/% 1),
    "\n\n", sep = "")
}


#' Run a worker in parallel or serial
#'
#' @importFrom foreach %do%
#' @importFrom foreach %dopar%

.exe_tile_tile_worker <-function(tile_names, worker, ...){

  clusters = getOption("misterRS.clusters")

  if(length(tile_names) == 0){

    return(NULL)

  }else{

    if(clusters > 1) cat("  Clusters         : ", clusters, "\n", sep = "")

    # Wrap worker function in 'tryCatch'
    workerE <- function(tile_name){ rr <- tryCatch({ worker(tile_name) }, error = function(e){e})}

    # Make progress bar
    pb <- .progbar(length(tile_names))

    # Generate 'foreach' statement
    fe <- foreach::foreach(
      tile_name = tile_names,
      .packages = c("raster"),
      .errorhandling = 'pass',
      .options.snow = list(
        progress = function(n) pb$tick()
      )
    )

    # Execute in parallel
    if(clusters > 1){

      cl <- parallel::makeCluster(clusters)
      doSNOW::registerDoSNOW(cl)
      on.exit(parallel::stopCluster(cl))

      # Force the evaluation of all arguments given in the parent frame
      # NOTE: Because R uses 'lazyload', some arguments (or "promises") given in the original function
      # are not evaluated immediately. For whatever reason, %dopar% doesn't like this, and is unable
      # to evaluate these promises. The line below "forces" their evaluation.
      for(v in names(formals(sys.function(-1)))) eval(parse(text = v),  sys.frame(-1))

      rr <- fe %dopar% workerE(tile_name)

      # Execute in serial
    }else{

      rr <- fe %do% {

        result <- workerE(tile_name)
        pb$tick()
        return(result)
      }

    }

    return(setNames(rr, tile_names))

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


.tile_queue <- function(tile_paths, overwrite, tile_names = NULL, verbose = getOption("misterRS.verbosity")){

  selected_tiles <- if(is.null(tile_names)){

    names(tile_paths)

  }else{

    notExist <- !tile_names %in% names(tile_paths)
    if(any(notExist)) stop("Following tile names do not exist:\n  ", paste(tile_names[notExist], collapse = "\n  "))

    tile_names
  }

  proc_tiles <- if(overwrite){

    selected_tiles

  }else{

    selected_tiles[!file.exists(tile_paths[selected_tiles])]
  }

  if(verbose){
    cat(
      "  Overwrite      : ", overwrite, "\n",
      "  Total tiles    : ", length(tile_paths), "\n",
      "  Selected tiles : ", length(selected_tiles),  "\n",
      "  Queued tiles   : ", length(proc_tiles),      "\n",
      sep = ""
    )
  }


  return(proc_tiles)
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


.print_process_process_status <- function(status){

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

.tile_neibs <- function(ts, tile_name, case = "queen"){

  tile <- ts@data[ts@data$tile_name == tile_name,]

  tile_names <- apply(
    expand.grid(
    R = tile$row + c(-1,0,1),
    C = tile$col + c(-1,0,1)
  ), 1, function(x) paste0("R", x[1], "C", x[2]))

  if(case == "rook") tile_names <- tile_names[c(2, 4,5,6, 8)]

  return(ts[ts@data$tile_name[ts@data$tile_name %in% tile_names]])

}


.read_las_tile <- function(in_cat, tile, select, classes = NULL){

  # Tile buffer
  buff_sf <- sf::st_as_sf(tile[["buffs"]])

  # LAS catalog geometry
  las_grid <- in_cat$geometry

  if(is.na(sf::st_crs(in_cat))) stop("Can't select LAS tiles since this LAS Catalog has no projection info")

  # Reproject grid to tile
  las_grid <- sf::st_transform(las_grid, sf::st_crs(buff_sf))

  # Get intersection between RSDS tile and LAS catalog
  las_intrsc <- lengths(sf::st_intersects(las_grid, buff_sf)) > 0

  if(all(!las_intrsc)) return(NULL)

  # Get LAS files
  las_files <- in_cat@data$filename[las_intrsc]

  if(any(!file.exists(las_files))) stop("Missing LAS files")

  # Create extent filter from buffer extent
  buff_xt   <- raster::extent(buff_sf)
  buff_filt <- paste("-keep_xy", buff_xt@xmin, buff_xt@ymin, buff_xt@xmax, buff_xt@ymax)

  # Create class filter
  class_filt <- if(!is.null(classes)) paste(c("-keep_class", classes), collapse = " ")

  # Read LAS files
  inLAS <- lidR::readLAS(las_files, select = select, filter = c(buff_filt, class_filt))

  if(lidR::is.empty(inLAS)) return(NULL) else return(inLAS)

}

.normalize_las <- function(inLAS, DEMpath, z_min, z_max){

  if(!file.exists(DEMpath)) stop("Could not find DEM file '", DEMpath, "'")

  # Read segments and DEM
  DEM <- terra::rast(DEMpath)

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

