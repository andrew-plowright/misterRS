.check_complete_input <- function(RSDS){

  filePaths <- .rsds_tile_paths(RSDS)

  tile_names <- getOption("misterRS.tile_names")

  if(!is.null(tile_names)){

    notExist <- !tile_names %in% names(filePaths)
    if(any(notExist)) stop("Following tile names do not exist for input '", RSDS@name ,"':\n  ", paste(tile_names[notExist], collapse = "\n  "))

    filePaths <- filePaths[tile_names]
  }


  if(!all(file.exists(filePaths))) stop("Input RSDS '", RSDS@name, "' is in complete", call. = FALSE)
}


.check_extension <- function(RSDS, extension){

    if(!any(RSDS@ext %in% extension)){

    stop("Input RSDS '", RSDS@name, "' should have a '", paste(extension, collapse = "', '"), "' extension", call. = FALSE)

    }
}

.get_tilescheme <- function(ts = getOption("misterRS.ts")){

  if(is.null(ts)) stop("No tile scheme has been set. Use 'options('misterRS.ts')' to set one")

  return(ts)
}


.rsds_tile_paths <- function(rsds){

  # Get tile scheme
  ts <- .get_tilescheme()

  # Get file paths
  tilePaths <- file.path(rsds@dir, "tiles", paste0(ts$tileName, ".", rsds@ext))

  # Get absolute path
  tilePaths <- suppressMessages(R.utils::getAbsolutePath(tilePaths))

  # Set names
  tilePaths <- setNames(tilePaths, ts$tileName)

  return(tilePaths)

}

.rsds_mosaic_path <- function(rsds){

  ext <- if( rsds@ext == 'shp') 'gpkg' else rsds@ext

  # Get file path
  mosaic_path <- file.path(rsds@dir, paste0(rsds@id, ".", ext))

  # Get absolute path
  mosaic_path <- suppressMessages(R.utils::getAbsolutePath(mosaic_path))

  return(mosaic_path)
}



.rsds_metadata_path <- function(rsds){

  # Get file path
  metadata_path <- file.path(rsds@dir, paste0(rsds@id, "_metadata.json"))

  # Get absolute path
  metadata_path <- suppressMessages(R.utils::getAbsolutePath(metadata_path))

  return(metadata_path)
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
#' @importFrom foreach %do%
#' @importFrom foreach %dopar%

.exe_tile_worker <-function(tile_names, worker){

  clusters = getOption("misterRS.clusters")

  assign("overwrite", getOption("misterRS.overwrite"), env = parent.frame())

  if(length(tile_names) == 0){

    return(NULL)

  }else{

    # Wrap worker function in 'tryCatch'
    workerE <- function(tile_name){ rr <- tryCatch({ worker(tile_name) }, error = function(e){e})}

    # Make progress bar
    pb <- .progbar(length(tile_names))

    # Generate 'foreach' statement
    fe <- foreach::foreach(
      tile_name = tile_names,
      #.packages = c("raster"),
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
      # to evaluate these promises. The two lines below "forces" their evaluation.
      func_args <- setdiff(names(formals(sys.function(-1))), "...")
      for(func_args in func_args) eval(parse(text = func_args),  sys.frame(-1))

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


.tile_queue <- function(tile_paths,
                        overwrite = getOption("misterRS.overwrite"),
                        tile_names = getOption("misterRS.tile_names"),
                        clusters = getOption("misterRS.clusters"),
                        verbose = getOption("misterRS.verbose")){

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
      "  Overwrite        : ", overwrite, "\n",
      "  Clusters         : ", clusters, "\n",
      "  Total tiles      : ", length(tile_paths), "\n",
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

.tile_neibs <- function(ts, tile_name, case = "queen"){

  tile <- ts@data[ts@data$tileName == tile_name,]

  tile_names <- apply(
    expand.grid(
    R = tile$row + c(-1,0,1),
    C = tile$col + c(-1,0,1)
  ), 1, function(x) paste0("R", x[1], "C", x[2]))

  if(case == "rook") tile_names <- tile_names[c(2, 4,5,6, 8)]

  return(ts[ts@data$tileName[ts@data$tileName %in% tile_names]])

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
  buff_xt   <- terra::ext(buff_sf)
  buff_filt <- paste("-keep_xy", buff_xt[1], buff_xt[3], buff_xt[2], buff_xt[4])

  # Create class filter
  class_filt <- if(!is.null(classes)) paste(c("-keep_class", classes), collapse = " ")

  # Read LAS files
  inLAS <- lidR::readLAS(las_files, select = select, filter = paste(buff_filt, class_filt))

  if(lidR::is.empty(inLAS)) return(NULL) else return(inLAS)

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

.is_las_ground_classified <- function(inLAS){

  return(!is.null(inLAS$Classification) && lidR:::fast_count_equal(inLAS$Classification, lidR::LASGROUND))
}

.is_las_full_classified <- function(inLAS){

  return(!is.null(inLAS$Classification) && lidR:::fast_count_equal(inLAS$Classification, lidR::LASBUILDING))
}

.is_las_intensity <- function(inLAS){

  return(!is.null(inLAS$Intensity) && lidR:::fast_countover(inLAS$Intensity, 0))
}

