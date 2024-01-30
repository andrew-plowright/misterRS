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

    if(buffered) selected_tiles <- .tile_neibs(selected_tiles)
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


.tilescheme <- function(ts = getOption("misterRS.ts")){

  if(is.null(ts)) stop("No tile scheme has been set. Use 'options('misterRS.ts')' to set one")

  return(ts)
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



.vts_write <- function(in_sf, out_vts, tile_name, overwrite = FALSE){

  in_sf[["tile_name"]] <- tile_name


  if(!file.exists(out_vts@gpkg)){

    sf::st_write(in_sf, out_vts@gpkg, layer="layer", quiet=TRUE)

  }else{

    # Does tile already exist?
    has_tile <- .vts_has_tiles(out_vts, tile_name)

    if(has_tile){
      if(overwrite){

        con <- DBI::dbConnect(RSQLite::SQLite(), dbname = out_vts@gpkg)
        withr::defer(DBI::dbDisconnect(con))

        DBI::dbExecute(con, paste0("DELETE FROM layer WHERE tile_name = '", tile_name,"'"))

      }else{
        stop("Tile already exists")
      }
    }

    sf::st_write(in_sf, out_vts@gpkg, layer="layer", append = TRUE, quiet = TRUE)
  }

}

.vts_read <- function(vts, tile_name = NULL, geom = NULL){


  if(is.null(tile_name) & is.null(geom)) stop("Subset VTS either by tile_name or by geometry")

  # by tilename
  if(!is.null(tile_name)){

    sf::st_read(vts@gpkg, quiet = TRUE, query = sprintf("SELECT * FROM layer WHERE tile_name = '%s'", tile_name))

  }else if(!is.null(geom)){

    bbox_wkt <- sf::st_as_text(sf::st_geometry(geom))

    sf::st_read(vts@gpkg, quiet = TRUE, wkt_filter = bbox_wkt)
  }

}



.vts_has_tiles <- function(vts, tile_names){

  if(is.null(tile_names) | length(tile_names) == 0){

    return(character())

  }else{

    if(!file.exists(vts@gpkg) || !("layer" %in% sf::st_layers(vts@gpkg)$name)){

      return(setNames(rep(F, length(tile_names)), tile_names))

    }else{

      con <- DBI::dbConnect(RSQLite::SQLite(), dbname = vts@gpkg)
      withr::defer(DBI::dbDisconnect(con))

      has_tiles <- DBI::dbGetQuery(
        con,paste0(
          "WITH st(tile_name) AS (VALUES ('", paste(tile_names, collapse="'), ('"), "'))
          SELECT st.tile_name, count(layer.tile_name) >0 as count FROM st left join layer on st.tile_name = layer.tile_name
          GROUP BY st.tile_name")
      )
      return(setNames(has_tiles$count > 0, has_tiles$tile_name))
    }
  }
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


.tile_queue <- function(xts,
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

        has_tiles <- .vts_has_tiles(xts, proc_tiles)

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

.tile_neibs <- function(tile_names, case = "queen"){

  ts <- .tilescheme()

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


.read_las_tile <- function(in_cat, tile, select, classes = NULL){

  # Tile buffer
  buff_sf <- sf::st_as_sf(tile[["buffs"]])

  # LAS catalog geometry
  las_grid <- in_cat$geometry

  if(is.na(sf::st_crs(in_cat))) stop("Can't select LAS tiles since this LAS Catalog has no projection info")

  # Reproject grid to tile
  las_grid <- sf::st_transform(las_grid, sf::st_crs(buff_sf))

  # Get intersection between tile and LAS catalog
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

