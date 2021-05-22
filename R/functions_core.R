.check_complete_input <- function(RSDS, tileNames = NULL){

  filePaths <- .get_RSDS_tilepaths(RSDS)

  if(!is.null(tileNames)){

    notExist <- !tileNames %in% names(filePaths)
    if(any(notExist)) stop("Following tile names do not exist for input '", RSDS@name ,"':\n  ", paste(tileNames[notExist], collapse = "\n  "))

    filePaths <- filePaths[tileNames]
  }


  if(!all(file.exists(filePaths))) stop("Input RSDS '", RSDS@name, "' is incomplete", call. = FALSE)
}


.check_extension <- function(RSDS, extension){


    if(!any(RSDS@ext %in% extension)){

    stop("Input RSDS '", RSDS@name, "' should have a '", paste(extension, collapse = "', '"), "' extension", call. = FALSE)

    }
}


.check_same_ts <- function(...){

  ds <- list(...)

  if(any(sapply(ds, class) != "RSDS")) "Input should be a list of RSDS class objects"

  if(length(ts) > 2){

    d1 <- ds[[1]]

    areIdentical <- sapply(ds[2:length(ds)], function(di) identical(d1@tileScheme, di@tileScheme))

    if(any(!areIdentical)) stop("Following RS Datasets must have the same Tile Scheme:\n  ", paste(sapply(ds, slot, "name"), collapse = "\n  "))

  }
}

.get_tilescheme <- function(ts = getOption("misterRS.tiles")){

  if(is.null(ts)) stop("Could not find default tile scheme")

  return(ts)
}


.get_RSDS_tilepaths <- function(RSDS){

  # Get tile scheme
  ts <- .get_tilescheme()

  # Get file paths
  tilePaths <- file.path(RSDS@dir, "tiles", paste0(ts$tileName, ".", RSDS@ext))

  # Get absolute path
  tilePaths <- suppressMessages(R.utils::getAbsolutePath(tilePaths))

  # Set names
  tilePaths <- setNames(tilePaths, ts$tileName)

  return(tilePaths)

}

.get_RSDS_mosaicpath <- function(RSDS){

  # Get file path
  mosaicPath <- file.path(RSDS@dir, paste0(RSDS@id, ".", RSDS@ext))

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

.doitlive <- function(tileNames, worker, ...){

  clusters = getOption("misterRS.clusters")

  if(length(tileNames) == 0){

    return(NULL)

  }else{

    if(clusters > 1) cat("  Clusters         : ", clusters, "\n", sep = "")

    # Wrap worker function in 'tryCatch'
    workerE <- function(tileName){ rr <- tryCatch({ worker(tileName) }, error = function(e){e})}

    # Make progress bar
    pb <- .progbar(length(tileNames))

    # Generate 'foreach' statement
    fe <- foreach::foreach(
      tileName = tileNames,
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

      rr <- fe %dopar% workerE(tileName)

      # Execute in serial
    }else{

      rr <- fe %do% {

        result <- workerE(tileName)
        pb$tick()
        return(result)
      }

    }

    return(setNames(rr, tileNames))

  }
}


#' Attempt to save a SHP file that may have a lock
#'
#' Certain programs (such as QGIS) may cause a file to be corrupted when an attempt is made to
#' overwrite it while it's locked. If this happens, this function will save a backup file

.lockSave <- function(output, outPath){

  # Create back-up file path
  backupPath <- gsub(".shp$","-backup.shp", outPath)

  tryArgs    <- list(output, outPath,    TRUE)
  backupArgs <- list(output, backupPath, TRUE)

  if("sf" %in% class(output)){

    func <- sf::st_write
    names(tryArgs) <- names(backupArgs) <- c("obj", "dsn", "delete_dsn")

    tryArgs    <- c(tryArgs,    list(quiet = TRUE))
    backupArgs <- c(backupArgs, list(quiet = TRUE))

  }else if(attributes(class(output))$package == "sp"){

    func <- APfun::APSHPsave
    names(tryArgs) <- names(backupArgs) <- c("object", "outfile", "overwrite")

  }else stop("Unrecognized file type")


  tryCatch({

      do.call(func, tryArgs)

    }, error = function(e){

      do.call(func, backupArgs)
      stop("Unable to overwrite file. File may be corrupt. Back-up saved.", call. = FALSE)
    })


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


.processing_tiles <- function(tile_paths, overwrite, tileNames = NULL){

  selected_tiles <- if(is.null(tileNames)){

    names(tile_paths)

  }else{

    notExist <- !tileNames %in% names(tile_paths)
    if(any(notExist)) stop("Following tile names do not exist:\n  ", paste(tileNames[notExist], collapse = "\n  "))

    tileNames
  }

  proc_tiles <- if(overwrite){

    selected_tiles

  }else{

    selected_tiles[!file.exists(tile_paths[selected_tiles])]
  }


  cat(
    "  Overwrite        : ", overwrite, "\n",
    "  Total tiles      : ", length(tile_paths), "\n",
    "  Selected tiles   : ", length(selected_tiles),  "\n",
    "  Processing tiles : ", length(proc_tiles),      "\n",
    sep = ""
  )

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


.statusReport <- function(status){

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

.tileNeighbours <- function(ts, tileName, case = "queen"){

  tile <- ts@data[ts@data$tileName == tileName,]

  tileNames <- apply(
    expand.grid(
    R = tile$row + c(-1,0,1),
    C = tile$col + c(-1,0,1)
  ), 1, function(x) paste0("R", x[1], "C", x[2]))

  if(case == "rook") tileNames <- tileNames[c(2, 4,5,6, 8)]

  return(ts[ts@data$tileName[ts@data$tileName %in% tileNames]])

}
