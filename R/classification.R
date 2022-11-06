#' Extract Training data
#'
#' @export

training_data_extract <- function(training_pts, seg_poly_rsds, metrics, seg_id, overwrite = FALSE){

  process_timer <- .headline("EXTRACT TRAINING DATA")

  ### CHECK INPUTS ----

  if(file.exists(training_pts@datafile) & !overwrite){

    cat("Training data table for '", training_pts@id, "' already exists. Set 'overwrite' to TRUE", "\n")

  }else{

    # Get tile scheme
    ts <- .get_tilescheme()
    tiles_sf <- sf::st_as_sf(ts[["tiles"]])

    # Get CRS
    crs <- getOption("misterRS.crs")

    # Get metric paths
    met_paths <- lapply(metrics, .get_rsds_tilepaths)
    seg_poly_paths <- .get_rsds_tilepaths(seg_poly_rsds)

    # Check extensions
    .check_extension(seg_poly_rsds, c("shp", "gpkg"))
    for(RS in metrics) .check_extension(RS,  "csv")

    # Check that inputs are complete
    .check_complete_input(seg_poly_rsds)
    for(RS in metrics) .check_complete_input(RS)


    ### READ TRAINING POINTS ----

    # Read training points
    training_sf <- sf::st_read(training_pts@SHPfile, quiet = TRUE)
    training_sf[["trainingID"]] <- 1:nrow(training_sf)

    # Assign training points to tiles
    training_sf[["tile_name"]] <- tiles_sf$tile_name[ sapply( sf::st_intersects(training_sf, tiles_sf), `[`, 1) ]
    unique_tiles <- unique(training_sf[["tile_name"]])

    if(any(is.na(unique_tiles))) stop("Training points outside of tileset")

    # Get all tile headers
    headers <- lapply(names(metrics), function(met_name){
      lapply(unique_tiles, function(tile_name){
        names(read.csv(met_paths[[met_name]][tile_name], row.names = 1, nrows = 1, check.names = FALSE, stringsAsFactors = FALSE))
      })
    })

    # Check that all headers are consistent between metrics
    for(i in 1:length(metrics)){
      if(length(unique(headers[[i]])) != 1) stop("Inconsistent tile headers for metrics '", metrics[[i]]@name, "'")
    }

    # Check that no duplicate headers exist
    if(any(duplicated(unlist(lapply(headers, function(h) h[[1]]))))) stop("Duplicated variable names between metrics")

    cat(

      "  Training pts : ", nrow(training_sf), "\n",
      "  Tiles        : ", length(unique_tiles), "\n",
      sep = ""
    )


    ### EXTRACT DATA ----

    # Create progress bar
    pb <- .progbar(length(unique_tiles))

    # Read training data
    trainingData <- lapply(unique_tiles, function(tile_name){

      seg_poly_path <- seg_poly_paths[tile_name]

      # Read segments
      seg_poly <- sf::st_read(seg_poly_path, quiet = TRUE)

      # Get all metrics
      seg_data <- unname(lapply(names(metrics), function(met_name) read.csv(met_paths[[met_name]][tile_name], row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)))

      # Check matching row names
      for(i in 1:length(metrics)){
        if(!all(seg_poly[[seg_id]] == row.names(seg_data[[i]]))){
          stop("Row names for '", metrics[[i]]@name, "' do not match '", seg_id, "' field for segments in tile '", tile_name, "'")
        }
      }

      # Assemble all metrics
      seg_poly <- dplyr::bind_cols(seg_poly[,seg_id], do.call(cbind, seg_data))

      # Get training points for tile
      training_tile <- training_sf[training_sf$tile_name == tile_name,]

      # Get segment polygons intersection with points
      trainingPoly <- seg_poly[as.numeric(sf::st_intersects(training_tile, seg_poly)),]
      sf::st_geometry(trainingPoly) <- NULL

      pb$tick()

      cbind(
        row.names = training_tile$trainingID,
        trainingSetID = training_pts@id,
        trainingPtID  = training_tile$trainingID,
        tile_name      = tile_name,
        trainingPoly,
        segClass = training_tile$segClass
      )
    })

    ### FORMAT AND SAVE OUTPUT ----

    # Combine
    trainingData <- do.call(rbind,trainingData)

    # Re-order
    trainingData <- trainingData[as.character(training_sf$trainingID), ]

    # Write output
    write.csv(trainingData, training_pts@datafile, row.names = FALSE, na = "")
  }

  # Conclude
  .conclusion(process_timer)
}

#' Create classifier
#'
#' @export

classifier_create <- function(training_pts, classifier_file = NULL, seg_id, predictors = NULL,
                             overwrite = FALSE,  verbose = TRUE){

  if(!is.null(classifier_file)){
    if(file.exists(classifier_file) & !overwrite){
      stop("Classifier already exists. Set 'overwrite' to TRUE")
    }
  }

  # Combine all training data
  alldata <- do.call(rbind, lapply(training_pts, function(tp){
    if(!file.exists(tp@datafile)) stop("Data file does not exist for '", tp@name, "'")

    read.csv(tp@datafile)
  }))

  # Drop columns
  dropCols <- c("trainingSetID", "trainingPtID", "tile_name" , seg_id)
  alldata <- alldata[,!names(alldata) %in% dropCols]

  # 'predictors' not defined
  if(is.null(predictors)){

    # Use autoSelect to choose uncorrelated variables
    predictors <- as.character(Biocomb::select.cfs(alldata)$Biomarker)

  }else{

    # Check that selected predictors exist
    notFound <- !predictors %in% names(alldata)

    if(any(notFound)) stop("Following predictor variables not found in survey's Training Data:\n  ",
                           paste(predictors[notFound], collapse = "\n  "))
  }

  if(verbose) cat("Following predictor variables selected:\n ", paste(predictors, collapse = "\n  "), "\n")

  # Factorize 'segClass' attribute
  alldata$segClass <- as.factor(alldata$segClass)

  # Check for rows with NA values and remove
  badRows <- apply(is.na(alldata), 1, any)
  if(any(badRows)){

    if(verbose) cat("Remove points:", length(badRows[badRows]), "\n")
    alldata <- alldata[!badRows,]
  }

  # Create classifier
  classifier <- randomForest::randomForest(
    as.formula(paste("segClass ~", paste(predictors, collapse = " + "))),
    data       = alldata,
    importance = TRUE,
    ntree      = 1000)

  if(verbose) cat("OOB error rate:", round(classifier$err.rate[classifier$ntree, "OOB"]*100, digits=2), "%", "\n\n")

  if(is.null(classifier_file)){

    return(classifier)

  }else{

    # create classifier output folder
    classifierDir <- dirname(classifier_file)
    if(!dir.exists(classifierDir)) dir.create(classifierDir, recursive = TRUE)

    # Save classifier
    saveRDS(classifier, classifier_file)
  }
}

#' Classify polygonal segments
#'
#' @export

classify_seg_poly <- function(classifier_file, seg_poly_rsds, seg_class_poly_rsds, class_edits, metrics, seg_id,
                             tile_names = NULL, overwrite = FALSE){


  process_timer <- .headline("CLASSIFY POLYGON SEGMENTS")

  ### INPUT CHECKS ----

    # Check extensions
    .check_extension(seg_poly_rsds,          c("shp", "gpkg"))
    .check_extension(seg_class_poly_rsds,     c("shp"))
    for(RS in metrics) .check_extension(RS, c("csv"))

    # Check that inputs are complete
    .check_complete_input(seg_poly_rsds, tile_names)
    for(RS in metrics) .check_complete_input(RS, tile_names)

    # Get CRS
    proj <- getOption("misterRS.crs")

    # Get tile scheme
    ts <- .get_tilescheme()
    tiles_sf <- sf::st_as_sf(ts[["tiles"]])

    # Get file paths
    seg_poly_paths <- .get_rsds_tilepaths(seg_poly_rsds)
    out_paths     <- .get_rsds_tilepaths(seg_class_poly_rsds)
    met_paths     <- lapply(metrics, .get_rsds_tilepaths)

    # Read classifier
    classifier <- readRDS(classifier_file)

  ### READ CLASS EDITS ----

    class_edits <- sf::st_read(class_edits@SHPfile, quiet = TRUE)

    if(nrow(class_edits) > 0){

      class_edits_bytile <- setNames(sf::st_intersects(tiles_sf, class_edits), ts[["tiles"]][["tile_name"]])

    }


  ### CREATE WORKER ----

    # Run process
    tile_worker <-function(tile_name){

      seg_poly_path <- seg_poly_paths[tile_name]
      out_path     <- out_paths[tile_name]

      # Read segments
      seg_poly <- sf::st_read(seg_poly_path, quiet = TRUE)

      # Get all metrics
      seg_data <- unname(lapply(names(metrics), function(met_name){
        read.csv(met_paths[[met_name]][tile_name],
                 row.names = 1, check.names = FALSE,
                 stringsAsFactors = FALSE)
      }))

      # Check matching row names
      for(i in 1:length(metrics)){
        if(!all(seg_poly[[seg_id]] == row.names(seg_data[[i]]))){
          stop("Row names for '", metrics[[i]]@name, "' do not match '", seg_id, "' field for segments in tile '", tile_name, "'")
        }
      }

      # Combine metrics
      seg_data <- do.call(cbind, seg_data)

      # Classify according to most-voted class
      votes   <- randomForest:::predict.randomForest(classifier, seg_data, type = "vote")
      elected <- colnames(votes)[apply(votes, 1, function(x) which.max(x)[1])]
      seg_poly[["segClass"]] <- elected
      if(length(elected) > 0){
        seg_poly[["votePrc"]]  <- sapply(1:length(elected), function(i){
          el <- elected[i]
          if(is.na(el)) NA else votes[i, el]
        })
      }

      # Manual edits
      if((nrow(class_edits) > 0) && (length(class_edits_bytile[[tile_name]]) > 0)){

        # Class edits for this tile
        class_edits_tile <- class_edits[class_edits_bytile[[tile_name]],]

        # Intersection between class edits and polygons
        class_edits_bypoly <- sf::st_intersects(class_edits_tile, seg_poly)

        for(i in 1:nrow(class_edits_tile)){

          edit <- class_edits_tile[i,]

          # Get to/from classes
          from <- strsplit(edit$fromClass, " ")[[1]]
          to   <- edit$toClass

          # Get segments that intersect with edit polygon
          edit_segs <- seg_poly[class_edits_bypoly[[i]],]

          # Subset according to specified 'fromClass' value (if specified)
          if(!is.na(from)) edit_segs <- edit_segs[edit_segs$segClass %in% from,]

          # Apply edit
          if(nrow(edit_segs) > 0){

            edit_which <- seg_poly[[seg_id]] %in% edit_segs[[seg_id]]
            seg_poly[edit_which,][["segClass"]] <- to
            seg_poly[edit_which,][["votePrc" ]] <- NA
          }
        }
      }

      # Save output
      sf::st_write(seg_poly, out_path, delete_dsn = file.exists(out_path), quiet = TRUE)

      if(file.exists(out_path)) "Success" else stop("Failed to create output")

    }

  ### APPLY WORKER ----

    # Get tiles for processing
    proc_tiles <- .tile_queue(out_paths, overwrite, tile_names)

    # Process
    process_status <- .exe_tile_worker(proc_tiles, worker)

    # Report
    .print_process_status(process_status)

    # Conclude
    .conclusion(process_timer)

}



#' Classify raster segments
#'
#' This needs \code{classify_seg_poly} to be run first
#'
#' @export

classify_seg_ras <- function(seg_class_poly_rsds, seg_ras_rsds, seg_class_ras_rsds, segClasses, seg_id,
                           tile_names = NULL, overwrite = FALSE){


  process_timer <- .headline("CLASSIFY RASTER SEGMENTS")

  ### INPUT CHECKS ----

  # Check extensions
  .check_extension(seg_class_poly_rsds, "shp")
  .check_extension(seg_class_ras_rsds,  "tif")
  .check_extension(seg_ras_rsds,       "tif")

  # Check that inputs are complete
  .check_complete_input(seg_class_poly_rsds, tile_names)
  .check_complete_input(seg_ras_rsds,       tile_names)

  seg_class_poly_paths <- .get_rsds_tilepaths(seg_class_poly_rsds)
  seg_ras_paths       <- .get_rsds_tilepaths(seg_ras_rsds)
  out_paths          <- .get_rsds_tilepaths(seg_class_ras_rsds)

  ### CREATE WORKER ----

  # Run process
  tile_worker <-function(tile_name){

    # Get file paths
    seg_class_poly_path <- seg_class_poly_paths[tile_name]
    seg_ras_path       <- seg_ras_paths[tile_name]
    out_path          <- out_paths[tile_name]

    # Get classified polygonal segments
    seg_poly <- sf::st_read(seg_class_poly_path, quiet = TRUE)

    # Get unclassified raster segments
    seg_ras <- raster::raster(seg_ras_path)

    # Convert 'seg_ras' segment numbers to class numbers
    seg_class_ras <- raster::setValues(seg_ras, factor(
      seg_poly[["segClass"]][match(seg_ras[], seg_poly[[seg_id]])],
      levels = segClasses))

    # Save output
    raster::writeRaster(seg_class_ras, out_path, overwrite = overwrite, datatype = "INT1U")

    if(file.exists(out_path)) "Success" else stop("Failed to create output")

  }

  ### APPLY WORKER ----

  # Get tiles for processing
  proc_tiles <- .tile_queue(out_paths, overwrite, tile_names)

  # Process
  process_status <- .exe_tile_worker(proc_tiles, worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)

}
