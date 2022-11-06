
#' Extract Training data
#'
#' @export

ExtractTrainingData <- function(trainingPts, segPoly_RSDS, metrics, segID, overwrite = FALSE){

  tim <- .headline("EXTRACT TRAINING DATA")

  ### CHECK INPUTS ----

  if(file.exists(trainingPts@datafile) & !overwrite){

    cat("Training data table for '", trainingPts@id, "' already exists. Set 'overwrite' to TRUE", "\n")

  }else{

    # Get tile scheme
    ts <- .get_tilescheme()
    tiles_sf <- sf::st_as_sf(ts[["tiles"]])

    # Get CRS
    crs <- getOption("misterRS.crs")

    # Get metric paths
    met_paths <- lapply(metrics, .get_RSDS_tilepaths)
    seg_poly_paths <- .get_RSDS_tilepaths(segPoly_RSDS)

    # Check extensions
    .check_extension(segPoly_RSDS, c("shp", "gpkg"))
    for(RS in metrics) .check_extension(RS,  "csv")

    # Check that inputs are complete
    .check_complete_input(segPoly_RSDS)
    for(RS in metrics) .check_complete_input(RS)


    ### READ TRAINING POINTS ----

    # Read training points
    training_sf <- sf::st_read(trainingPts@SHPfile, quiet = TRUE)
    training_sf[["trainingID"]] <- 1:nrow(training_sf)

    # Assign training points to tiles
    training_sf[["tileName"]] <- tiles_sf$tileName[ sapply( sf::st_intersects(training_sf, tiles_sf), `[`, 1) ]
    unique_tiles <- unique(training_sf[["tileName"]])

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
        if(!all(seg_poly[[segID]] == row.names(seg_data[[i]]))){
          stop("Row names for '", metrics[[i]]@name, "' do not match '", segID, "' field for segments in tile '", tile_name, "'")
        }
      }

      # Assemble all metrics
      seg_poly <- dplyr::bind_cols(seg_poly[,segID], do.call(cbind, seg_data))

      # Get training points for tile
      training_tile <- training_sf[training_sf$tileName == tile_name,]

      # Get segment polygons intersection with points
      trainingPoly <- seg_poly[as.numeric(sf::st_intersects(training_tile, seg_poly)),]
      sf::st_geometry(trainingPoly) <- NULL

      pb$tick()

      cbind(
        row.names = training_tile$trainingID,
        trainingSetID = trainingPts@id,
        trainingPtID  = training_tile$trainingID,
        tileName      = tile_name,
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
    write.csv(trainingData, trainingPts@datafile, row.names = FALSE, na = "")
  }

  # Conclude
  .conclusion(tim)
}

#' Create classifier
#'
#' @export

CreateClassifier <- function(trainingPts, classifierFile = NULL, segID, predictors = NULL,
                             overwrite = FALSE,  verbose = TRUE){

  if(!is.null(classifierFile)){
    if(file.exists(classifierFile) & !overwrite){
      stop("Classifier already exists. Set 'overwrite' to TRUE")
    }
  }

  # Combine all training data
  alldata <- do.call(rbind, lapply(trainingPts, function(tp){
    if(!file.exists(tp@datafile)) stop("Data file does not exist for '", tp@name, "'")

    read.csv(tp@datafile)
  }))

  # Drop columns
  dropCols <- c("trainingSetID", "trainingPtID", "tileName" , segID)
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

  if(is.null(classifierFile)){

    return(classifier)

  }else{

    # create classifier output folder
    classifierDir <- dirname(classifierFile)
    if(!dir.exists(classifierDir)) dir.create(classifierDir, recursive = TRUE)

    # Save classifier
    saveRDS(classifier, classifierFile)
  }
}

#' Classify segments
#'
#' @export

ClassifySegments <- function(classifierFile, segPoly_RSDS, segClassPoly_RSDS, classEdits, metrics, segID,
                             tileNames = NULL, overwrite = FALSE){


  tim <- .headline("CLASSIFY POLYGON SEGMENTS")

  ### INPUT CHECKS ----

    # Check extensions
    .check_extension(segPoly_RSDS,          c("shp", "gpkg"))
    .check_extension(segClassPoly_RSDS,     c("shp"))
    for(RS in metrics) .check_extension(RS, c("csv"))

    # Check that inputs are complete
    .check_complete_input(segPoly_RSDS, tileNames)
    for(RS in metrics) .check_complete_input(RS, tileNames)

    # Get CRS
    proj <- getOption("misterRS.crs")

    # Get tile scheme
    ts <- .get_tilescheme()
    tiles_sf <- sf::st_as_sf(ts[["tiles"]])

    # Get file paths
    seg_poly_paths <- .get_RSDS_tilepaths(segPoly_RSDS)
    out_paths     <- .get_RSDS_tilepaths(segClassPoly_RSDS)
    met_paths     <- lapply(metrics, .get_RSDS_tilepaths)

    # Read classifier
    classifier <- readRDS(classifierFile)

  ### READ CLASS EDITS ----

    class_edits <- sf::st_read(classEdits@SHPfile, quiet = TRUE)

    if(nrow(class_edits) > 0){

      class_edits_bytile <- setNames(sf::st_intersects(tiles_sf, class_edits), ts[["tiles"]][["tileName"]])

    }


  ### CREATE WORKER ----

    # Run process
    worker <- function(tileName){

      seg_poly_path <- seg_poly_paths[tileName]
      out_path     <- out_paths[tileName]

      # Read segments
      seg_poly <- sf::st_read(seg_poly_path, quiet = TRUE)

      # Get all metrics
      seg_data <- unname(lapply(names(metrics), function(met_name){
        read.csv(met_paths[[met_name]][tileName],
                 row.names = 1, check.names = FALSE,
                 stringsAsFactors = FALSE)
      }))

      # Check matching row names
      for(i in 1:length(metrics)){
        if(!all(seg_poly[[segID]] == row.names(seg_data[[i]]))){
          stop("Row names for '", metrics[[i]]@name, "' do not match '", segID, "' field for segments in tile '", tileName, "'")
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
      if((nrow(class_edits) > 0) && (length(class_edits_bytile[[tileName]]) > 0)){

        # Class edits for this tile
        class_edits_tile <- class_edits[class_edits_bytile[[tileName]],]

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

            edit_which <- seg_poly[[segID]] %in% edit_segs[[segID]]
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
    proc_tiles <- .processing_tiles(out_paths, overwrite, tileNames)

    # Process
    status <- .doitlive(proc_tiles, worker)

    # Report
    .statusReport(status)

    # Conclude
    .conclusion(tim)

}



#' Classify raster segments
#'
#' @export

ClassifyRaster <- function(segClassPoly_RSDS, segRas_RSDS, segClassRas_RSDS, segClasses, segID,
                           tileNames = NULL, overwrite = FALSE){


  tim <- .headline("CLASSIFY RASTER SEGMENTS")

  ### INPUT CHECKS ----

  # Check extensions
  .check_extension(segClassPoly_RSDS, "shp")
  .check_extension(segClassRas_RSDS,  "tif")
  .check_extension(segRas_RSDS,       "tif")

  # Check that inputs are complete
  .check_complete_input(segClassPoly_RSDS, tileNames)
  .check_complete_input(segRas_RSDS,       tileNames)

  segClassPoly_paths <- .get_RSDS_tilepaths(segClassPoly_RSDS)
  segRas_paths       <- .get_RSDS_tilepaths(segRas_RSDS)
  out_paths          <- .get_RSDS_tilepaths(segClassRas_RSDS)

  ### CREATE WORKER ----

  # Run process
  worker <- function(tileName){

    # Get file paths
    segClassPoly_path <- segClassPoly_paths[tileName]
    segRas_path       <- segRas_paths[tileName]
    out_path          <- out_paths[tileName]

    # Get classified polygonal segments
    seg_poly <- sf::st_read(segClassPoly_path, quiet = TRUE)

    # Get unclassified raster segments
    segRas <- raster::raster(segRas_path)

    # Convert 'segRas' segment numbers to class numbers
    segClassRas <- raster::setValues(segRas, factor(
      seg_poly[["segClass"]][match(segRas[], seg_poly[[segID]])],
      levels = segClasses))

    # Save output
    raster::writeRaster(segClassRas, out_path, overwrite = overwrite, datatype = "INT1U")

    if(file.exists(out_path)) "Success" else stop("Failed to create output")

  }

  ### APPLY WORKER ----

  # Get tiles for processing
  proc_tiles <- .processing_tiles(out_paths, overwrite, tileNames)

  # Process
  status <- .doitlive(proc_tiles, worker)

  # Report
  .statusReport(status)

  # Conclude
  .conclusion(tim)

}
