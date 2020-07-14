
#' Extract Training data
#'
#' @export

ExtractTrainingData <- function(trainingPts, segPoly_RSDS, metrics, segID, overwrite = FALSE){

  tim <- .headline("EXTRACT TRAINING DATA")

  ### CHECK INPUTS ----

  if(file.exists(trainingPts@datafile) & !overwrite){

    cat("Training data table for '", trainingPts@name, "' already exists. Set 'overwrite' to TRUE", "\n")

  }else{

    # Check that all RSDS have same tileScheme
    do.call(.check_same_ts, c(list(segPoly_RSDS), metrics))

    # Check extensions
    .check_extension(segPoly_RSDS, c("shp", "gpkg"))
    for(RS in metrics) .check_extension(RS,  "csv")

    # Check that inputs are complete
    .check_complete_input(segPoly_RSDS)
    for(RS in metrics) .check_complete_input(RS)


    ### READ TRAINING POINTS ----

    # Read training points
    trainingSP <- APfun::APSHPread(trainingPts@SHPfile)
    trainingSP[["trainingID"]] <- 1:length(trainingSP)

    # Get tiles for training points
    raster::crs(trainingSP) <- raster::crs(segPoly_RSDS@tileScheme[["tiles"]])
    trainingSP[["tileName"]] <- over(trainingSP, segPoly_RSDS@tileScheme[["tiles"]])[["tileName"]]
    uniqueTiles <- unique(trainingSP[["tileName"]])

    # Get all tile headers
    headers <- lapply(metrics, function(met){
      lapply(uniqueTiles, function(tileName){
        names(read.csv(met@tilePaths[tileName], row.names = 1, nrows = 1, check.names = FALSE, stringsAsFactors = FALSE))
      })
    })

    # Check that all headers are consistent between metrics
    for(i in 1:length(metrics)){
      if(length(unique(headers[[i]])) != 1) stop("Inconsistent tile headers for metrics '", metrics[[1]]@name, "'")
    }

    # Check that no duplicate headers exist
    if(any(duplicated(unlist(lapply(headers, function(h) h[[1]]))))) stop("Duplicated variable names between metrics")

    cat(
      "  Training set : ", trainingPts@name, "\n",
      "  Training pts : ", length(trainingSP), "\n",
      "  Tiles        : ", length(uniqueTiles), "\n",
      sep = ""
    )


    ### EXTRACT DATA ----

    # Create progress bar
    pb <- .progbar(length(uniqueTiles))

    # Read training data
    trainingData <- lapply(uniqueTiles, function(tileName){

      # Read segments
      segPoly <- sf::st_read(segPoly_RSDS@tilePaths[tileName], quiet = TRUE)

      # Get all metrics
      segData <- unname(lapply(metrics, function(met) read.csv(met@tilePaths[tileName], row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)))

      # Check matching row names
      for(i in 1:length(metrics)){
        if(!all(segPoly[[segID]] == row.names(segData[[i]]))){
          stop("Row names for '", metrics[[i]]@name, "' do not match '", segID, "' field for segments in tile '", tileName, "'")
        }
      }

      # Assemble all metrics
      segPoly <- dplyr::bind_cols(segPoly[,segID], do.call(cbind, segData))

      # Get training points for tile
      trainingTile <- sf::st_as_sf(trainingSP[trainingSP$tileName == tileName,])

      # Get segment polygons intersection with points
      trainingPoly <- segPoly[as.numeric(sf::st_intersects(trainingTile, segPoly)),]
      sf::st_geometry(trainingPoly) <- NULL

      pb$tick()

      cbind(
        row.names = trainingTile$trainingID,
        trainingSetID = trainingPts@name,
        trainingPtID  = trainingTile$trainingID,
        tileName      = tileName,
        trainingPoly,
        segClass = trainingTile$segClass
      )
    })

    ### FORMAT AND SAVE OUTPUT ----

    # Combine
    trainingData <- do.call(rbind,trainingData)

    # Re-order
    trainingData <- trainingData[as.character(trainingSP$trainingID), ]

    # Write output
    write.csv(trainingData, trainingPts@datafile, row.names = FALSE, na = "")
  }

  # Conclude
  .conclusion(tim)
}

#' Create classifier
#'
#' @export

CreateClassifier <- function(trainingPts, classifierFile, segID, predictors = NULL, overwrite = FALSE){

  if(file.exists(classifierFile) & !overwrite) stop("Classifier already exists. Set 'overwrite' to TRUE")

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

  cat("Following predictor variables selected:\n ", paste(predictors, collapse = "\n  "), "\n")

  # Factorize 'segClass' attribute
  alldata$segClass <- as.factor(alldata$segClass)

  # Check for rows with NA values and remove
  badRows <- apply(is.na(alldata), 1, any)
  if(any(badRows)){

    cat("Remove points:", length(badRows[badRows]), "\n")
    alldata <- alldata[!badRows,]
  }

  # Create classifier
  classifier <- randomForest::randomForest(
    as.formula(paste("segClass ~", paste(predictors, collapse = " + "))),
    data       = alldata,
    importance = TRUE,
    ntree      = 1000)

  cat("OOB error rate:", round(classifier$err.rate[classifier$ntree, "OOB"]*100, digits=2), "%", "\n\n")

  # create classifier output folder
  classifierDir <- dirname(classifierFile)
  if(!dir.exists(classifierDir)) dir.create(classifierDir, recursive = TRUE)

  # Save classifier
  saveRDS(classifier, classifierFile)

}

#' Classify segments
#'
#' @export

ClassifySegments <- function(classifierFile, segPoly_RSDS, segClassPoly_RSDS, classEdits, metrics, segID,
                             tileNames = NULL, clusters = 1, overwrite = FALSE){


  tim <- .headline("CLASSIFY POLYGON SEGMENTS")

  ### INPUT CHECKS ----

    # Check that all RSDS have same tileScheme
    do.call(.check_same_ts, c(list(segPoly_RSDS, segClassPoly_RSDS), metrics))

    # Check extensions
    .check_extension(segPoly_RSDS,          c("shp", "gpkg"))
    .check_extension(segClassPoly_RSDS,     c("shp"))
    for(RS in metrics) .check_extension(RS, c("csv"))

    # Check that inputs are complete
    .check_complete_input(segPoly_RSDS, tileNames)
    for(RS in metrics) .check_complete_input(RS)


  ### READ CLASSIFIER ----

    classifier <- readRDS(classifierFile)

  ### READ CLASS EDITS ----

    cEdits <- suppressWarnings(rgdal::ogrInfo(classEdits@SHPfile)$have_features)

    if(cEdits){

      classEdits <- APfun::APSHPread(classEdits@SHPfile)
      raster::crs(classEdits) <- raster::crs(segPoly_RSDS@tileScheme[["tiles"]])

      cTiles <- setNames(lapply( sp::over(
        segPoly_RSDS@tileScheme[["tiles"]],
        classEdits,
        returnList = TRUE
      ), row.names), segPoly_RSDS@tileScheme[["tiles"]]$tileName)
    }





  ### CREATE WORKER ----

    # Run process
    worker <- function(tileName){

      # Get segments
      segPoly <- sf::st_read(segPoly_RSDS@tilePaths[tileName], quiet = TRUE)

      # Out file
      outfile <- segClassPoly_RSDS@tilePaths[tileName]

      # Get all metrics
      segData <- unname(lapply(metrics, function(met) read.csv(met@tilePaths[tileName], row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)))

      # Check matching row names
      for(i in 1:length(metrics)){
        if(!all(segPoly[[segID]] == row.names(segData[[i]]))){
          stop("Row names for '", metrics[[i]]@name, "' do not match '", segID, "' field for segments in tile '", tileName, "'")
        }
      }

      # Combine metrics
      segData <- do.call(cbind, segData)

      # Classify according to most-voted class
      votes   <- randomForest:::predict.randomForest(classifier, segData, type = "vote")
      elected <- colnames(votes)[apply(votes, 1, function(x) which.max(x)[1])]
      segPoly[["segClass"]] <- elected

      # Manual edits
      if(cEdits && length(cTiles[[tileName]]) > 0){

        edits <- sf::st_as_sf(classEdits[cTiles[[tileName]],])

        editIntersc <- sf::st_intersects(edits, segPoly)

        for(i in 1:nrow(edits)){

          edit <- edits[i,]

          # Get to/from classes
          from <- strsplit(edit$fromClass, " ")[[1]]
          to   <- edit$toClass

          # Get segments that intersect with edit polygon
          editSegs <- segPoly[editIntersc[[i]],]

          # Subset according to specified 'fromClass' value (if specified)
          if(!is.na(from)) editSegs <- editSegs[editSegs$segClass %in% from,]

          # Apply edit
          if(nrow(editSegs) > 0){

            segPoly[segPoly[[segID]] %in% editSegs[[segID]],][["segClass"]] <- to
          }
        }
      }

      # Save output
      sf::st_write(segPoly, outfile, delete_dsn = file.exists(outfile), quiet = TRUE)

      if(file.exists(outfile)) "Success" else stop("Failed to create output")

    }

  ### APPLY WORKER ----

    # Get tiles for processing
    procTiles <- .processing_tiles(segClassPoly_RSDS, overwrite, tileNames)

    # Process
    status <- .doitlive(procTiles, clusters, worker)

    # Report
    .statusReport(status)

    # Conclude
    .conclusion(tim)

}



#' Classify raster segments
#'
#' @export

ClassifyRaster <- function(segClassPoly_RSDS, segRas_RSDS, segClassRas_RSDS, segClasses, segID,
                           tileNames = NULL, clusters = 1, overwrite = FALSE){


  tim <- .headline("CLASSIFY RASTER SEGMENTS")

  ### INPUT CHECKS ----

  # Check that all RSDS have same tileScheme
 .check_same_ts(segClassPoly_RSDS, segRas_RSDS, segClassRas_RSDS)

  # Check extensions
  .check_extension(segClassPoly_RSDS, "shp")
  .check_extension(segClassRas_RSDS,  "tif")
  .check_extension(segRas_RSDS,       "tif")

  # Check that inputs are complete
  .check_complete_input(segClassPoly_RSDS, tileNames)
  .check_complete_input(segRas_RSDS,       tileNames)

  ### CREATE WORKER ----

  # Run process
  worker <- function(tileName){

    # Get classified polygonal segments
    segPoly <- sf::st_read(segClassPoly_RSDS@tilePaths[tileName], quiet = TRUE)

    # Get unclassified raster segments
    segRas <- raster::raster(segRas_RSDS@tilePaths[tileName])

    # Out file
    outfile <- segClassRas_RSDS@tilePaths[tileName]

    # Convert 'segRas' segment numbers to class numbers
    segClassRas <- raster::setValues(segRas, factor(
      segPoly[["segClass"]][match(segRas[], segPoly[[segID]])],
      levels = segClasses))

    # Save output
    raster::writeRaster(segClassRas, outfile, overwrite = overwrite, datatype = "INT1U")

    if(file.exists(outfile)) "Success" else stop("Failed to create output")

  }

  ### APPLY WORKER ----

  # Get tiles for processing
  procTiles <- .processing_tiles(segClassRas_RSDS, overwrite, tileNames)

  # Process
  status <- .doitlive(procTiles, clusters, worker)

  # Report
  .statusReport(status)

  # Conclude
  .conclusion(tim)

}
