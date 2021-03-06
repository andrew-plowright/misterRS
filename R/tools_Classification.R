
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

    # Get CRS
    crs <- getOption("misterRS.crs")

    # Get metric paths
    met_paths <- lapply(metrics, .get_RSDS_tilepaths)
    segPoly_paths <- .get_RSDS_tilepaths(segPoly_RSDS)

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
    raster::crs(trainingSP) <- crs

    # Assign training points to tiles
    trainingSP[["tileName"]] <- sp::over(trainingSP, ts[["tiles"]])[["tileName"]]
    unique_tiles <- unique(trainingSP[["tileName"]])

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
      "  Training set : ", trainingPts@id, "\n",
      "  Training pts : ", length(trainingSP), "\n",
      "  Tiles        : ", length(unique_tiles), "\n",
      sep = ""
    )


    ### EXTRACT DATA ----

    # Create progress bar
    pb <- .progbar(length(unique_tiles))

    # Read training data
    trainingData <- lapply(unique_tiles, function(tile_name){

      segPoly_path <- segPoly_paths[tile_name]

      # Read segments
      segPoly <- sf::st_read(segPoly_path, quiet = TRUE)

      # Get all metrics
      segData <- unname(lapply(names(metrics), function(met_name) read.csv(met_paths[[met_name]][tile_name], row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)))

      # Check matching row names
      for(i in 1:length(metrics)){
        if(!all(segPoly[[segID]] == row.names(segData[[i]]))){
          stop("Row names for '", metrics[[i]]@name, "' do not match '", segID, "' field for segments in tile '", tile_name, "'")
        }
      }

      # Assemble all metrics
      segPoly <- dplyr::bind_cols(segPoly[,segID], do.call(cbind, segData))

      # Get training points for tile
      trainingTile <- sf::st_as_sf(trainingSP[trainingSP$tileName == tile_name,])

      # Get segment polygons intersection with points
      trainingPoly <- segPoly[as.numeric(sf::st_intersects(trainingTile, segPoly)),]
      sf::st_geometry(trainingPoly) <- NULL

      pb$tick()

      cbind(
        row.names = trainingTile$trainingID,
        trainingSetID = trainingPts@id,
        trainingPtID  = trainingTile$trainingID,
        tileName      = tile_name,
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

    # Get file paths
    segPoly_paths <- .get_RSDS_tilepaths(segPoly_RSDS)
    out_paths     <- .get_RSDS_tilepaths(segClassPoly_RSDS)
    met_paths     <- lapply(metrics, .get_RSDS_tilepaths)

    # Read classifier
    classifier <- readRDS(classifierFile)

  ### READ CLASS EDITS ----

    cEdits <- suppressWarnings(rgdal::ogrInfo(classEdits@SHPfile)$have_features)

    if(cEdits){

      classEdits <- APfun::APSHPread(classEdits@SHPfile)
      raster::crs(classEdits) <- proj

      cTiles <- setNames(lapply( sp::over(
        ts[["tiles"]],
        classEdits,
        returnList = TRUE
      ), row.names), ts[["tiles"]][["tileName"]])
    }


  ### CREATE WORKER ----

    # Run process
    worker <- function(tileName){

      segPoly_path <- segPoly_paths[tileName]
      out_path     <- out_paths[tileName]

      # Read segments
      segPoly <- sf::st_read(segPoly_path, quiet = TRUE)

      # Get all metrics
      segData <- unname(lapply(names(metrics), function(met_name){
        read.csv(met_paths[[met_name]][tileName],
                 row.names = 1, check.names = FALSE,
                 stringsAsFactors = FALSE)
      }))

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
      if(length(elected) > 0){
        segPoly[["votePrc"]]  <- sapply(1:length(elected), function(i){
          el <- elected[i]
          if(is.na(el)) NA else votes[i, el]
        })
      }

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

            whichEdit <- segPoly[[segID]] %in% editSegs[[segID]]
            segPoly[whichEdit,][["segClass"]] <- to
            segPoly[whichEdit,][["votePrc" ]] <- NA
          }
        }
      }

      # Save output
      sf::st_write(segPoly, out_path, delete_dsn = file.exists(out_path), quiet = TRUE)

      if(file.exists(out_path)) "Success" else stop("Failed to create output")

    }

  ### APPLY WORKER ----

    # Get tiles for processing
    procTiles <- .processing_tiles(out_paths, overwrite, tileNames)

    # Process
    status <- .doitlive(procTiles, worker)

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
    segPoly <- sf::st_read(segClassPoly_path, quiet = TRUE)

    # Get unclassified raster segments
    segRas <- raster::raster(segRas_path)

    # Convert 'segRas' segment numbers to class numbers
    segClassRas <- raster::setValues(segRas, factor(
      segPoly[["segClass"]][match(segRas[], segPoly[[segID]])],
      levels = segClasses))

    # Save output
    raster::writeRaster(segClassRas, out_path, overwrite = overwrite, datatype = "INT1U")

    if(file.exists(out_path)) "Success" else stop("Failed to create output")

  }

  ### APPLY WORKER ----

  # Get tiles for processing
  procTiles <- .processing_tiles(out_paths, overwrite, tileNames)

  # Process
  status <- .doitlive(procTiles,worker)

  # Report
  .statusReport(status)

  # Conclude
  .conclusion(tim)

}
