# Training data class ----

#' Training Data (class)
#' @export

setClass(
  "trainingdata",
  representation(
    name = 'character',
    file_path = 'character'
  )
)

setMethod("show", "trainingdata", function(object){

  if(file.exists(object@file_path)){

    lyrs <- sf::st_layers(object@file_path)

    features_points <- lyrs$features[lyrs$name == "points"]
    features_polys  <- lyrs$features[lyrs$name == "polygons"]
    features_data   <- if("data" %in% lyrs$name) lyrs$features[lyrs$name == "data"] else 0

  }else{

    features_points <- 0
    features_polys  <- 0
    features_data   <- 0
  }

  cat(
    "TRAINING DATASET", "\n",
    "Name      : ", object@name,  "\n",
    "File      : ", object@file_path, "\n",
    "Points    : ", features_points, "\n",
    "Polygons  : ", features_polys, "\n",
    "Data rows : ", features_data,

    sep = ""
  )
})


#' Training Data (constructor)
#' @export

training_data <- function(name, dir, proj = getOption("misterRS.crs"), overwrite = FALSE){

  if(proj == "" | is.na(proj) | is.null(proj)) stop("Invalid CRS")

  file_path <- file.path(dir, paste0(name, "_training_data.gpkg"))

  if(!file.exists(file_path) | overwrite){

    if(overwrite) file.remove(file_path)

    dir <- dirname(file_path)
    if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)

    # Create Simple Feature object with blank geometry and empty attribute fields
    pts  <- poly <- sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs( proj)), training_set = integer(), list(seg_class = character()))

    sf::st_write(pts,  file_path, quiet = TRUE, layer = "points",   delete_layer = TRUE)
    sf::st_write(poly, file_path, quiet = TRUE, layer = "polygons", delete_layer = TRUE)

    con = DBI::dbConnect(RSQLite::SQLite(), dbname= file_path)
    withr::defer(DBI::dbDisconnect(con))

    DBI::dbExecute(con, "UPDATE gpkg_geometry_columns SET geometry_type_name = 'POINT' WHERE table_name = 'points'")
    DBI::dbExecute(con, "UPDATE gpkg_geometry_columns SET geometry_type_name = 'POLYGON' WHERE table_name = 'polygons'")
  }

  # Create new object
  new("trainingdata", name = name,  file_path = file_path)
}

#' Classification edits (class)
#' @export

setClass(
  "class_edits",
  representation(
    file_path  = 'character'
  )
)


setMethod("show", "class_edits", function(object){

  vec_num <- 0
  if(file.exists(object@file_path)){

    info <- sf::st_layers(object@file_path)
    vec_num <- info$features[1]
  }

  cat(
    "CANOPY EDITS", "\n",
    "Vectors : ", vec_num,"\n",
    sep = ""
  )
})

#' Class Edits (constructor)
#' @export

class_edits <- function(name, dir, proj = getOption("misterRS.crs"), overwrite = FALSE){

  if(proj == "" | is.na(proj) | is.null(proj)) stop("Invalid CRS")

  file_path <- file.path(dir, paste0(name, "_class_edits.gpkg"))

  if(!file.exists(file_path) | overwrite){

    # Create Simple Feature object with blank geometry and empty attribute fields
    s <- sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(proj)), list(to_class = character(), from_class = character()))

    # Write to geopackage
    sf::st_write(s,  file_path, quiet = TRUE, layer = "edits", delete_layer = TRUE)

    # Set geometry type
    con = DBI::dbConnect(RSQLite::SQLite(), dbname = file_path)
    withr::defer(DBI::dbDisconnect(con))
    DBI::dbExecute(con, "UPDATE gpkg_geometry_columns SET geometry_type_name = 'POLYGON' WHERE table_name = 'edits'")
  }

  # Create new object
  new("class_edits", file_path = file_path)
}


.sql_extract_point <- function(seg_vts, training_pts, attribute_fields, existing_fids){

  if(nrow(training_pts) > 0){

    # Read segment data from points
    seg_data <- seg_vts$read_from_pts(training_pts, c("fid", attribute_fields))

    # Assign training data to polygons
    seg_data[["type"]]         <- 1
    seg_data[["training_set"]] <- training_pts[["training_set"]]
    seg_data[["training_fid"]] <- training_pts[["fid"]]
    seg_data[["seg_class"]]    <- training_pts[["seg_class"]]

    # Remove NAs (i.e.: points that intersect with nothing)
    seg_data <- seg_data[!is.na(seg_data[["fid"]]),]

    return(seg_data)

  }else NULL
}


.sql_extract_poly <- function(seg_vts, training_polys, attribute_fields, existing_fids){

  if(nrow(training_polys) > 0){

    # Get data from training polys
    seg_data <- seg_vts$read_from_polys(training_polys, c("fid", attribute_fields))

    # Assign training data to polygons
    for(i in 1:nrow(training_polys)){
      if(nrow(seg_data[[i]]) > 0){
        seg_data[[i]][["type"]] <- 2
        seg_data[[i]][["training_set"]] <- training_polys[["training_set"]][i]
        seg_data[[i]][["training_fid"]] <- training_polys[["fid"]][i]
        seg_data[[i]][["seg_class"]]    <- training_polys[["seg_class"]][i]
      }

    }

    seg_data <- do.call(rbind, seg_data)

    return(seg_data)

  }else NULL
}

.print_fids_to_delete <- function(training_fids, extract_data, training_type){

  if(training_type == "POLYGON"){
    training_type_code <- 2
  }else if(training_type == "POINT"){
    training_type_code <- 1
  }else stop("Unrecognized training type")

  empty_fids <- training_fids[!training_fids %in% subset(extract_data, type == training_type_code, "training_fid", drop = TRUE)]

  if(length(empty_fids) > 0){

    cat(crayon::yellow(
      "  Following *", training_type,"* FIDs contained no data or overlap over existing segments and should be deleted:\n",
      "  Selection query: \"fid\" in (", paste(empty_fids, collapse = ", ") ,")\n"
      , sep=""))
  }
}

#' Extract Training data
#'
#' @export

training_data_extract <- function(training_data, attribute_set, seg_vts,   overwrite = FALSE){

  process_timer <- .headline("EXTRACT TRAINING DATA")

  seg_vts$connect()

  .complete_input(seg_vts, attribute = "geom")

  # Read training points and polygons
  training_pts   <- sf::st_read(training_data@file_path, "points",   fid_column_name ="fid", quiet = TRUE)
  training_polys <- sf::st_read(training_data@file_path, "polygons", fid_column_name ="fid", quiet = TRUE)

  # Connect to training data
  train_con = DBI::dbConnect(RSQLite::SQLite(),dbname= training_data@file_path)
  withr::defer(DBI::dbDisconnect(train_con))

  cat(
    "  Total pts   : ", nrow(training_pts), "\n",
    "  Total polys : ", nrow(training_polys), "\n",
    "  Overwrite   : ", overwrite, "\n",
    sep = ""
  )

  if(!overwrite & "data" %in% DBI::dbListTables(train_con)){

    existing_fid_pts   <- DBI::dbGetQuery(train_con, "SELECT DISTINCT training_fid FROM data WHERE type = 1")[,"training_fid"]
    existing_fid_polys <- DBI::dbGetQuery(train_con, "SELECT DISTINCT training_fid FROM data WHERE type = 2")[,"training_fid"]

    training_polys <- training_polys[!training_polys$fid %in% existing_fid_polys,]
    training_pts   <- training_pts  [!training_pts$fid   %in% existing_fid_pts,]
  }

  cat(
    "\n",
    "  New pts     : ", nrow(training_pts), "\n",
    "  New polys   : ", nrow(training_polys), "\n",
    sep = ""
  )

  if(nrow(training_polys) + nrow(training_pts) == 0){

    cat("  No new training data extracted\n", sep = "")

  }else{

    # Select attributes
    attribute_names_all <- seg_vts$fields()
    attribute_fields <- unname(unlist(sapply(paste0(attribute_set, "_"), function(attribute_prefix)  attribute_names_all[startsWith(attribute_names_all, attribute_prefix)])))

    # Extract data
    extract_data <- rbind(
      .sql_extract_poly( seg_vts, training_polys, attribute_fields),
      .sql_extract_point(seg_vts, training_pts,   attribute_fields)
    )

    # Remove duplicated segments
    extract_data <- extract_data[!duplicated(extract_data[["fid"]]),]

    # Remove segments that were already in the training dataset
    existing_fids <- DBI::dbGetQuery(train_con, "SELECT fid FROM data")[,'fid']
    extract_data <- extract_data[!extract_data[["fid"]] %in% existing_fids,]

    # Print message indicating which training geometry to delete
    .print_fids_to_delete(training_polys[["fid"]], extract_data, "POLYGON")
    .print_fids_to_delete(training_pts[["fid"]],   extract_data, "POINT")

    if(any(is.na(extract_data))) stop("Extracted training data contained NA values")

    # Write data
    if(nrow(extract_data) > 0){
      if(overwrite){
        DBI::dbWriteTable(train_con, name = "data", value = extract_data, overwrite = TRUE)
      }else{
        DBI::dbWriteTable(train_con, name = "data", value = extract_data, append = TRUE)
      }
    }

    # Get total data
    total_data <- DBI::dbGetQuery(train_con, "SELECT COUNT(*) FROM data")[,1]

    cat(
      "  New data    : ", nrow(extract_data), "\n",
      "  Total data  : ", total_data, "\n",
      sep = ""
    )
  }

  seg_vts$disconnect()

  # Conclude
  .conclusion(process_timer)
}


#' Create classifier
#'
#' @export

classifier_create <- function(training_data, training_set, classifier_file = NULL, seg_id, predictors = NULL,
                              overwrite = FALSE){

  process_timer <- .headline("CREATE CLASSIFIER")

  if(!is.null(classifier_file)){
    if(file.exists(classifier_file) & !overwrite){
      stop("Classifier already exists. Set 'overwrite' to TRUE", call. = FALSE)
    }
  }

  # Connect to training data
  train_con = DBI::dbConnect(RSQLite::SQLite(),dbname= training_data@file_path)
  withr::defer(DBI::dbDisconnect(train_con))

  if(!"data" %in% DBI::dbListTables(train_con)) stop("No training data has been extracted")

  # Get data
  all_data <- DBI::dbGetQuery(train_con, sprintf("SELECT * FROM data WHERE training_set IN (%s)", paste(training_set, collapse = ", ")))

  cat("  Training sets :", paste(training_set, collapse = ", "), "\n")

  # Drop unwanted columns
  all_data <- all_data[, !names(all_data) %in% c('type', 'fid', 'training_set', 'training_fid'), drop = FALSE]

  # Check for rows with NA values and remove
  drop_rows <- apply(is.na(all_data), 1, any)
  if(any(drop_rows)){

    cat("  Remove rows :", length(drop_rows[drop_rows]), "\n")
    all_data <- all_data[!drop_rows,]
  }

  # Predictors: auto-select
  if(all("auto" %in% predictors)){

    # Use autoSelect to choose uncorrelated variables
    #predictors <- as.character(Biocomb::select.cfs(all_data)$Biomarker)
    stop("Auto selection of indicators has been removed since the 'Biocomb' package is obsolete")

    # Predictors: all variables
  }else if(is.null(predictors)){

    predictors <- "."

    # Predictors: defined by user
  }else{

    # Check that selected predictors exist
    notFound <- !predictors %in% names(all_data)

    if(any(notFound)) stop("  Predictor variables not found in  Training Data:\n    ",
                           paste(predictors[notFound], collapse = "\n    "))
  }

  cat("  Predictor variables selected :\n    ", paste(predictors, collapse = "\n    "), "\n")

  # Factorize 'seg_class' attribute
  all_data[["seg_class"]] <- as.factor(all_data[["seg_class"]])

  # Create classifier
  classifier <- randomForest::randomForest(
    as.formula(paste("seg_class ~", paste(predictors, collapse = " + "))),
    data       = all_data,
    importance = TRUE,
    ntree      = 1000)

  cat("  OOB error rate :", round(classifier$err.rate[classifier$ntree, "OOB"]*100, digits=2), "%", "\n\n")

  # create classifier output folder
  classifierDir <- dirname(classifier_file)
  if(!dir.exists(classifierDir)) dir.create(classifierDir, recursive = TRUE)

  # Save classifier
  saveRDS(classifier, classifier_file)

  # Conclude
  .conclusion(process_timer)
}


#' Classify polygonal segments
#'
#' @export

classify_seg_poly <- function(classifier_file, seg_vts, iteration, class_table, class_edits, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("CLASSIFY POLYGON SEGMENTS")

  ### INPUT CHECKS ----

  # Read classifier
  classifier <- readRDS(classifier_file)

  # Get class variables
  class_features <- row.names(randomForest::importance(classifier))

  # Get attributes
  attributes <- unique(sapply(strsplit(class_features, "_"),"[", 1))

  seg_vts$connect()

  # Check that inputs are complete
  for(attribute in attributes) .complete_input(seg_vts, attribute= attribute)

  # Get tile scheme
  ts <- .tilescheme()

  # Add column
  class_label <- paste0("class_", iteration)
  seg_vts$add_field(class_label, "INTEGER")

  # Add attribute set name to tile registry
  seg_vts$add_attribute(class_label)

  # Read in class edits
  class_edits <- sf::st_read(class_edits@file_path, quiet = TRUE)
  if(nrow(class_edits) > 0){
    class_edits_bytile <- setNames(sf::st_intersects(ts[["tiles"]], class_edits), ts[["tile_name"]])
  }

  # Unique id field
  seg_id <- seg_vts$id_field


  ### CREATE WORKER ----

  # Run process
  tile_worker <-function(tile_name){

    # Read all
    seg_poly <- seg_vts$read_tile(tile_name = tile_name, fields = c(seg_id, "geom", class_features))

    if(nrow(seg_poly) > 0){
      # Drop FID
      #seg_poly <- seg_poly[,names(seg_poly) != "fid"]

      # Subset predictors
      tile_predictors <- seg_poly[,class_features, drop = FALSE]

      # Classify according to most-voted class
      votes   <- randomForest:::predict.randomForest(classifier, tile_predictors, type = "vote")
      elected <- colnames(votes)[apply(votes, 1, function(x) which.max(x)[1])]
      seg_poly[[class_label]] <- elected

      # votePrc  <- if(length(elected) > 0){
      #  sapply(1:length(elected), function(i){if(is.na(elected[i])) NA else votes[i, elected[i]]})
      # }else{
      #   numeric()
      # }

      # Manual edits
      if((nrow(class_edits) > 0) && (length(class_edits_bytile[[tile_name]]) > 0)){

        # Class edits for this tile
        class_edits_tile <- class_edits[class_edits_bytile[[tile_name]],]

        # Intersection between class edits and polygons
        class_edits_bypoly <- sf::st_intersects(class_edits_tile, seg_poly)

        for(i in 1:nrow(class_edits_tile)){

          edit <- class_edits_tile[i,]

          # Get to/from classes
          from <- strsplit(edit$from_class, " ")[[1]]
          to   <- edit$to_class

          # Get segments that intersect with edit polygon
          edit_segs <- seg_poly[class_edits_bypoly[[i]],]

          # Subset according to specified 'from_Class' value (if specified)
          if(!is.na(from)) edit_segs <- edit_segs[edit_segs[[class_label]] %in% from,]

          # Apply edit
          if(nrow(edit_segs) > 0){

            edit_which <- seg_poly[[seg_id]] %in% edit_segs[[seg_id]]
            seg_poly[edit_which,][[class_label]] <- to
          }
        }
      }

      # Convert to numerical class IDs
      seg_poly[[class_label]] <- class_table$id[match(seg_poly[[class_label]], class_table$code)]

      # Keep only data that is needed
      seg_poly <- sf::st_drop_geometry(seg_poly)[, c(seg_id, class_label)]
    }

    # Write attributes
    seg_vts$update_data(data = seg_poly, tile_name = tile_name, attribute = class_label, overwrite = TRUE)

    return("Success")
  }

  ### APPLY WORKER ----

  # Get tiles for processing
  proc_tiles <- .tile_queue(seg_vts, attribute_set_name = class_label)

  seg_vts$disconnect()

  # Process
  process_status <- .exe_tile_worker(proc_tiles, tile_worker, cluster_vts = "seg_vts")

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

classify_seg_ras <- function(seg_vts, seg_rts, seg_class_rts, iteration, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("CLASSIFY RASTER SEGMENTS")

  ### INPUT CHECKS ----

  class_label <- paste0("class_", iteration)

  seg_vts$connect()

  # Check that inputs are complete
  .complete_input(seg_rts)
  .complete_input(seg_vts, attribute = class_label)

  seg_vts$disconnect()

  # Unique id field
  seg_id <- seg_vts$id_field

  ### CREATE WORKER ----

  # Run process
  tile_worker <-function(tile_name){

    # Get file paths
    seg_path <- seg_rts$tile_path(tile_name)
    out_path <- seg_class_rts$tile_path(tile_name)

    # Read polygons (which have classes)
    seg_poly <-  seg_vts$read_tile(tile_name = tile_name, field = c(seg_id, class_label))

    # Get unclassified raster segments
    seg_ras <- terra::rast(seg_path)

    # Classifiy values
    classified_ras_vals <- seg_poly[[class_label]][match(seg_ras[], seg_poly[[seg_id]])]

    # Create output
    seg_class_ras <- terra::setValues(seg_ras, classified_ras_vals)

    # Save output
    terra::writeRaster(seg_class_ras, out_path, overwrite = TRUE, datatype = "INT1U")

    if(file.exists(out_path)) "Success" else stop("Failed to create output")

  }

  ### APPLY WORKER ----

  # Get tiles for processing
  proc_tiles <- .tile_queue(seg_class_rts)

  # Process
  process_status <- .exe_tile_worker(proc_tiles, tile_worker, cluster_vts = "seg_vts")

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)

}
