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
    pts  <- poly <- sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs( proj)), training_set = integer(), list(segClass = character()))

    sf::st_write(pts,  file_path, quiet = TRUE, layer = "points",   delete_layer = TRUE)
    sf::st_write(poly, file_path, quiet = TRUE, layer = "polygons", delete_layer = TRUE)

    con = DBI::dbConnect(RSQLite::SQLite(),dbname= file_path)
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
    SHPfile  = 'character'
  )
)


setMethod("show", "class_edits", function(object){

  vec_num <- 0
  if(file.exists(object@SHPfile)){

    info <- sf::st_layers(object@SHPfile)
    vec_num <- info$features[1]
  }

  cat(
    "CLASSIFICATION EDITS", "\n",
    "Vectors : ", vec_num,"\n",
    sep = ""
  )
})

#' Remote Sensing Dataset
#' @export

class_edits <- function(SHPpath, proj = getOption("misterRS.crs"), overwrite = FALSE){

  if(tools::file_ext(SHPpath) != "shp") stop("Classification Edits path should be a SHP file")

  if(proj == "" | is.na(proj) | is.null(proj)) stop("Invalid CRS")

  # Get absolute SHP file path
  SHPpath <- suppressMessages(R.utils::getAbsolutePath(SHPpath))

  # Check folder
  folder <- dirname(SHPpath)
  if(!dir.exists(folder)) dir.create(folder, recursive = TRUE)

  if(!file.exists(SHPpath) | overwrite){

    # Create Simple Feature object with blank geometry and empty attribute fields
    s <- sf::st_sf(geometry = sf::st_sfc(crs = proj), list(toClass = character(), fromClass = character()))

    sf::write_sf(s, SHPpath, layer_options = "SHPT=POLYGON", quiet = TRUE, delete_dsn = overwrite)
  }

  # Create new object
  new("class_edits", SHPfile = SHPpath)
}


.sql_extract_point <- function(con, in_sf, attribute_fields){

  if(nrow(in_sf) > 0){

    sql_template <- "SELECT %1$s FROM layer
      WHERE Intersects(layer.geom, st_point(%2$s, %3$s))
      AND fid IN (SELECT id FROM rtree_layer_geom WHERE minx <= %2$s AND maxx >= %2$s AND miny <= %3$s AND maxy >= %3$s)"

    attribute_query <- paste(attribute_fields, collapse =", ")

    do.call(rbind, lapply(1:nrow(in_sf), function(i){

      coords <- sf::st_coordinates(in_sf[i,])

      x <- coords[,"X"]
      y <- coords[,"Y"]

      sql <- sprintf(sql_template,attribute_query, x, y)

      cbind(
        type = 1,
        fid = in_sf$fid[i],
        training_set = in_sf$training_set[i],
        DBI::dbGetQuery(con, sql),
        segClass = in_sf$segClass[i]
      )
    }))
  }else NULL
}


.sql_extract_poly <- function(con, in_sf, attribute_fields){

  if(nrow(in_sf) > 0){

    sql_template <- "WITH in_poly(geom) AS (VALUES(ST_GeomFromText('%s'))) SELECT %s FROM layer, in_poly
    WHERE Intersects(layer.geom, in_poly.geom)
    AND fid IN (SELECT id FROM rtree_layer_geom, in_poly WHERE minx <= MbrMaxX(in_poly.geom) AND maxx >= MbrMinX(in_poly.geom) AND miny <= MbrMaxY(in_poly.geom) AND maxy >= MbrMinY(in_poly.geom))"

    attribute_query <- paste(attribute_fields, collapse =", ")

    do.call(rbind, lapply(1:nrow(in_sf), function(i){

      poly_text <- sf::st_as_text(sf::st_geometry(in_sf[i,]))

      sql <- sprintf(sql_template,poly_text, attribute_query)

      cbind(
        type = 2,
        fid = in_sf$fid[i],
        training_set = in_sf$training_set[i],
        DBI::dbGetQuery(con, sql),
        segClass = in_sf$segClass[i]
      )
    }))

  }else NULL
}

#' Extract Training data
#'
#' @export

training_data_extract <- function(training_data, attribute_set, seg_vts,  seg_id, overwrite = FALSE){

  process_timer <- .headline("EXTRACT TRAINING DATA")

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

    existing_fid_pts   <- DBI::dbGetQuery(train_con, "SELECT DISTINCT fid FROM data WHERE type = 1")[,"fid"]
    existing_fid_polys <- DBI::dbGetQuery(train_con, "SELECT DISTINCT fid FROM data WHERE type = 2")[,"fid"]

    training_polys <- training_polys[!training_polys$fid %in% existing_fid_polys,]
    training_pts   <- training_pts[!training_pts$fid %in% existing_fid_pts,]
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


    # Connect to VTS
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = seg_vts@gpkg)
    withr::defer(DBI::dbDisconnect(con))

    # Load spatialite
    withr::with_envvar(list(PATH = getOption("misterRS.mod_spatialite")), {
      DBI::dbExecute(con, "SELECT load_extension('mod_spatialite')")
    })

    # Select attributes
    attribute_names_all <- DBI::dbListFields(con, "layer")
    attribute_fields <- unname(unlist(sapply(attribute_set, function(attribute_prefix)  attribute_names_all[startsWith(attribute_names_all, attribute_prefix)])))

    # Extract
    extract_data <- rbind(
      .sql_extract_poly(con, training_polys, attribute_fields),
      .sql_extract_point(con, training_pts, attribute_fields)
    )

    if(any(is.na(extract_data))) stop("Extracted training data contained NA values")

    # Write data

    if(overwrite){
      DBI::dbWriteTable(train_con, name = "data", value = extract_data, overwrite = TRUE)
    }else{
      DBI::dbWriteTable(train_con, name = "data", value = extract_data, append = TRUE)
    }

    # Get total data
    total_data <- DBI::dbGetQuery(train_con, "SELECT COUNT(*) FROM data")[,1]

    cat(
      "  New data    : ", nrow(extract_data), "\n",
      "  Total data  : ", total_data, "\n",
      sep = ""
    )
  }

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


  cat("  Training sets: ", paste(training_set, collapse = ", "), "\n")

  # Drop unwanted columns
  all_data <- all_data[, !names(all_data) %in% c('type', 'fid', 'training_set'), drop = FALSE]

  # Check for rows with NA values and remove
  drop_rows <- apply(is.na(all_data), 1, any)
  if(any(drop_rows)){

    cat("  Remove rows:", length(drop_rows[drop_rows]), "\n")
    all_data <- all_data[!drop_rows,]
  }

  # Predictors: auto-select
  if(all("auto" %in% predictors)){

    # Use autoSelect to choose uncorrelated variables
    predictors <- as.character(Biocomb::select.cfs(all_data)$Biomarker)

    # Predictors: all variables
  }else if(is.null(predictors)){

    predictors <- "."

    # Predictors: defined by user
  }else{

    # Check that selected predictors exist
    notFound <- !predictors %in% names(all_data)

    if(any(notFound)) stop("  Following predictor variables not found in survey's Training Data:\n    ",
                           paste(predictors[notFound], collapse = "\n    "))
  }

  cat("  Following predictor variables selected:\n    ", paste(predictors, collapse = "\n    "), "\n")

  # Factorize 'segClass' attribute
  all_data$segClass <- as.factor(all_data$segClass)

  # Create classifier
  classifier <- randomForest::randomForest(
    as.formula(paste("segClass ~", paste(predictors, collapse = " + "))),
    data       = all_data,
    importance = TRUE,
    ntree      = 1000)

  cat("  OOB error rate:", round(classifier$err.rate[classifier$ntree, "OOB"]*100, digits=2), "%", "\n\n")

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

classify_seg_poly <- function(classifier_file, seg_vts, iteration, class_table, class_edits, seg_id, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("CLASSIFY POLYGON SEGMENTS")

  ### INPUT CHECKS ----

  # Check that inputs are complete
  .complete_input(seg_vts)

  # Get CRS
  proj <- getOption("misterRS.crs")

  # Get tile scheme
  ts <- .tilescheme()
  tiles_sf <- sf::st_as_sf(ts[["tiles"]])

  # Read classifier
  classifier <- readRDS(classifier_file)

  # Get class variables
  class_features <-  row.names(randomForest::importance(classifier))

  # Add column
  class_label <- paste0("class_", iteration)
  .vts_add_fields(seg_vts, class_label, field_type = "integer")

  # Add attribute set name to tile registry
  .vts_tile_reg_attribute_set(seg_vts, class_label)

  # Read in class edits

  class_edits <- sf::st_read(class_edits@SHPfile, quiet = TRUE)

  if(nrow(class_edits) > 0){

    class_edits_bytile <- setNames(sf::st_intersects(tiles_sf, class_edits), ts[["tiles"]][["tileName"]])
  }

  ### CREATE WORKER ----

  # Run process
  tile_worker <-function(tile_name){

    # Read treetops
    seg_poly <-  .vts_read(seg_vts, tile_name = tile_name)

    # Get all metrics
    seg_data <- sf::st_drop_geometry(seg_poly)[,class_features, drop=FALSE]

    # Classify according to most-voted class
    votes   <- randomForest:::predict.randomForest(classifier, seg_data, type = "vote")
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
        from <- strsplit(edit$fromClass, " ")[[1]]
        to   <- edit$toClass

        # Get segments that intersect with edit polygon
        edit_segs <- seg_poly[class_edits_bypoly[[i]],]

        # Subset according to specified 'fromClass' value (if specified)
        if(!is.na(from)) edit_segs <- edit_segs[edit_segs$segClass %in% from,]

        # Apply edit
        if(nrow(edit_segs) > 0){

          edit_which <- seg_poly[[seg_id]] %in% edit_segs[[seg_id]]
          seg_poly[edit_which,][[class_label]] <- to
        }
      }
    }

    # Convert to class IDs
    seg_poly[[class_label]] <- class_table$id[match(seg_poly[[class_label]], class_table$code)]

    # Select data to be written
    out_data <- sf::st_drop_geometry(seg_poly)[,c(seg_id, class_label)]

    # Write data
    .vts_write_attribute_set(data = out_data, out_vts = seg_vts, id_field = seg_id, attribute_set_field = class_label,tile_name = tile_name)

    return("Success")
  }

  ### APPLY WORKER ----

  # Get tiles for processing
  proc_tiles <- .tile_queue(seg_vts, class_label)

  # Process
  process_status <- .exe_tile_worker(proc_tiles, tile_worker)

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

classify_seg_ras <- function(seg_vts, seg_rts, seg_class_rts, iteration, seg_id, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("CLASSIFY RASTER SEGMENTS")

  ### INPUT CHECKS ----

  # Check that inputs are complete
  .complete_input(seg_rts)
  .complete_input(seg_vts)

  seg_paths <- .rts_tile_paths(seg_rts)
  out_paths <- .rts_tile_paths(seg_class_rts)

  class_label <- paste0("class_", iteration)

  ### CREATE WORKER ----

  # Run process
  tile_worker <-function(tile_name){

    # Get file paths

    seg_path <- seg_paths[tile_name]
    out_path <- out_paths[tile_name]

    # Read polygons (which have classes)
    seg_poly <-  .vts_read(seg_vts, tile_name = tile_name, field = c(seg_id, class_label))

    # Get unclassified raster segments
    seg_ras <- terra::rast(seg_path)

    # Classifiy values
    classified_ras_vals <- seg_poly[[class_label]][match(seg_ras[], seg_poly[[seg_id]])]

    # Create output
    seg_class_ras <- terra::setValues(seg_ras, classified_ras_vals)

    # Save output
    terra::writeRaster(seg_class_ras, out_path, overwrite = overwrite, datatype = "INT1U")

    if(file.exists(out_path)) "Success" else stop("Failed to create output")

  }

  ### APPLY WORKER ----

  # Get tiles for processing
  proc_tiles <- .tile_queue(seg_class_rts)

  # Process
  process_status <- .exe_tile_worker(proc_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)

}
