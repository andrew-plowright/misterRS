#' Segment Metrics - Textural
#'
#' @export

seg_metrics_tex <- function(seg_rts, seg_vts, img_rts, attribute_set,
                            seg_id, band = 1, discretize_range = NULL, n_grey = 16, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("SEGMENT METRICS - TEXTURAL")

  ### INPUT CHECKS ----

  # Check complete inputs
  .complete_input(seg_rts)
  .complete_input(seg_vts)
  .complete_input(img_rts)

  # Get file paths
  seg_ras_paths <- .rts_tile_paths(seg_rts)
  img_paths     <- .rts_tile_paths(img_rts)

  # Get tilepaths
  ts <- .tilescheme()

  cat("  Image            :", img_rts@name, "\n")

  ### GET RANGE ----

  if(is.null(discretize_range)){
    discretize_range <- unlist(.metadata(img_rts, "range")[[band]])
  }

  ### METRIC FIELDS ----

  metric_fields <- names(GLCMTextures::glcm_metrics(matrix()))
  metric_fields <- paste0(attribute_set, "_", metric_fields)

  # Add columns to GPKG
  .vts_add_fields(seg_vts, metric_fields, field_type = "MEDIUMINT")

  # Add attribute set name to tile registry
  .vts_tile_reg_attribute_set(seg_vts, attribute_set)

  ### CREATE WORKER ----

  # Run process
  tile_worker <-function(tile_name){

    # Get tile
    tile <- ts[tile_name]

    # Get output file path
    img_path      <- img_paths[tile_name]
    seg_ras_path  <- seg_ras_paths[tile_name]

    # Get seg IDs
    seg_data <- .vts_read(seg_vts, tile_name = tile_name)

    # Compute metrics
    tile_metrics <- if(nrow(seg_data) > 0){

      # Read segments
      seg_ras <- terra::rast(seg_ras_path)

      # Get image
      img <- terra::rast(img_path, lyrs = band)

      # Cap min.max va
      img[img < min(discretize_range)] <- min(discretize_range)
      img[img > max(discretize_range)] <- max(discretize_range)

      # Get minimum value and adjust value range (cannot have negative values)
      #min_value <- terra::global(img, "min", na.rm = TRUE)[,1]
      #img <- img - min_value

      # Remove values below 0 (cannot have NA values)
      #img[is.na(img)] <- 0

      # Compute GLCMs
      glcm_metrics <- ForestTools::glcm(img, seg_ras, n_grey = n_grey, discretize_range = discretize_range)

      # Add prefix
      names(glcm_metrics)  <- paste0(attribute_set, "_", names(glcm_metrics))

      # Reorder
      glcm_metrics <- glcm_metrics[as.character(seg_data[[seg_id]]),]

      # Remove NAs
      glcm_metrics[is.na(glcm_metrics)] <- 0

      # Convert to integer
      glcm_metrics <- as.data.frame(lapply(glcm_metrics, function(x) as.integer(x * 10000 )))

      # Attach to existing data
      cbind(seg_data[,!names(seg_data) %in% names(glcm_metrics)], glcm_metrics)


    # Return empty table of metrics
    }else NULL

    # Write attributes
    .vts_write_attribute_set(tile_metrics, seg_vts, "fid", attribute_set, tile_name)


    return("Success")
  }


  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(seg_vts, attribute_set)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)
}



#' Segment Metrics - Spectral
#'
#' @export

seg_metrics_spec <- function(seg_rts, seg_vts, img_rts, attribute_set, seg_id,
                           bands = c("R" = 1, "G" = 2, "B" = 3), zonalFun = c("mean", "sd"),
                           ...){

  .env_misterRS(list(...))

  process_timer <- .headline("SEGMENT METRICS - SPECTRAL")

  ### INPUT CHECKS ----

  # Check complete inputs
  .complete_input(seg_rts)
  .complete_input(seg_vts)
  .complete_input(img_rts)

  # Get file paths
  seg_ras_paths <- .rts_tile_paths(seg_rts)
  ortho_paths   <- .rts_tile_paths(img_rts)

  # Get tilepaths
  ts <- .tilescheme()

  do_metrics_RGB <- all(c("R", "G", "B") %in% names(bands))
  do_metrics_IR  <- all(c("IR", "R")     %in% names(bands))

  ### SET NAMES OF METRICS ----

  met_names_noprefix <- names(bands)
  if(do_metrics_RGB) met_names_noprefix <- c(met_names_noprefix, "GLI", "VARI", "NGRDI", "NGBDI", "NRBDI")
  if(do_metrics_IR)  met_names_noprefix <- c(met_names_noprefix, "NDVI", "EVI2", "MSAVI2", "SAVI")

  # With prefixes
  met_names_prefix <- setNames(paste0(attribute_set, "_", met_names_noprefix), met_names_noprefix)

  # With zonal functions
  met_names_full <- as.vector(outer(met_names_prefix, zonalFun, paste, sep="_"))

  # Add columns to GPKG
  .vts_add_fields(seg_vts, met_names_full, field_type = "MEDIUMINT")

  # Add attribute set name to tile registry
  .vts_tile_reg_attribute_set(seg_vts, attribute_set)


  ### CREATE WORKER ----

  # Run process
  tile_worker <-function(tile_name){

    # Get tile
    tile <- ts[tile_name]

    # Get output file path
    ortho_path   <- ortho_paths[tile_name]
    seg_ras_path  <- seg_ras_paths[tile_name]

    # Get seg IDs
    seg_data <- .vts_read(seg_vts, tile_name = tile_name)

    # Compute tile metrics
    tile_metrics <- if(nrow(seg_data) > 0){

      # Read ortho and segment raster
      o <- terra::rast(ortho_path)
      seg_ras <- terra::rast(seg_ras_path)

      # Subset bands
      if(max(bands) > terra::nlyr(o)) stop("Ortho has fewer bands than those specified in the 'bands' argument")
      o <- o[[bands]]
      names(o) <- met_names_prefix[names(bands)]

      # Produce indices (RGB)
      if(do_metrics_RGB){

        oR <- o[[met_names_prefix["R"]]]
        oG <- o[[met_names_prefix["G"]]]
        oB <- o[[met_names_prefix["B"]]]

        o[[met_names_prefix["GLI"]]]    <- (2 * oG - oR - oB + 0.01) / (2 * oG + oR + oB+ 0.01)
        o[[met_names_prefix["VARI"]]]   <- (oG - oR) / (oG + oR - oB + 0.01)

        o[[met_names_prefix["NGRDI"]]]  <- (oG - oR) / (oG + oR)
        o[[met_names_prefix["NGBDI"]]]  <- (oG - oB) / (oG + oB)
        o[[met_names_prefix["NRBDI"]]]  <- (oR - oB) / (oR + oB)

      }

      # Produce indices (Infrared)
      if(do_metrics_IR){

        oR <- o[[met_names_prefix["R"]]]
        oIR <- o[[met_names_prefix["IR"]]]

        o[[met_names_prefix["NDVI"]]]   <- (oIR - oR) / (oIR + oR)
        o[[met_names_prefix["EVI2"]]]   <- (oIR - oR) / (oIR + 2.4 * oR + 1)
        o[[met_names_prefix["MSAVI2"]]] <- (2 * oIR + 1 - sqrt( (2 * oIR + 1)^2 - 8 * (oIR - oR) )) / 2
        o[[met_names_prefix["SAVI"]]]   <- ((oIR - oR) / (oIR + oR + 0.5)) * (1.5)

      }

      # Compute standard metrics
      spec_metrics <- do.call(cbind, lapply(zonalFun, function(zf){
        spec_metrics <- terra::zonal(o, seg_ras, fun = zf)
        colnames(spec_metrics) <- paste0(c("zone", names(o)), "_", zf)
        return(spec_metrics)
      }))

      # Set row names
      row.names(spec_metrics) <- spec_metrics[,1]

      # Remove extra 'zone' colum
      spec_metrics <- spec_metrics[,!grepl("^zone_", colnames(spec_metrics)), drop = FALSE]

      # Reorder
      spec_metrics <- spec_metrics[as.character(seg_data[[seg_id]]),]

      # Remove NAs
      spec_metrics[is.na(spec_metrics)] <- 0

      # Convert to integer
      spec_metrics <- as.data.frame(lapply(spec_metrics, function(x) as.integer(x * 10000 )))

      # Attach to existing data
      cbind(seg_data[,!names(seg_data) %in% names(spec_metrics)], spec_metrics)

    }else NULL



    # Write attributes
    .vts_write_attribute_set(tile_metrics, seg_vts, "fid", attribute_set, tile_name)

    return("Success")
  }

  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(seg_vts, attribute_set)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)
}


#' Segment Metrics - LAS
#'
#' @export

seg_metrics_las <- function(seg_rts, seg_vts, in_cat, dem_rts, attribute_set, seg_id,
                          z_min = 0, z_max = 100,
                          ground_class = NULL, full_class = NULL, rgb = NULL,
                          ...){

  .env_misterRS(list(...))

  process_timer <- .headline("SEGMENT METRICS - LAS")

  ### INPUT CHECKS ----

  # Check inputs are complete
  .complete_input(seg_rts)
  .complete_input(seg_vts)
  .complete_input(dem_rts)

  # Auto-detect classification and RGB
  if(any(sapply(list(rgb, full_class, ground_class), is.null))){

    testLAS <- lidR::readLAS(las_cat$filename[[1]])

    if(is.null(full_class)) full_class <- .is_las_full_classified(testLAS)
    if(is.null(ground_class)) ground_class <- .is_las_ground_classified(testLAS)
    if(is.null(rgb)) rgb <- .is_las_rgb(testLAS)
    # if(is.null(intensity)) intensity <- .is_las_intensity(testLAS)

  }

  cat(
    "  Classified (Ground) : ", ground_class, "\n",
    "  Classified (Full)   : ", full_class, "\n",
    "  RGB                 : ", rgb, "\n",
    "\n", sep = ""
  )

  ### PREPARE DATA ----

  # Get file paths
  seg_ras_paths <- .rts_tile_paths(seg_rts)
  dem_paths     <- .rts_tile_paths(dem_rts)

  # Get tile scheme
  ts <- .tilescheme()

  # Variables that depend on classification and RGB setting
  las_variables <- list("Z" = "Z")
  las_select    <- c("xyz")
  by_thresh     <- NULL
  formula_args  <- NULL

  if(rgb){
    las_select    <- paste0(las_select, c("RGB"))
    rgb_variables <- c("R", "G", "B", 'NGI' , 'NGB', 'NRB', 'VAg', 'GLI')
    las_variables <- c(las_variables, as.list(setNames(rgb_variables, rgb_variables)))
    by_thresh     <- c(by_thresh, rgb_variables)
  }

  if(ground_class | full_class){
    las_select    <- paste0(las_select, c("c"))
    las_variables <- c(las_variables, list("class" = "Classification"))

    if(full_class)   formula_args <- c(formula_args, "full_classification = TRUE")
    if(ground_class) formula_args <- c(formula_args, "ground_classification = TRUE")
  }

  if(!is.null(by_thresh)){
    formula_args <- c(formula_args, paste0("by_thresh=c('", paste(by_thresh, collapse = "', '") , "')"))
  }

  # if(intensity){
  #   las_select    <- paste0(las_select, c("i"))
  #   las_variables <- c(las_variables, list("I" = "Intensity"))
  #   by_thresh     <- c(by_thresh, "I")
  # }

  # Create formula
  metric_formula <- as.formula(paste0(
    "~misterRS:::.metric_fun(",

    # LAS Variables
    paste(paste(names(las_variables), '=', las_variables), collapse = ", "),

    # Other arguments
    if(!is.null(formula_args)) paste0(", ", paste(formula_args, collapse = ", ")),

    ")"
  ))


  ### ENSURE THAT OUTPUT GPKG HAS APPROPRIATE COLUMNS ----

  # Create empty result
  empty_result <- eval(lazyeval::as_call(metric_formula), envir = setNames(rep(list(0), length(las_variables)), las_variables))
  metric_fields <- paste0(attribute_set, "_", names(empty_result))

  # Add columns to GPKG
  .vts_add_fields(seg_vts, metric_fields, field_type = "MEDIUMINT")

  # Add attribute set name to tile registry
  .vts_tile_reg_attribute_set(seg_vts, attribute_set)


  ### CREATE WORKER ----

  # Run process
  tile_worker <- function(tile_name){

    # Get tile
    tile <- ts[tile_name]

    # Get output file path
    dem_path     <- dem_paths[tile_name]
    seg_ras_path  <- seg_ras_paths[tile_name]

    # Get seg IDs
    seg_data <- .vts_read(seg_vts, tile_name = tile_name)

    # Compute tile metrics
    if(nrow(seg_data) > 0){

      # Fill in blank values for seg_dat
      seg_data[,metric_fields] <- 0

      # Read LAS tile
      las_tile <- .read_las_tile(in_cat, tile = tile, select = las_select)

      if(!is.null(las_tile)){

        # Normalize LAS tile
        las_tile <- .normalize_las(las_tile, DEM_path = dem_path, z_min, z_max)

      }

      if(!is.null(las_tile)){

        # Read segment rasters
        seg_ras <- terra::rast(seg_ras_path)

        # Get segment subset. Multiply by 1 to force it into a FLOAT format, otherwise it won't work
        seg_ras <- terra::crop(seg_ras, lidR::ext(las_tile))

        # Assign segment ID to LAS points
        las_tile <- lidR::merge_spatial(las_tile, seg_ras, attribute = seg_id)

        # Check if there are less than five usable points
        if(lidR::npoints(las_tile) - lidR:::fast_count_equal(las_tile$polyID, NA) < 5){
          las_tile <- NULL
        }
      }

      if(!is.null(las_tile)){

        # Compute RGB indices
        if(rgb) las_tile <- .las_rgb_metrics(las_tile)

        # Compute cloud statistics within segments
        las_metrics <- lidR::crown_metrics(las_tile, metric_formula, attribute = seg_id) %>% as.data.frame()

        # Reorder
        las_metrics <- las_metrics[match(seg_data[[seg_id]], las_metrics[[seg_id]]),]

        # Remove unneeded columns
        las_metrics <- las_metrics[, !names(las_metrics) %in% c("geometry", seg_id), drop = FALSE]

        # Add prefix
        names(las_metrics) <- paste0(attribute_set, "_", names(las_metrics))

        # Remove NA values
        las_metrics[is.na(las_metrics)] <- 0

        # Convert to integer
        las_metrics <- as.data.frame(lapply(las_metrics, function(x) as.integer(x * 10000 )))

        # Attach to existing data
        seg_data <- cbind(seg_data[,!names(seg_data) %in% names(las_metrics)], las_metrics)
      }
    }

    # Write attributes
    .vts_write_attribute_set(seg_data, seg_vts, "fid", attribute_set, tile_name)

    return("Success")
  }

  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(seg_vts, attribute_set)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)
}


.las_rgb_metrics <- function(las){

  las$R[las$R == 0] <- as.integer(1)
  las$G[las$G == 0] <- as.integer(1)
  las$B[las$B == 0] <- as.integer(1)


  spec_indices <- list(
    'NGI' = (las$G - las$R) / (las$G + las$R),
    'NGB' = (las$G - las$B) / (las$G + las$B),
    'NRB' = (las$R - las$B) / (las$R + las$B),
    'VAg' = (las$G - las$R) / (las$G + las$R - las$B + 0.01),
    'GLI' = (2 * las$G - las$R - las$B) / (2 * las$G + las$R + las$B)
  )

  spec_indices <- lapply(spec_indices, function(index) replace(index, !is.finite(index), NA))

  for(index_name in names(spec_indices)){
    las <- lidR::add_attribute(las, spec_indices[[index_name]], index_name)
  }

  return(las)
}

.metric_fun <- function(..., by_thresh = NULL, ground_classification = FALSE, full_classification = FALSE){

  p <- data.frame(...)

  # Count number of points
  p_length <- length(p$Z)

  # Compute Z percentiles
  z_threshs <- quantile(p$Z, c(.5, .75, .9)) %>% setNames(paste0("p", gsub("%", "", names(.))))

  # Stats for Z
  stats_out <- with(p,{
    data.frame(
      Z_qmn  = sqrt(mean(Z^2)),
      Z_mn   = mean(Z),
      Z_sd   = sd(Z),
      Z_iq   = IQR(Z),
      Z_p50  = z_threshs["p50"],
      Z_p75  = z_threshs["p75"],
      Z_p90  = z_threshs["p90"],

      Z_over1 = length(Z[Z>1]) / p_length,
      Z_over2 = length(Z[Z>2]) / p_length,
      Z_over3 = length(Z[Z>3]) / p_length
    )
  })


  # Subset variables according to z thresholds
  if(length(by_thresh) > 0){
    p_threshs <- lapply(names(z_threshs), function(z_thresh_name){

      z_thresh <- z_threshs[z_thresh_name]

      p_thresh <- p[p$Z >= z_thresh, by_thresh, drop = FALSE]

      names(p_thresh) <- paste0(names(p_thresh), "_", z_thresh_name)

      return(p_thresh)

    })
    p_threshs <- c(list(p[,by_thresh, drop = FALSE]), p_threshs)


    stats_thresh <- data.frame( as.list(do.call(c, lapply(p_threshs, function(p_thresh){
      c(
        apply(p_thresh, 2, mean) %>% setNames(paste0(names(.), "_mn")),
        apply(p_thresh, 2, max)  %>% setNames(paste0(names(.), "_mx"))
      )
    }))))
    stats_out <- cbind(stats_out, stats_thresh)
  }


  # Ground classification
  if(ground_classification){

    if(p_length == 0){

      stats_grnd <- data.frame(grnd = 0)

    }else{

      p_grnd <- p$class[p$class %in% 2]
      stats_grnd <- data.frame(grnd = length(p_grnd) / p_length)

    }

    stats_out <- cbind(stats_out, stats_grnd)
  }

  # Full classification
  if(full_classification){

    stats_class <- do.call(cbind, lapply(c(0,1,2,3), function(z_thresh){

      p_class <- p$class

      if(z_thresh > 0) p_class <- p_class[p$Z > z_thresh]

      p_class_length <- length(p_class)

      if(p_class_length == 0){

        stats <- data.frame(
          lvg   = 0,
          hvg   = 0,
          bld   = 0
        )

      }else{

        lvg   = length(p_class[p_class %in% 3])
        hvg   = length(p_class[p_class %in% c(4,5)])
        bld   = length(p_class[p_class %in% 6])

        stats <- data.frame(
          lvg = lvg  / p_class_length, # Percentage of low vegetation
          hvg = hvg  / p_class_length, # Percentage of high vegetation
          bld = bld  / p_class_length  # Percentage of building points
        )
      }

      if(z_thresh > 0) names(stats) <- paste0(names(stats), "_over", z_thresh)

      return(stats)
    }))

    stats_out <- cbind(stats_out, stats_class)
  }

  return(stats_out)
}

