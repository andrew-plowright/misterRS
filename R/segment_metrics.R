#' Segment Metrics - Textural
#'
#' @export

seg_metrics_tex <- function(seg_ras_rsds, seg_poly_rsds, img_rsds, out_rsds,
                            seg_id, band = 1,
                            prefix = "", n_grey = 16, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("SEGMENT METRICS - TEXTURAL")

  ### INPUT CHECKS ----

    # Check extensions
    .check_extension(seg_ras_rsds, "tif")
    .check_extension(img_rsds,    "tif")
    .check_extension(out_rsds,    "csv")

    # Check complete inputs
    .check_complete_input(seg_ras_rsds)
    .check_complete_input(seg_poly_rsds)
    .check_complete_input(img_rsds)

    # Get file paths
    seg_ras_paths  <- .get_rsds_tilepaths(seg_ras_rsds)
    seg_poly_paths <- .get_rsds_tilepaths(seg_poly_rsds)
    img_paths      <- .get_rsds_tilepaths(img_rsds)
    out_paths      <- .get_rsds_tilepaths(out_rsds)

    # Get tilepaths
    ts <- .get_tilescheme()

    cat("  Image            :", img_rsds@name, "\n")

  ### CREATE EMPTY METRICS TABLE ----

    met_names <- names(ForestTools:::.glcm_stats(1))
    met_names  <- c(seg_id, gsub("^glcm_", paste0(prefix, "glcm_"),  met_names))
    empty_metrics <- setNames(data.frame(matrix(ncol = length(met_names), nrow = 0)), met_names)

  ### CREATE WORKER ----

    # Run process
    tile_worker <-function(tile_name){

      # Get tile
      tile <- ts[tile_name]

      # Get output file path
      out_path      <- out_paths[tile_name]
      img_path      <- img_paths[tile_name]
      seg_ras_path  <- seg_ras_paths[tile_name]
      seg_poly_path <- seg_poly_paths[tile_name]

      # Read segment DBF
      seg_dbf <- .read_poly_attributes(seg_poly_path)
      if(!seg_id %in% names(seg_dbf)) stop("Could not find '", seg_id, "' in the '", seg_poly_rsds@name, "' dataset")

      # Compute metrics
      tile_metrics <- if(nrow(seg_dbf) > 0){

        # Read segments
        seg_ras <- terra::rast(seg_ras_path)

        # Get image
        img  <- terra::rast(img_path, lyrs = band)

        # Get minimum value and adjust value range (cannot have negative values)
        min_value <- terra::global(img, "min", na.rm = TRUE)[,1]
        img <- img - min_value

        # Remove values below 0 (cannot have NA values)
        img[is.na(img)] <- 0

        # Compute GLCMs
        glcm <- ForestTools::glcm(img, seg_ras, n_grey = n_grey)

        # Add prefix
        names(glcm)  <- gsub("^glcm_", paste0(prefix, "glcm_"),  names(glcm))

        # Add segment ID
        glcm <- cbind(row.names(glcm), glcm)
        colnames(glcm)[1] <- seg_id

        # Reorder to match 'seg_dbf'
        glcm             <- glcm[as.character(seg_dbf[[seg_id]]),]
        glcm[[seg_id]]   <- seg_dbf[[seg_id]]

        glcm

      # Return empty table of metrics
      }else empty_metrics

      # Write output
      write.csv(tile_metrics, out_path, row.names = FALSE, na = "")

      if(file.exists(out_path)) "Success" else "FAILED"
    }


  ### APPLY WORKER ----

    # Get tiles for processing
    queued_tiles <- .tile_queue(out_paths)

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

seg_metrics_spec <- function(seg_ras_rsds, seg_poly_rsds, img_rsds, out_rsds,
                           bands = c("R" = 1, "G" = 2, "B" = 3), zonalFun = c("mean", "sd"),
                           seg_id, prefix = NULL, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("SEGMENT METRICS - SPECTRAL")

  ### INPUT CHECKS ----

  # Check extensions
  .check_extension(img_rsds,  "tif")
  .check_extension(seg_ras_rsds, "tif")
  .check_extension(out_rsds,    "csv")

  # Check complete inputs
  .check_complete_input(seg_ras_rsds)
  .check_complete_input(seg_poly_rsds)
  .check_complete_input(img_rsds)

  # Get file paths
  seg_ras_paths  <- .get_rsds_tilepaths(seg_ras_rsds)
  seg_poly_paths <- .get_rsds_tilepaths(seg_poly_rsds)
  ortho_paths   <- .get_rsds_tilepaths(img_rsds)
  out_paths     <- .get_rsds_tilepaths(out_rsds)

  # Get tilepaths
  ts <- .get_tilescheme()

  do_metrics_RGB <- all(c("R", "G", "B") %in% names(bands))
  do_metrics_IR  <- all(c("IR", "R")     %in% names(bands))


  ### SET NAMES OF METRICS ----

  met_names_noprefix <- names(bands)
  if(do_metrics_RGB) met_names_noprefix <- c(met_names_noprefix, "GLI", "VARI", "NGRDI", "NGBDI", "NRBDI")
  if(do_metrics_IR)  met_names_noprefix <- c(met_names_noprefix, "NDVI", "EVI2", "MSAVI2", "SAVI")

  met_names_prefix <- setNames(paste0(prefix, met_names_noprefix), met_names_noprefix)


  ### CREATE EMPTY METRICS TABLE ----

  met_names_final <- c(seg_id, apply(expand.grid(met_names_prefix, zonalFun), 1, paste, collapse="_"))
  empty_metrics <- setNames(data.frame(matrix(ncol = length(met_names_final), nrow = 0)), met_names_final)


  ### CREATE WORKER ----

  # Run process
  tile_worker <-function(tile_name){

    # Get tile
    tile <- ts[tile_name]

    # Get output file path
    out_path     <- out_paths[tile_name]
    ortho_path   <- ortho_paths[tile_name]
    seg_ras_path  <- seg_ras_paths[tile_name]
    seg_poly_path <- seg_poly_paths[tile_name]

    # Read segment DBF
    seg_dbf <- .read_poly_attributes(seg_poly_path)
    if(!seg_id %in% names(seg_dbf)) stop("Could not find '", seg_id, "' in the '", seg_poly_rsds@name, "' dataset")

    # Compute tile metrics
    tile_metrics <- if(nrow(seg_dbf) > 0){

      # Read ortho and segment raster
      o <- terra::rast(ortho_path)
      seg_ras <- terra::rast(seg_ras_path)

      # Subset bands
      if(max(bands) >  terra::nlyr(o)) stop("Ortho has fewer bands than those specified in the 'bands' argument")
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
      specMetrics <- do.call(cbind, lapply(zonalFun, function(zf){
        specMetrics <- terra::zonal(o, seg_ras, fun = zf)
        colnames(specMetrics) <- paste0(c("zone", names(o)), "_", zf)
        return(specMetrics)
      }))

      # Remove extra 'zone' colum
      specMetrics <- data.frame(
        zoneID = specMetrics[,1],
        specMetrics[,!grepl("^zone_", colnames(specMetrics)), drop = FALSE]
      )

      # Keep only segments found in 'seg_poly', and re-order them to match
      names(specMetrics)[names(specMetrics) == "zoneID"] <- seg_id
      row.names(specMetrics) <- specMetrics[[seg_id]]
      specMetrics <- specMetrics[as.character(seg_dbf[[seg_id]]),]
      specMetrics[[seg_id]] <- seg_dbf[[seg_id]]

      specMetrics

    }else{
      empty_metrics
    }

    write.csv(tile_metrics, out_path, row.names = FALSE, na = "")

    if(file.exists(out_path)) "Success" else "FAILED"
  }

  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_paths)

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

seg_metrics_las <- function(seg_ras_rsds, seg_poly_rsds, in_cat, dem_rsds, out_rsds,
                          z_min, z_max,
                          seg_id, prefix = NULL,
                          is_ground_classified = NULL, is_full_classified = NULL, is_rgb = NULL, is_intensity = NULL,
                          ...){

  .env_misterRS(list(...))

  process_timer <- .headline("SEGMENT METRICS - LAS")

  ### INPUT CHECKS ----

  # Check extensions
  .check_extension(seg_ras_rsds, "tif")
  .check_extension(out_rsds,    "csv")

  # Check input RSDS are complete
  .check_complete_input(seg_ras_rsds)
  .check_complete_input(seg_poly_rsds)
  .check_complete_input(dem_rsds)

  # Get file paths
  seg_ras_paths  <- .get_rsds_tilepaths(seg_ras_rsds)
  seg_poly_paths <- .get_rsds_tilepaths(seg_poly_rsds)
  DEM_paths      <- .get_rsds_tilepaths(dem_rsds)
  out_paths      <- .get_rsds_tilepaths(out_rsds)

  # Get tile scheme
  ts <- .get_tilescheme()

  ### PREPARE DATA ----

  # Auto-detect classification and RGB
  if(any(sapply(list(is_rgb, is_intensity, is_full_classified, is_ground_classified), is.null))){

    testLAS <- lidR::readLAS(las_cat$filename[[1]])

    if(is.null(is_full_classified)){
      is_full_classified <- .is_las_full_classified(testLAS)
    }

    if(is.null(is_ground_classified)){
      is_ground_classified <- .is_las_ground_classified(testLAS)
    }

    if(is.null(is_rgb)){
      is_rgb <- .is_las_rgb(testLAS)
    }

    if(is.null(is_intensity)){
      is_intensity <- .is_las_intensity(testLAS)
    }
  }

  # Variables that depend on classification and RGB setting

  las_variables <- list("Z" = "Z")
  las_select    <- c("xyz")
  by_thresh     <- NULL
  formula_args  <- NULL

  if(is_rgb){
    las_select    <- paste0(las_select, c("RGB"))
    rgb_variables <- c("R", "G", "B", 'NGI' , 'NGB', 'NRB', 'VAg', 'GLI')
    las_variables <- c(las_variables, as.list(setNames(rgb_variables, rgb_variables)))
    by_thresh     <- c(by_thresh, rgb_variables)
  }
  if(is_intensity){
    las_select    <- paste0(las_select, c("i"))
    las_variables <- c(las_variables, list("I" = "Intensity"))
    by_thresh     <- c(by_thresh, "I")
  }
  if(is_ground_classified | is_full_classified){
    las_select    <- paste0(las_select, c("c"))
    las_variables <- c(las_variables, list("class" = "Classification"))

    if(is_full_classified)   formula_args <- c(formula_args, "full_classification = TRUE")
    if(is_ground_classified) formula_args <- c(formula_args, "ground_classification = TRUE")
  }
  if(!is.null(by_thresh)){
    formula_args <- c(formula_args, paste0("by_thresh=c('", paste(by_thresh, collapse = "', '") , "')"))
  }

  # Create formula
  metric_formula <- as.formula(paste0(
    "~misterRS:::.metric_fun(",

    # LAS Variables
    paste(paste(names(las_variables), '=', las_variables), collapse = ", "),

    # Other arguments
    if(!is.null(formula_args)) paste0(", ", paste(formula_args, collapse = ", ")),

    ")"
  ))

  # Create empty result
  empty_result <- eval(lazyeval::as_call(metric_formula), envir = setNames(rep(list(0), length(las_variables)), las_variables))
  empty_result[] <- NA

  cat(
    "  Intensity           : ", is_intensity, "\n",
    "  Classified (Ground) : ", is_ground_classified, "\n",
    "  Classified (Full)   : ", is_full_classified, "\n",
    "  RGB                 : ", is_rgb, "\n",
    "\n", sep = ""
  )

  ### CREATE WORKER ----

  # Run process
  tile_worker <- function(tile_name){

    # Get tile
    tile <- ts[tile_name]

    # Get output file path
    out_path     <- out_paths[tile_name]
    DEM_path     <- DEM_paths[tile_name]
    seg_ras_path  <- seg_ras_paths[tile_name]
    seg_poly_path <- seg_poly_paths[tile_name]

    # Read segment DBF
    seg_dbf <- .read_poly_attributes(seg_poly_path)
    if(!seg_id %in% names(seg_dbf)) stop("Could not find '", seg_id, "' in the '", seg_poly_rsds@name, "' dataset")

    # Compute tile metrics
    tile_metrics <- if(nrow(seg_dbf) > 0){

      # Read LAS tile
      las_tile <- .read_las_tile(in_cat, tile = tile, select = las_select)

      if(!is.null(las_tile)){

        # Normalize LAS tile
        las_tile <- .normalize_las(las_tile, DEM_path = DEM_path, z_min, z_max)

        if(!is.null(las_tile)){

          # Read segment rasters
          seg_ras <- terra::rast(seg_ras_path)

          # Get segment subset. Multiply by 1 to force it into a FLOAT format, otherwise it won't work
          seg_ras <- terra::crop(seg_ras, lidR::ext(las_tile))

          # Assign segment ID to LAS points
          las_tile <- lidR::merge_spatial(las_tile, seg_ras, attribute = seg_id)

          if(!all(is.na(las_tile[[seg_id]]))){

            # Compute RGB indices
            if(is_rgb) las_tile <- .las_rgb_metrics(las_tile)

            # Compute cloud statistics within segments
            las_metrics <- lidR::crown_metrics(las_tile, metric_formula, attribute = seg_id) %>% as.data.frame()

            # Remove unneeded columns
            las_metrics <- las_metrics[, !names(las_metrics) %in% "geometry", drop = FALSE]

            # Reorder
            las_metrics <- las_metrics[match(seg_dbf[[seg_id]], las_metrics[[seg_id]]),]
            las_metrics[[seg_id]] <- seg_dbf[[seg_id]]

            # Add prefix
            names(las_metrics)[2:ncol(las_metrics)] <- paste0(prefix, names(las_metrics)[2:ncol(las_metrics)])

            las_metrics

          }else NULL
        }else NULL
      }else NULL
    }else NULL


    # If no LAS points were found or segment file was empty, create a dummy table
    if(is.null(tile_metrics)){

      empty_metrics <- empty_result[rep(1, nrow(seg_dbf)), ]
      names(empty_metrics) <- paste0(prefix, names(empty_metrics))

      tile_metrics <- cbind(seg_dbf[,seg_id, drop = FALSE], empty_metrics)
    }

    # Write table
    write.csv(tile_metrics, out_path, row.names = FALSE, na = "")

    if(file.exists(out_path)) "Success" else "FAILED"

  }

  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_paths)

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

