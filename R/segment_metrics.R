#' Segment Metrics - Textural
#'
#' @export

seg_metrics_tex <- function(seg_ras_rsds, seg_poly_rsds, img_rsds, out_rsds, seg_id, band = 1,
                          tile_names = NULL, overwrite = FALSE, prefix = "", n_grey = 16){

  process_timer <- .headline("SEGMENT METRICS - TEXTURAL")

  ### INPUT CHECKS ----

    # Check extensions
    .check_extension(seg_ras_rsds, "tif")
    .check_extension(img_rsds,    "tif")
    .check_extension(out_rsds,    "csv")

    # Check complete inputs
    .check_complete_input(seg_ras_rsds,  tile_names)
    .check_complete_input(seg_poly_rsds, tile_names)
    .check_complete_input(img_rsds,     tile_names)

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
    queued_tiles <- .tile_queue(out_paths, overwrite, tile_names)

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
                           seg_id,  tile_names = NULL, overwrite = FALSE, prefix = NULL){

  process_timer <- .headline("SEGMENT METRICS - SPECTRAL")

  ### INPUT CHECKS ----

  # Check extensions
  .check_extension(img_rsds,  "tif")
  .check_extension(seg_ras_rsds, "tif")
  .check_extension(out_rsds,    "csv")

  # Check complete inputs
  .check_complete_input(seg_ras_rsds,  tile_names)
  .check_complete_input(seg_poly_rsds, tile_names)
  .check_complete_input(img_rsds,   tile_names)

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
      o <- raster::brick(ortho_path)
      seg_ras <- raster::raster(seg_ras_path)

      # Subset bands
      if(max(bands) > raster::nlayers(o)) stop("Ortho has fewer bands than those specified in the 'bands' argument")
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
        specMetrics <- raster::zonal(o, seg_ras, fun = zf)
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

    write.csv(tile_metrics, out_path, row.names = FALSE)

    if(file.exists(out_path)) "Success" else "FAILED"
  }

  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_paths, overwrite, tile_names)

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
                          metric_fun,
                          z_min, z_max, seg_id, prefix = NULL,
                          tile_names = NULL, overwrite = FALSE){

  process_timer <- .headline("SEGMENT METRICS - LAS")

  ### INPUT CHECKS ----

  # Check extensions
  .check_extension(seg_ras_rsds, "tif")
  .check_extension(out_rsds,    "csv")

  # Check input RSDS are complete
  .check_complete_input(seg_ras_rsds,  tile_names)
  .check_complete_input(seg_poly_rsds, tile_names)
  .check_complete_input(dem_rsds,     tile_names)

  # Get file paths
  seg_ras_paths  <- .get_rsds_tilepaths(seg_ras_rsds)
  seg_poly_paths <- .get_rsds_tilepaths(seg_poly_rsds)
  DEM_paths     <- .get_rsds_tilepaths(dem_rsds)
  out_paths     <- .get_rsds_tilepaths(out_rsds)

  # Get tile scheme
  ts <- .get_tilescheme()

  ### PREPARE DATA ----

  # Set metric formula. This is a bizarre way of doing things, but this is the way it has
  # to be for us to swap out different functions that get passed on to 'lidR::tree_metrics()'
  if(metric_fun == "RGB"){

    metricFormula <- as.formula("~misterRS:::.metric_fun_RGB(Z, R, G, B)")
    emptyResult   <- misterRS:::.metric_fun_RGB(0, 0, 0, 0)
    LASselect     <- "xyzRGB"

  }else if(metric_fun == "classified"){

    metricFormula <- as.formula("~misterRS:::.metric_fun_classified(Z, Intensity, Classification)")
    emptyResult   <- misterRS:::.metric_fun_classified(0, 0, 0)
    LASselect     <- "xyzci"

  }else stop("Unrecognized 'metric_fun' input: '", metric_fun, "'", call. = FALSE)


  ### CREATE WORKER ----

  # Run process
  tile_worker <-function(tile_name){

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
      LAStile <- .read_las_tile(in_cat, tile = tile, select = LASselect)

      if(!is.null(LAStile)){

        # Normalize LAS tile
        LAStile <- .normalize_las(LAStile, DEMpath = DEM_path, z_min, z_max)

        if(!is.null(LAStile)){

          # Read segment rasters
          seg_ras <- terra::rast(seg_ras_path)

          # Get segment subset. Multiply by 1 to force it into a FLOAT format, otherwise it won't work
          seg_ras <- terra::crop(seg_ras, lidR::ext(LAStile))

          # Assign segment ID to LAS points
          LAStile <- lidR::merge_spatial(LAStile, seg_ras, attribute = seg_id)

          if(!all(is.na(LAStile[[seg_id]]))){

            # Compute cloud statistics within segments
            LASmetrics <- as.data.frame(lidR::crown_metrics(LAStile, metricFormula, attribute = seg_id))

            # Remove unneeded columns
            LASmetrics <- LASmetrics[, !names(LASmetrics) %in% "geometry"]

            # Reorder
            LASmetrics <- LASmetrics[match(seg_dbf[[seg_id]], LASmetrics[[seg_id]]),]
            LASmetrics[[seg_id]] <- seg_dbf[[seg_id]]

            # Add prefix
            names(LASmetrics)[2:ncol(LASmetrics)] <- paste0(prefix, names(LASmetrics)[2:ncol(LASmetrics)])

            LASmetrics

          }else NULL
        }else NULL
      }else NULL
    }else NULL


    # If no LAS points were found or segment file was empty, create a dummy table
    if(is.null(tile_metrics)){

      dummyMetrics   <- emptyResult
      dummyMetrics[] <- NA
      dummyMetrics   <- dummyMetrics[rep(1, nrow(seg_dbf)), ]
      names(dummyMetrics) <- paste0(prefix, names(dummyMetrics))

      tile_metrics <- cbind(seg_dbf[,seg_id, drop = FALSE], dummyMetrics)
    }

    # Write table
    write.csv(tile_metrics, out_path, row.names = FALSE, na = "")

    if(file.exists(out_path)) "Success" else "FAILED"

  }

  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_paths, overwrite, tile_names)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)
}


.metric_by_height <- function(spec, heightThresh = c("p50", "p75")){

  # Compute total number of points and Z percentiles
  allPts    <- nrow(spec)
  Zp        <- quantile(spec$Z, c(.5, .75, .9, .99))
  names(Zp) <- paste0("p", gsub("%", "", names(Zp)))

  # Remove non-finite values
  spec <- data.frame(lapply(spec, function(x) replace(x, !is.finite(x), NA)))

  # Standard Z metrics
  Zstand <- data.frame(

    allPts      = allPts,

    ZquadMean   = sqrt(mean(spec$Z^2)),
    Zmean       = mean(spec$Z),
    Zstddev     = sd(spec$Z),
    Ziq         = IQR(spec$Z),
    Zp50        = Zp["p50"],
    Zp75        = Zp["p75"],
    Zp90        = Zp["p90"],
    Zp99        = Zp["p99"],

    PrcOver1    = length(spec$Z[spec$Z>1]) / allPts,
    PrcOver2    = length(spec$Z[spec$Z>2]) / allPts,
    PrcOver3    = length(spec$Z[spec$Z>3]) / allPts
  )

  # Divide spectral values by Z percentile percentile
  varNames <- names(spec)[names(spec) != "Z"]
  Pspec <- c(list(spec[,varNames, drop = FALSE]), lapply(Zp[heightThresh], function(Zpp) subset(spec, subset = Z >= Zpp, select = varNames)))
  Pspec <- lapply(1:length(Pspec), function(i) setNames(Pspec[[i]], paste(names(Pspec[[i]]), names(Pspec)[i], sep = "_")))

  # Spectral statistics by Z percentile
  Zspec <- rbind(c(
    Pspec %>% lapply(sapply, mean,   na.rm = TRUE) %>% unlist %>% setNames(paste0(names(.), "mn")),
    Pspec %>% lapply(sapply, median, na.rm = TRUE) %>% unlist %>% setNames(paste0(names(.), "med"))
  ))

  # Combine
  cbind(Zstand, Zspec)
}

.metric_fun_RGB <- function(Z, R, G, B){

  # Remove '0' values for RGB (otherwise several fractional indices will mess up)
  R[R == 0] <- 1
  B[B == 0] <- 1
  G[G == 0] <- 1

  # Create spectral indices
  spec <- data.frame(
    Z     = Z,
    R     = R,
    G     = G,
    B     = B,
    RG    = R/G,
    RB    = R/B,
    NGI = (G - R) / (G + R),     # Normalized Green Red Difference Index (NGRDI)
    NGB = (G - B) / (G + B),
    NRB = (R - B) / (R + B),
    VAg = (G - R) / (G + R - B + 0.01), #	Visible Atmospherically Resistant Index (VARIg)
    GLI   = (2 * G - R - B) / (2 * G + R + B) # Green Leaf Index (GLI)
  )

  .metric_by_height(spec)
}


.metric_fun_classified <- function(Z, I, class){

  all  = length(Z)

  grnd  = length(class[class %in% 2])
  lvg   = length(class[class %in% 3])
  hvg   = length(class[class %in% c(4,5)])
  bld   = length(class[class %in% 6])
  water = length(class[class %in% 9])

  cbind(

    .metric_by_height(data.frame(Z = Z, I = I)),

    grndPrc  = grnd / all, # Percentage of ground points
    lvgPrc   = lvg  / all, # Percentage of low vegetation
    hvgPrc   = hvg  / all, # Percentage of high vegetation
    bldPrc   = bld  / all  # Percentage of building points
  )
}

