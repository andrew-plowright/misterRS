#' Segment Metrics - Textural
#'
#' @export

SegMetricsTex <- function(segRas_RSDS, segPoly_RSDS, img_RSDS, out_RSDS, segID, band = 1,
                          tileNames = NULL, overwrite = FALSE, prefix = ""){

  cat("*** This function needs a terra update ***", "\n\n")

  tim <- .headline("SEGMENT METRICS - TEXTURAL")


  ### INPUT CHECKS ----

    # Check extensions
    .check_extension(segRas_RSDS, "tif")
    .check_extension(img_RSDS,    "tif")
    .check_extension(out_RSDS,    "csv")

    # Check complete inputs
    .check_complete_input(segRas_RSDS,  tileNames)
    .check_complete_input(segPoly_RSDS, tileNames)
    .check_complete_input(img_RSDS,     tileNames)

    # Get file paths
    segras_paths  <- .get_RSDS_tilepaths(segRas_RSDS)
    segpoly_paths <- .get_RSDS_tilepaths(segPoly_RSDS)
    img_paths     <- .get_RSDS_tilepaths(img_RSDS)
    out_paths     <- .get_RSDS_tilepaths(out_RSDS)

    # Get tilepaths
    ts <- .get_tilescheme()

    cat("  Image            :", img_RSDS@name, "\n")


  ### CREATE EMPTY METRICS TABLE ----

    met_names <- names(ForestTools:::.GLCMstats(1))
    met_names  <- c(segID, gsub("^glcm_", paste0(prefix, "glcm_"),  met_names))
    empty_metrics <- setNames(data.frame(matrix(ncol = length(met_names), nrow = 0)), met_names)

  ### CREATE WORKER ----

    # Run process
    worker <- function(tileName){

      # Get tile
      tile <- ts[tileName]

      # Get output file path
      out_path     <- out_paths[tileName]
      img_path     <- img_paths[tileName]
      segras_path  <- segras_paths[tileName]
      segpoly_path <- segpoly_paths[tileName]

      # Read segment DBF
      segDBF <- .read_poly_attributes(segpoly_path)
      if(!segID %in% names(segDBF)) stop("Could not find '", segID, "' in the '", segPoly_RSDS@name, "' dataset")

      # Compute metrics
      tile_metrics <- if(nrow(segDBF) > 0){

        # Read segments
        segras <- raster::raster(segras_path)

        # Get image
        img  <- raster::raster(img_path, band = band)

        # Get minimum value and adjust value range (cannot have negative values)
        min_value <- raster::cellStats(img, "min", na.rm = TRUE)
        img <- img - min_value

        # Remove values below 0 (cannot have NA values)
        img[is.na(img)] <- 0

        # Compute GLCMs
        glcm <- ForestTools::glcm(segras, img, n_grey = 8)

        # Rename, combine and reorder
        names(glcm)  <- gsub("^glcm_", paste0(prefix, "glcm_"),  names(glcm))
        names(glcm)[1]   <- segID
        row.names(glcm)  <- glcm[[segID]]
        glcm             <- glcm[as.character(segDBF[[segID]]),]
        glcm[[segID]]    <- segDBF[[segID]]

        glcm

      # Return empty table of metrics
      }else empty_metrics

      # Write output
      write.csv(tile_metrics, out_path, row.names = FALSE, na = "")

      if(file.exists(out_path)) "Success" else "FAILED"
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



#' Segment Metrics - Spectral
#'
#' @export

SegMetricsSpec <- function(segRas_RSDS, segPoly_RSDS, ortho_RSDS, out_RSDS,
                           bands = c("R" = 1, "G" = 2, "B" = 3), zonalFun = c("mean", "sd"),
                           segID,  tileNames = NULL, overwrite = FALSE, prefix = NULL){

  tim <- .headline("SEGMENT METRICS - SPECTRAL")

  ### INPUT CHECKS ----

  # Check extensions
  .check_extension(ortho_RSDS,  "tif")
  .check_extension(segRas_RSDS, "tif")
  .check_extension(out_RSDS,    "csv")

  # Check complete inputs
  .check_complete_input(segRas_RSDS,  tileNames)
  .check_complete_input(segPoly_RSDS, tileNames)
  .check_complete_input(ortho_RSDS,   tileNames)

  # Get file paths
  segras_paths  <- .get_RSDS_tilepaths(segRas_RSDS)
  segpoly_paths <- .get_RSDS_tilepaths(segPoly_RSDS)
  ortho_paths   <- .get_RSDS_tilepaths(ortho_RSDS)
  out_paths     <- .get_RSDS_tilepaths(out_RSDS)

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

  met_names_final <- c(segID, apply(expand.grid(met_names_prefix, zonalFun), 1, paste, collapse="_"))
  empty_metrics <- setNames(data.frame(matrix(ncol = length(met_names_final), nrow = 0)), met_names_final)


  ### CREATE WORKER ----

  # Run process
  worker <- function(tileName){

    # Get tile
    tile <- ts[tileName]

    # Get output file path
    out_path     <- out_paths[tileName]
    ortho_path   <- ortho_paths[tileName]
    segras_path  <- segras_paths[tileName]
    segpoly_path <- segpoly_paths[tileName]

    # Read segment DBF
    segDBF <- .read_poly_attributes(segpoly_path)
    if(!segID %in% names(segDBF)) stop("Could not find '", segID, "' in the '", segPoly_RSDS@name, "' dataset")

    # Compute tile metrics
    tile_metrics <- if(nrow(segDBF) > 0){

      # Read ortho and segment raster
      o <- raster::brick(ortho_path)
      segras <- raster::raster(segras_path)

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
        specMetrics <- raster::zonal(o, segras, fun = zf)
        colnames(specMetrics) <- paste0(c("zone", names(o)), "_", zf)
        return(specMetrics)
      }))

      # Remove extra 'zone' colum
      specMetrics <- data.frame(
        zoneID = specMetrics[,1],
        specMetrics[,!grepl("^zone_", colnames(specMetrics)), drop = FALSE]
      )

      # Keep only segments found in 'segPoly', and re-order them to match
      names(specMetrics)[names(specMetrics) == "zoneID"] <- segID
      row.names(specMetrics) <- specMetrics[[segID]]
      specMetrics <- specMetrics[as.character(segDBF[[segID]]),]
      specMetrics[[segID]] <- segDBF[[segID]]

      specMetrics

    }else{
      empty_metrics
    }

    write.csv(tile_metrics, out_path, row.names = FALSE)

    if(file.exists(out_path)) "Success" else "FAILED"
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


#' Segment Metrics - LAS
#'
#' @export

SegMetricsLAS <- function(segRas_RSDS, segPoly_RSDS, in_cat, DEM_RSDS, out_RSDS,
                          metricFunc,
                          zMin, zMax, segID, prefix = NULL,
                          tileNames = NULL, overwrite = FALSE){

  tim <- .headline("SEGMENT METRICS - LAS")

  ### INPUT CHECKS ----

  # Check extensions
  .check_extension(segRas_RSDS, "tif")
  .check_extension(out_RSDS,    "csv")

  # Check input RSDS are complete
  .check_complete_input(segRas_RSDS,  tileNames)
  .check_complete_input(segPoly_RSDS, tileNames)
  .check_complete_input(DEM_RSDS,     tileNames)

  # Get file paths
  segras_paths  <- .get_RSDS_tilepaths(segRas_RSDS)
  segpoly_paths <- .get_RSDS_tilepaths(segPoly_RSDS)
  DEM_paths     <- .get_RSDS_tilepaths(DEM_RSDS)
  out_paths     <- .get_RSDS_tilepaths(out_RSDS)

  # Get tile scheme
  ts <- .get_tilescheme()

  ### PREPARE DATA ----

  # Set metric formula. This is a bizarre way of doing things, but this is the way it has
  # to be for us to swap out different functions that get passed on to 'lidR::tree_metrics()'
  if(metricFunc == "RGB"){

    metricFormula <- as.formula("~misterRS:::.RGB(Z, R, G, B)")
    emptyResult   <- misterRS:::.RGB(0, 0, 0, 0)
    LASselect     <- "xyzRGB"

  }else if(metricFunc == "classified"){

    metricFormula <- as.formula("~misterRS:::.classified(Z, Intensity, Classification)")
    emptyResult   <- misterRS:::.classified(0, 0, 0)
    LASselect     <- "xyzci"

  }else stop("Unrecognized 'metricFunc' input: '", metricFunc, "'", call. = FALSE)


  ### CREATE WORKER ----

  # Run process
  worker <- function(tileName){

    # Get tile
    tile <- ts[tileName]

    # Get output file path
    out_path     <- out_paths[tileName]
    DEM_path     <- DEM_paths[tileName]
    segras_path  <- segras_paths[tileName]
    segpoly_path <- segpoly_paths[tileName]

    # Read segment DBF
    segDBF <- .read_poly_attributes(segpoly_path)
    if(!segID %in% names(segDBF)) stop("Could not find '", segID, "' in the '", segPoly_RSDS@name, "' dataset")

    # Compute tile metrics
    tile_metrics <- if(nrow(segDBF) > 0){

      # Read LAS tile
      LAStile <- .readLAStile(in_cat, tile = tile, select = LASselect)

      if(!is.null(LAStile)){

        # Normalize LAS tile
        LAStile <- .normalizeLAS(LAStile, DEMpath = DEM_path, zMin, zMax)

        if(!is.null(LAStile)){

          # Read segment rasters
          segras <- terra::rast(segras_path)

          # Get segment subset. Multiply by 1 to force it into a FLOAT format, otherwise it won't work
          segras <- terra::crop(segras, lidR::ext(LAStile))

          # Assign segment ID to LAS points
          LAStile <- lidR::merge_spatial(LAStile, segras, attribute = segID)

          if(!all(is.na(LAStile[[segID]]))){

            # Compute cloud statistics within segments
            LASmetrics <- as.data.frame(lidR::crown_metrics(LAStile, metricFormula, attribute = segID))

            # Remove unneeded columns
            LASmetrics <- LASmetrics[, !names(LASmetrics) %in% "geometry"]

            # Reorder
            LASmetrics <- LASmetrics[match(segDBF[[segID]], LASmetrics[[segID]]),]
            LASmetrics[[segID]] <- segDBF[[segID]]

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
      dummyMetrics   <- dummyMetrics[rep(1, nrow(segDBF)), ]
      names(dummyMetrics) <- paste0(prefix, names(dummyMetrics))

      tile_metrics <- cbind(segDBF[,segID, drop = FALSE], dummyMetrics)
    }

    # Write table
    write.csv(tile_metrics, out_path, row.names = FALSE, na = "")

    if(file.exists(out_path)) "Success" else "FAILED"

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


.metricsByHeight <- function(spec, heightThresh = c("p50", "p75")){

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

.RGB <- function(Z, R, G, B){

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

  .metricsByHeight(spec)
}


.classified <- function(Z, I, class){

  all  = length(Z)

  grnd  = length(class[class %in% 2])
  lvg   = length(class[class %in% 3])
  hvg   = length(class[class %in% c(4,5)])
  bld   = length(class[class %in% 6])
  water = length(class[class %in% 9])

  cbind(

    .metricsByHeight(data.frame(Z = Z, I = I)),

    grndPrc  = grnd / all, # Percentage of ground points
    lvgPrc   = lvg  / all, # Percentage of low vegetation
    hvgPrc   = hvg  / all, # Percentage of high vegetation
    bldPrc   = bld  / all  # Percentage of building points
  )
}

