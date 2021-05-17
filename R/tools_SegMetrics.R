#' Segment Metrics - Textural
#'
#' @export

SegMetricsTex <- function(segRas_RSDS, segPoly_RSDS, PCA_RSDS, nDSM_RSDS, out_RSDS, segID,
                          tileNames = NULL, overwrite = FALSE){

  tim <- .headline("SEGMENT METRICS - TEXTURAL")

  ### INPUT CHECKS ----

    # Check extensions
    .check_extension(PCA_RSDS,    "tif")
    .check_extension(nDSM_RSDS,   "tif")
    .check_extension(segRas_RSDS, "tif")
    .check_extension(out_RSDS,    "csv")

    # Check complete inputs
    .check_complete_input(segRas_RSDS,  tileNames)
    .check_complete_input(segPoly_RSDS, tileNames)
    .check_complete_input(PCA_RSDS,     tileNames)
    .check_complete_input(nDSM_RSDS,    tileNames)

    # Get file paths
    segRas_paths  <- .get_RSDS_tilepaths(segRas_RSDS)
    segPoly_paths <- .get_RSDS_tilepaths(segPoly_RSDS)
    PCA_paths     <- .get_RSDS_tilepaths(PCA_RSDS)
    nDSM_paths    <- .get_RSDS_tilepaths(nDSM_RSDS)
    out_paths     <- .get_RSDS_tilepaths(out_RSDS)

    # Get tilepaths
    ts <- .get_tilescheme()

  ### CREATE EMPTY METRICS TABLE ----

    metNames <- names(radiomics::calc_features(radiomics::glcm(matrix(1), n_grey = 1)))
    metNames <- c(segID, gsub("^glcm_", "glcm_PCA_",  metNames), gsub("^glcm_", "glcm_nDSM_", metNames))

    emptyMetrics <- setNames(data.frame(matrix(ncol = length(metNames), nrow = 0)), metNames)

  ### CREATE WORKER ----

    # Run process
    worker <- function(tileName){

      # Get tile
      tile <- ts[tileName]

      # Get output file path
      out_path     <- out_paths[tileName]
      PCA_path     <- PCA_paths[tileName]
      nDSM_path    <- nDSM_paths[tileName]
      segRas_path  <- segRas_paths[tileName]
      segPoly_path <- segPoly_paths[tileName]

      # Read segment DBF
      segDBF <- .read_poly_attributes(segPoly_path)
      if(!segID %in% names(segDBF)) stop("Could not find '", segID, "' in the '", segPoly_RSDS@name, "' dataset")

      # Compute metrics
      tileMetrics <- if(nrow(segDBF) > 0){

        # Read segments
        segRas <- raster::raster(segRas_path)

        # Get first PCA and nDSM
        PCA  <- raster::brick(PCA_path)[[1]]
        nDSM <- raster::raster(nDSM_path)

        # Get minimum value and adjust value range of PCA (the PCA cannot have negative values)
        minv <- raster::cellStats(PCA, "min")
        PCAmv <- PCA - minv

        # Remove values below 0 (cannot have negative values for GLCM)
        nDSM[nDSM < 0] <- 0
        nDSM[is.na(nDSM)] <- 0

        # Compute GLCMs
        GLCM_PCA  <- suppressWarnings(ForestTools::glcm(segRas, PCAmv, n_grey = 8))
        GLCM_nDSM <- suppressWarnings(ForestTools::glcm(segRas,  nDSM, n_grey = 8))

        # Rename, combine and reorder
        names(GLCM_PCA)  <- gsub("^glcm_", "glcm_PCA_",  names(GLCM_PCA ))
        names(GLCM_nDSM) <- gsub("^glcm_", "glcm_nDSM_", names(GLCM_nDSM))
        GLCM             <- cbind(GLCM_PCA, GLCM_nDSM[,-1])
        names(GLCM)[1]   <- segID
        row.names(GLCM)  <- GLCM[[segID]]
        GLCM             <- GLCM[as.character(segDBF[[segID]]),]
        GLCM[[segID]]    <- segDBF[[segID]]

        GLCM

      # Return empty table of metrics
      }else emptyMetrics

      # Write output
      write.csv(tileMetrics, out_path, row.names = FALSE, na = "")

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
                           segID,  tileNames = NULL, overwrite = FALSE){

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
  segRas_paths  <- .get_RSDS_tilepaths(segRas_RSDS)
  segPoly_paths <- .get_RSDS_tilepaths(segPoly_RSDS)
  ortho_paths   <- .get_RSDS_tilepaths(ortho_RSDS)
  out_paths     <- .get_RSDS_tilepaths(out_RSDS)

  # Get tilepaths
  ts <- .get_tilescheme()

  ### CREATE EMPTY METRICS TABLE ----

  metNames <- names(bands)
  if(all(c("R", "G", "B") %in% metNames)) metNames <- c(metNames, "GLI", "VARI", "NGRDI", "NGBDI", "NRBDI")
  if(all(c("IR", "R")     %in% metNames)) metNames <- c(metNames, "NDVI", "EVI2", "MSAVI2", "SAVI")
  metNames <- c(segID, apply(expand.grid(metNames, zonalFun), 1, paste, collapse="_"))

  emptyMetrics <- setNames(data.frame(matrix(ncol = length(metNames), nrow = 0)), metNames)


  ### CREATE WORKER ----

  # Run process
  worker <- function(tileName){

    # Get tile
    tile <- ts[tileName]

    # Get output file path
    out_path     <- out_paths[tileName]
    ortho_path   <- ortho_paths[tileName]
    segRas_path  <- segRas_paths[tileName]
    segPoly_path <- segPoly_paths[tileName]

    # Read segment DBF
    segDBF <- .read_poly_attributes(segPoly_path)
    if(!segID %in% names(segDBF)) stop("Could not find '", segID, "' in the '", segPoly_RSDS@name, "' dataset")

    # Compute tile metrics
    tileMetrics <- if(nrow(segDBF) > 0){

      # Read ortho and segment raster
      o <- raster::brick(ortho_path)
      segRas <- raster::raster(segRas_path)

      # Subset bands
      if(max(bands) > raster::nlayers(o)) stop("Ortho has fewer bands than those specified in the 'bands' argument")
      o <- o[[bands]]
      names(o) <- names(bands)

      # Produce indices (RGB)
      if(all(c("R", "G", "B") %in% names(o))){

        o[["GLI"]]    <- (2 * o$G - o$R - o$B + 0.01) / (2 * o$G + o$R + o$B+ 0.01)
        o[["VARI"]]   <- (o$G - o$R) / (o$G + o$R - o$B + 0.01)

        o[["NGRDI"]]  <- (o$G - o$R) / (o$G + o$R)
        o[["NGBDI"]]  <- (o$G - o$B) / (o$G + o$B)
        o[["NRBDI"]]  <- (o$R - o$B) / (o$R + o$B)

      }
      # Produce indices (Infrared)
      if(all(c("IR", "R") %in% names(o))){

        o[["NDVI"]]   <- (o$IR - o$R) / (o$IR + o$R)
        o[["EVI2"]]   <- (o$IR - o$R) / (o$IR + 2.4 * o$R + 1)
        o[["MSAVI2"]] <- (2 * o$IR + 1 - sqrt( (2 * o$IR + 1)^2 - 8 * (o$IR - o$R) )) / 2
        o[["SAVI"]]   <- ((o$IR - o$R) / (o$IR + o$R + 0.5)) * (1.5)

      }

      # Compute standard metrics
      specMetrics <- do.call(cbind, lapply(zonalFun, function(zf){
        specMetrics <- raster::zonal(o, segRas, fun = zf)
        colnames(specMetrics) <- paste0(colnames(specMetrics), "_", zf)
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
      emptyMetrics
    }

    write.csv(tileMetrics, out_path, row.names = FALSE)

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
                          zMin, zMax, segID,
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
  segRas_paths  <- .get_RSDS_tilepaths(segRas_RSDS)
  segPoly_paths <- .get_RSDS_tilepaths(segPoly_RSDS)
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
    segRas_path  <- segRas_paths[tileName]
    segPoly_path <- segPoly_paths[tileName]

    # Read segment DBF
    segDBF <- .read_poly_attributes(segPoly_path)
    if(!segID %in% names(segDBF)) stop("Could not find '", segID, "' in the '", segPoly_RSDS@name, "' dataset")

    # Compute tile metrics
    tileMetrics <- if(nrow(segDBF) > 0){

      # Read LAS tile
      LAStile <- .readLAStile(in_cat, tile = tile, select = LASselect)

      if(!is.null(LAStile)){

        # Normalize LAS tile
        LAStile <- .normalizeLAS(LAStile, DEMpath = DEM_path, zMin, zMax)

        if(!is.null(LAStile)){

          # Read segment rasters
          segRas <- raster::raster(segRas_path)

          # Get segment subset. Multiply by 1 to force it into a FLOAT format, otherwise it won't work
          segRas <- raster::crop(segRas, raster::extent(LAStile)) * 1

          # Assign segment ID to LAS points
          LAStile <- lidR::merge_spatial(LAStile, segRas, attribute = segID)

          if(!all(is.na(LAStile[[segID]]))){

            # Compute cloud statistics within segments
            LASmetrics <- as.data.frame(lidR::tree_metrics(LAStile, metricFormula, attribute = segID))

            # Reorder
            LASmetrics <- LASmetrics[match(segDBF[[segID]], LASmetrics[[segID]]),]
            LASmetrics[[segID]] <- segDBF[[segID]]

            # Remove unneeded columns
            LASmetrics <- LASmetrics[, !names(LASmetrics) %in% c("x.pos.t", "y.pos.t")]

            LASmetrics

          }else NULL
        }else NULL
      }else NULL
    }else NULL


    # If no LAS points were found or segment file was empty, create a dummy table
    if(is.null(tileMetrics)){

      dummyMetrics   <- emptyResult
      dummyMetrics[] <- NA
      dummyMetrics   <- dummyMetrics[rep(1, nrow(segDBF)), ]

      tileMetrics <- cbind(segDBF[,segID, drop = FALSE], dummyMetrics)
    }

    # Write table
    write.csv(tileMetrics, out_path, row.names = FALSE, na = "")

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

