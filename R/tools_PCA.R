#' Principal Component Analysis - local
#'
#' This is the first approach I tried: calculate a different PCA for each tile.
#' This means that the tiles don't necessarily match very well (edge effects are present),
#' but it does maximize local variation.
#'
#'
#' @export

PCAlocal <- function(ortho_RSDS, out_RSDS, nComp = 2, in_bands = c(1,2,3),
                tileNames = NULL, overwrite = FALSE){

  tim <- .headline("PRINCIPAL COMPONENT ANALYSIS")

  ### INPUT CHECKS ----

  .check_complete_input(ortho_RSDS, tileNames)

  # Get tiles
  ts <- .get_tilescheme()

  # Get file paths
  out_files   <- .get_RSDS_tilepaths(out_RSDS)
  ortho_files <- .get_RSDS_tilepaths(ortho_RSDS)

  ### PROCESS ----

  worker <- function(tileName){

    in_file  <- ortho_files[tileName]
    out_file <- out_files[tileName]

    # NOTE: Don't include the fourth band (the alpha band) or it'll mess things up
    in_ras <- raster::brick(in_file)[[in_bands]]

    # Generate PCA
    PCAras <- try(RStoolbox::rasterPCA(in_ras, nComp = nComp, spca = TRUE), silent = T)

    # Manage errors
    if(class(PCAras) == "try-error"){

      if(attr(PCAras,"condition") == "cannot use 'cor = TRUE' with a constant variable"){

        # Return dummy raster
        PCAras <- list(map = in_ras)
        bandNames <- names(PCAras$map)[1:2]
        PCAras$map <- setValues(PCAras$map[[c(1,2)]],0)
        PCAras$map <- setNames(PCAras$map, bandNames)
      }else{
        stop(PCAras, call. = FALSE)
      }
    }

    # Write PCA
    raster::writeRaster(PCAras$map, out_file, overwrite = overwrite)

    if(file.exists(out_file)){
      return("Success")
    }else{
      stop("Failed to create tile")
    }
  }

  ### APPLY WORKER ----

  # Get tiles for processing
  procTiles <- .processing_tiles(out_files, overwrite, tileNames)

  # Process
  status <- .doitlive(procTiles, worker)

  # Report
  .statusReport(status)

  # Conclude
  .conclusion(tim)
}




#' Principal Component Analysis - global
#'
#' Second approach: create a SINGLE PCA (using `PCAmodel`) and then apply it to each tile.
#' This looks better from an ensemble view, but may not accentuate variation as much as the
#' other approach.
#'
#' @export

PCAglobal <- function(ortho_RSDS, out_RSDS, PCA_model, nComp = 2, in_bands = c(1,2,3),
                tileNames = NULL, overwrite = FALSE){

  tim <- .headline("PRINCIPAL COMPONENT ANALYSIS")

  ### INPUT CHECKS ----

  .check_complete_input(ortho_RSDS, tileNames)

  # Get tiles
  ts <- .get_tilescheme()

  # Get file paths
  out_files   <- .get_RSDS_tilepaths(out_RSDS)
  ortho_files <- .get_RSDS_tilepaths(ortho_RSDS)

  # Read model
  model <- readRDS(PCA_model)

  ### PROCESS ----

  worker <- function(tileName){

    in_file  <- ortho_files[tileName]
    out_file <- out_files[tileName]

    in_ras <- raster::brick(in_file)[[in_bands]]
    names(in_ras) <- in_bands

    # Generate PCA
    raster::predict(in_ras, model, na.rm = TRUE, index=1:nComp, filename = out_file, overwrite = overwrite)

    if(file.exists(out_file)){
      return("Success")
    }else{
      stop("Failed to create tile")
    }
  }

  ### APPLY WORKER ----

  # Get tiles for processing
  procTiles <- .processing_tiles(out_files, overwrite, tileNames)

  # Process
  status <- .doitlive(procTiles, worker)

  # Report
  .statusReport(status)

  # Conclude
  .conclusion(tim)
}


#' Principal Component Analysis model
#'
#' Create a single PCA model for the entire area
#'
#' @export

PCAmodel <- function(ortho_RSDS, out_file, nSamples = NULL, in_bands = c(1,2,3), removeBlack = T, spca = FALSE){

  tim <- .headline("PCA MODEL")

  # Get tiles
  ts <- .get_tilescheme()

  if(is.null(nSamples)) nSamples <- length(ts) * 1100

  # Get paths
  in_paths <- .get_RSDS_tilepaths(ortho_RSDS)

  # Create sample points
  samples <- suppressWarnings(sp::spsample(ts[["tiles"]], nSamples , "random"))
  samples_df <- sp::SpatialPointsDataFrame(samples, data.frame(tileName = rep(NA, length(samples))))
  samples_df[["tileName"]] <- sp::over(samples_df, ts[["tiles"]])[["tileName"]]
  unique_tiles <- unique(samples_df[["tileName"]])

  cat(
    "  Sample pts        : ", nrow(samples_df), "\n",
    "  Tiles             : ", length(unique_tiles), "\n",
    sep = ""
  )

  # Create progress bar
  pb <- .progbar(length(unique_tiles))

  # Read training data
  samples_vals <- lapply(unique_tiles, function(tile_name){

    ras_path <- in_paths[tile_name]

    # Read segments
    ras <- raster::brick(ras_path)[[in_bands]]
    names(ras) <- in_bands

    # Subset of sample points
    samples_sub <- samples_df[samples_df[["tileName"]] == tile_name,]

    # Extract values
    samples_val <- raster::extract(ras, samples_sub)

    pb$tick()

    return(samples_val)
  })
  samples_vals <- do.call(rbind,samples_vals)

  # Remove black points
  if(removeBlack){

    samples_vals <- samples_vals[!apply(samples_vals, 1, function(x) all(x==0)),]

    cat("  Removing black px : ", nrow(samples_df) - nrow(samples_vals), "\n", sep = "")
  }

  cat("  Creating model",  "\n", sep = "")
  model <- princomp(samples_vals, scores = FALSE, cor = spca)

  # Save classifier
  saveRDS(model, out_file)

  # Conclude
  .conclusion(tim)
}
