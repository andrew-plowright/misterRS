#' Principal Component Analysis - local
#'
#' This is the first approach I tried: calculate a different PCA for each tile.
#' This means that the tiles don't necessarily match very well (edge effects are present),
#' but it does maximize local variation.
#'
#'
#' @export

PCAlocal <- function(ortho_RSDS, out_RSDS, nComp = 2, in_bands = c(1,2,3),
                tileNames = NULL, overwrite = FALSE, spca = TRUE){

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
    # NOTE: 'maskCheck = FALSE' saves processing time but assumes that there's no NA values
    PCAras <- try(RStoolbox::rasterPCA(in_ras, nComp = nComp, spca = spca, maskCheck = FALSE), silent = T)

    # Manage errors
    if("try-error" %in% class(PCAras) ){

      if(attr(PCAras,"condition")$message == "cannot use 'cor = TRUE' with a constant variable"){

        # Return dummy raster
        PCAras <- list(map = in_ras)
        bandNames <- names(PCAras$map)[1:2]
        PCAras$map <- raster::setValues(PCAras$map[[c(1,2)]],0)
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

    in_ras <- terra::rast(in_file, lyrs = in_bands)
    names(in_ras) <- in_bands

    # Generate PCA
    out_pca <- terra::predict(in_ras, model)[[1:nComp]]

    # Write output
    terra::writeRaster(out_pca,   filename = out_file, overwrite = overwrite)


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

  # Get paths
  in_paths <- .get_RSDS_tilepaths(ortho_RSDS)

  # Get tiles
  ts <- .get_tilescheme()
  tiles_sf <- sf::st_as_sf(ts[["tiles"]])

  # Default number of samples if it's not specified
  if(is.null(nSamples)) nSamples <- length(ts) * 1100

  # Create sample points
  samples <- sf::st_as_sf(sf::st_sample(tiles_sf, size = nSamples))

  # Assign each sample its tile
  samples[["tileName"]] <- ts[["tiles"]]$tileName[ sapply(sf::st_intersects(samples, tiles_sf), "[[", 1) ]


  # Get unique tiles
  unique_tiles <- unique(samples[["tileName"]])

  cat(
    "  Sample pts        : ", length(samples), "\n",
    "  Tiles             : ", length(unique_tiles), "\n",
    sep = ""
  )

  # Create progress bar
  pb <- .progbar(length(unique_tiles))

  # Read training data
  samples_vals <- lapply(unique_tiles, function(tile_name){


    ras_path <- in_paths[tile_name]

    # Read ortho tile
    ras <- terra::rast(ras_path, lyrs = in_bands )
    names(ras) <- in_bands

    # Subset of sample points
    samples_sub <- samples[samples$tileName == tile_name,]

    # Extract values
    samples_val <- terra::extract(ras, sf::st_coordinates(samples_sub))

    pb$tick()

    return(samples_val)
  })
  samples_vals <- do.call(rbind,samples_vals)

  # Remove NAs
  remove_nas <- apply(samples_vals, 1, function(x) any(is.na(x)))
  samples_vals <- samples_vals[!remove_nas,]
  cat("  Removing NA px   : ", length(remove_nas[remove_nas]), "\n", sep = "")

  # Remove black points
  if(removeBlack){

    remove_blacks <- !apply(samples_vals, 1, function(x) all(x==0))
    samples_vals <- samples_vals[remove_blacks,]
    cat("  Removing black px : ", length(remove_blacks[!remove_blacks]), "\n", sep = "")

  }

  cat("  Creating model",  "\n", sep = "")
  model <- princomp(samples_vals, scores = FALSE, cor = spca)

  # Save classifier
  saveRDS(model, out_file)

  # Conclude
  .conclusion(tim)
}
