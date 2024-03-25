#' Principal Component Analysis - local
#'
#' This is the first approach I tried: calculate a different PCA for each tile.
#' This means that the tiles don't necessarily match very well (edge effects are present),
#' but it does maximize local variation.
#'
#' @export

pca_local <- function(img_rts, out_rts, n_comp = 2, in_bands = c(1,2,3),  ...){

  .env_misterRS(list(...))

  process_timer <- .headline("PRINCIPAL COMPONENT ANALYSIS")

  ### INPUT CHECKS ----

  .complete_input(img_rts)

  # Get tiles
  ts <- .tilescheme()


  ### PROCESS ----

  tile_worker <-function(tile_name){

    in_file  <- img_rts$tile_path(tile_name)
    out_file <- out_rts$tile_path(tile_name)

    # NOTE: Don't include the fourth band (the alpha band) or it'll mess things up
    in_ras <- terra::rast(in_file)[[in_bands]]

    # Calculate PCA model
    pca <- prcomp(in_ras)

    # Apply model to raster
    pca_ras <- terra::predict(in_ras, pca, index = 1:n_comp)

    # Write PCA
    terra::writeRaster(pca_ras, out_file, overwrite = TRUE)

    if(file.exists(out_file)){
      return("Success")
    }else{
      stop("Failed to create tile")
    }
  }

  ### APPLY WORKER ----

  out_rts %>%
    .tile_queue() %>%
    .exe_tile_worker(tile_worker) %>%
    .print_process_status()

  # Conclude
  .conclusion(process_timer)
}




#' Principal Component Analysis (Global)
#'
#' Second approach: create a SINGLE PCA (using \code{pca_model()}) and then apply it to each tile.
#' This looks better from an ensemble view, but may not accentuate variation as much as the
#' other approach.
#'
#' @export

pca_global <- function(img_rts, out_rts, pca_model,
                       n_comp = 2, in_bands = c(1,2,3), ...){

  .env_misterRS(list(...))

  process_timer <- .headline("PRINCIPAL COMPONENT ANALYSIS")

  ### INPUT CHECKS ----

  .complete_input(img_rts)

  # Get tiles
  ts <- .tilescheme()

  # Read model
  model <- readRDS(pca_model)

  ### PROCESS ----

  tile_worker <-function(tile_name){

    in_file  <- img_rts$tile_path(tile_name)
    out_file <- out_rts$tile_path(tile_name)

    in_ras <- terra::rast(in_file, lyrs = in_bands)
    names(in_ras) <- in_bands

    # Generate PCA
    out_pca <- terra::predict(in_ras, model, index = 1:n_comp)

    # Write output
    terra::writeRaster(out_pca, filename = out_file, overwrite = TRUE)

    if(file.exists(out_file)) "Success" else stop("Failed to create tile")
  }

  ### APPLY WORKER ----

  out_rts %>%
    .tile_queue() %>%
    .exe_tile_worker(tile_worker) %>%
    .print_process_status()


  # Conclude
  .conclusion(process_timer)
}


#' Principal Component Analysis model
#'
#' Create a single PCA model for the entire area. Used in conjunction with \code{pca_global}
#'
#' @export

pca_model <- function(img_rts, out_file, n_samples = NULL, in_bands = c(1,2,3), remove_black = TRUE){

  process_timer <- .headline("PCA MODEL")

  # Get tiles
  ts <- .tilescheme()

  # Default number of samples if it's not specified
  if(is.null(n_samples)) n_samples <- length(ts) * 1100

  # Create sample points
  samples <- sf::st_sample(ts[["tiles"]], size = n_samples)

  # Assign each sample its tile
  sample_intersec <- sapply(sf::st_intersects(samples, ts[["tiles"]]), "[[", 1)
  samples <- sf::st_sf(samples, tile_name = ts[["tile_name"]][sample_intersec])

  # Get unique tiles
  unique_tiles <- unique(samples[["tile_name"]])

  cat(
    "  Sample pts        : ", nrow(samples), "\n",
    "  Tiles             : ", length(unique_tiles), "\n",
    sep = ""
  )

  # Create progress bar
  pb <- .progbar(length(unique_tiles))

  # Read training data
  samples_vals <- do.call(rbind, lapply(unique_tiles, function(tile_name){

    ras_path <- img_rts$tile_path(tile_name)

    # Read ortho tile
    ras <- terra::rast(ras_path, lyrs = in_bands)
    names(ras) <- in_bands

    # Subset of sample points
    samples_sub <- samples[samples$tile_name == tile_name,]

    # Extract values
    samples_val <- terra::extract(ras, sf::st_coordinates(samples_sub))

    pb$tick()

    return(samples_val)
  }))

  # Remove NAs
  remove_nas <- apply(samples_vals, 1, function(x) any(is.na(x)))
  samples_vals <- samples_vals[!remove_nas,]
  cat("  Removing NA px    : ", length(remove_nas[remove_nas]), "\n", sep = "")

  # Remove black points
  if(remove_black){

    removed_blacks <- !apply(samples_vals, 1, function(x) all(x==0))
    samples_vals <- samples_vals[removed_blacks,]
    cat("  Removing black px : ", length(removed_blacks[!removed_blacks]), "\n", sep = "")
  }

  # Create model
  model <- prcomp(samples_vals)

  # Save classifier
  saveRDS(model, out_file)

  # Conclude
  .conclusion(process_timer)
}
