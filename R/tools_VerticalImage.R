#' Vertical image
#'
#' Composite image of two ortho PCA bands and a normalized digital surface model
#'
#' @export

VerticalImage <- function(PCA_RSDS, nDSM_RSDS, out_RSDS, nDSM_range, PCA_range = NULL,
                          tileNames = NULL, overwrite = FALSE){

  tim <- .headline("VERTICAL IMAGE")

  ### INPUT CHECKS ----

  .check_complete_input(PCA_RSDS,  tileNames)
  .check_complete_input(nDSM_RSDS, tileNames)

  .check_extension(PCA_RSDS,  "tif")
  .check_extension(nDSM_RSDS, "tif")
  .check_extension(out_RSDS,  "tif")

  # Get tiles
  ts <- .get_tilescheme()

  # Get file paths
  out_files  <- .get_RSDS_tilepaths(out_RSDS)
  nDSM_files <- .get_RSDS_tilepaths(nDSM_RSDS)
  PCA_files  <- .get_RSDS_tilepaths(PCA_RSDS)

  ### GET PCA FILES INFO ----


  PCA_stat_files <- if(is.null(tileNames)) PCA_files else PCA_files[tileNames]

  PCA_stats <- lapply(PCA_stat_files, function(f){
    attr(rgdal::GDALinfo(f), "df")[, c("Bmin", "Bmax")]
  })

  PCA_bands <- unique(sapply(PCA_stats, nrow))
  if(length(PCA_bands) > 1) stop("Inconsistent number of PCA bands", call. = FALSE)

  # If no PCA range has been provided, get the average min/max value per bands
  if(is.null(PCA_range)){

    PCA_range <- lapply(1:PCA_bands, function(b) apply(do.call(rbind, lapply(PCA_stats, function(f) f[b,] )), 2, mean))

  }else{

    if(class(PCA_range) == "numeric"){

      if(length(PCA_range) != 2) stop("Input 'PCA_range' should have 2 values: a minimum and a maximum")
      PCA_range <- list(PCA_range)

    }else if(class(PCA_range)  != "list") stop("Input 'PCA_range' should be a list", call. = FALSE)

    if(length(PCA_range) == 1) PCA_range <- rep(PCA_range, PCA_bands)

    if(length(PCA_range) != PCA_bands) stop("Input 'PCA_range' should be a list with ranges for ", PCA_bands, " PCA bands", call. = FALSE)
  }

  # Print ranges
  cat("  nDSM range       : ", nDSM_range[1], " to ", nDSM_range[2], "\n", sep = "")
  for(rn in 1:length(PCA_range)){
    cat("  PCA ", rn, " range      : ",  PCA_range[[rn]][1], " to ",  PCA_range[[rn]][2], "\n", sep = "")
  }


  ### CREATE WORKER ----

  worker <- function(tileName){

    PCA_file  <- PCA_files[tileName]
    nDSM_file <- nDSM_files[tileName]
    out_file  <- out_files[tileName]

    # Read raster
    PCA  <- raster::brick(PCA_file)
    nDSM <- raster::raster(nDSM_file)

    # Remove empty values and rescale nDSM
    nDSM[is.na(nDSM)] <- 0
    nDSM_resc <- .rescale(nDSM, from = nDSM_range, to = c(0,254))

    # Rescale PCA
    PCA_resc <- lapply(1:PCA_bands, function(b){
      .rescale(PCA[[b]], from = PCA_range[[b]], to = c(0,254))
    })


    # Combine and save
    combined <- do.call(raster::stack, c(PCA_resc, list(nDSM_resc)))
    raster::writeRaster(combined, filename = out_file, datatype = "INT1U", overwrite = overwrite)

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
