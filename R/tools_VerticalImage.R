#' Vertical image
#'
#' Composite image of two ortho PCA bands and a normalized digital surface model
#'
#' @export

VerticalImage <- function(inputs, out_RSDS,  tileNames = NULL, overwrite = FALSE){

  tim <- .headline("VERTICAL IMAGE")

  ### INPUT CHECKS ----

  for(input in inputs){
    .check_complete_input(input$rsds,  tileNames)
    .check_extension(input$rsds,  "tif")
  }
  .check_extension(out_RSDS,  "tif")

  # Get tiles
  ts <- .get_tilescheme()

  # Get file paths
  out_files  <- .get_RSDS_tilepaths(out_RSDS)
  in_files <- lapply(inputs, function(input){.get_RSDS_tilepaths(input$rsds)})

  # ### GET PCA FILES INFO ----
  #
  # PCA_stat_files <- if(is.null(tileNames)) PCA_files else PCA_files[tileNames]
  #
  # PCA_stats <- lapply(PCA_stat_files, function(f){
  #   attr(rgdal::GDALinfo(f), "df")[, c("Bmin", "Bmax")]
  # })
  #
  # PCA_bands <- unique(sapply(PCA_stats, nrow))
  # if(length(PCA_bands) > 1) stop("Inconsistent number of PCA bands", call. = FALSE)
  #
  # # If no PCA range has been provided, get the average min/max value per bands
  # if(is.null(PCA_range)){
  #
  #   PCA_range <- lapply(1:PCA_bands, function(b) apply(do.call(rbind, lapply(PCA_stats, function(f) f[b,] )), 2, mean))
  #
  # }else{
  #
  #   if(class(PCA_range) == "numeric"){
  #
  #     if(length(PCA_range) != 2) stop("Input 'PCA_range' should have 2 values: a minimum and a maximum")
  #     PCA_range <- list(PCA_range)
  #
  #   }else if(class(PCA_range)  != "list") stop("Input 'PCA_range' should be a list", call. = FALSE)
  #
  #   if(length(PCA_range) == 1) PCA_range <- rep(PCA_range, PCA_bands)
  #
  #   if(length(PCA_range) != PCA_bands) stop("Input 'PCA_range' should be a list with ranges for ", PCA_bands, " PCA bands", call. = FALSE)
  # }
  #
  # # Print ranges
  # cat("  nDSM range       : ", nDSM_range[1], " to ", nDSM_range[2], "\n", sep = "")
  # for(rn in 1:length(PCA_range)){
  #   cat("  PCA ", rn, " range      : ",  PCA_range[[rn]][1], " to ",  PCA_range[[rn]][2], "\n", sep = "")
  # }


  ### CREATE WORKER ----

  worker <- function(tileName){

    # Output file
    out_file  <- out_files[tileName]

    # Create raster stack
    ras_stack <- do.call(raster::stack, lapply(1:length(inputs), function(i){

      input <- inputs[[i]]
      in_file <- in_files[[i]][tileName]
      in_ras <- raster::raster(in_file, band = input$band)

      in_ras[is.na(in_ras)] <- 0

      return(  .rescale(in_ras, from = input$range, to = c(0,254)) )

    }))

    # Write output
    raster::writeRaster(ras_stack, filename = out_file, datatype = "INT1U", overwrite = overwrite)

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
