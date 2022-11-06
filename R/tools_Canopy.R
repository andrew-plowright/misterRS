#' Create canopy mask
#'
#' @param canopyClasses names of raster classes that are included in the canopy
#' @param openings the number of times that a morphological opening will be applied
#' @param openingRadius radius of morphological opening window
#'
#' @export

CanopyMask <- function(segClassRas_RSDS, canopyMask_RSDS, canopyClasses, canopyEdits = NULL, openings = 1, openingRadius = 0.5,
                           tileNames = NULL, overwrite = FALSE){


  tim <- .headline("CANOPY MASK")

  ### INPUT CHECKS ----

  # Check extensions
  .check_extension(segClassRas_RSDS,  "tif")
  .check_extension(canopyMask_RSDS,   "tif")

  # Check that inputs are complete
  .check_complete_input(segClassRas_RSDS, tileNames)

  # Get file paths
  seg_class_ras_paths <- .get_RSDS_tilepaths(segClassRas_RSDS)
  out_paths         <- .get_RSDS_tilepaths(canopyMask_RSDS)

  ts <- .get_tilescheme()

  if(!is.numeric(openings) || openings < 0) stop("Invalid input for 'openings':", openings)

  if(!is.null(canopyEdits)){

    # Temporary folder
    edit_folder <- file.path(tempdir(), "canopyEdits")
    dir.create(edit_folder, showWarnings = FALSE)
    on.exit(unlink(edit_folder, recursive = TRUE))

    # Read canopy edits
    edits <- sf::st_read(canopyEdits, quiet = T)

    if(!all(edits$canopy %in% c(1,0))) stop("All values in the 'canopy' column should be either 1 or 0")
  }



  ### CREATE WORKER ----

  # Run process
  worker <- function(tileName){

    # Get paths
    out_path <- out_paths[tileName]

    buff <- sf::st_as_sf(ts[tileName][["buffs"]])

    # Get neighbours
    neib_names <- .tileNeighbours(ts, tileName)$tileName
    seg_class_ras_neibs <- lapply(seg_class_ras_paths[neib_names], terra::rast)

    # Check if raster is classified
    ras_classes <- terra::cats(seg_class_ras_neibs[[tileName]])[[1]]
    if(is.null(ras_classes)) stop("'segClassRas_RSDS' was an unclassified input")

    # Merge and then crop
    seg_class_ras <- seg_class_ras_neibs %>%
      terra::sprc() %>%
      terra::merge() %>%
      terra::crop(terra::ext(buff))

    # Create matrix for converting clsses into binary canopy mask
    convert_matrix <- matrix(c(ras_classes$value, as.numeric(ras_classes$category %in% canopyClasses)),ncol = 2)

    # Reclassify raster
    canopy_ras <- terra::classify(seg_class_ras, convert_matrix)

    if(openings > 0){

      # Get rid of NA values (otherwise the "closing" part of the operation below will malfunction along the edges)
      canopy_ras[is.na(canopy_ras)] <- 0

      # Make matrix
      mat <-  terra::focalMat(canopy_ras, openingRadius, fillNA = TRUE)
      if(!dim(mat)[1] > 1) stop("'openingRadius' is too small", call. = FALSE)
      mat[!is.na(mat)] <- 1

      while(openings > 0){

        # Apply morphological opening
        canopy_ras <- canopy_ras %>%
            terra::focal(mat, min, na.rm = TRUE, pad = TRUE) %>% # Erode
            terra::focal(mat, max, na.rm = TRUE, pad = TRUE)     # Dilate

        openings <- openings - 1
      }
    }

    if(!is.null(canopyEdits)){

      edit_file <- file.path(edit_folder, paste0(tileName, ".tif"))

      gpal2::gdal_rasterize(
        a = 'canopy',
        a_nodata = -1,
        co = c("COMPRESS=LZW"),
        te = terra::ext(canopy_ras),
        tr = terra::res(canopy_ras),
        ot = "Int16",
        R.utils::getAbsolutePath(canopyEdits),
        edit_file
      )

      editRas <- terra::rast(edit_file)

      canopy_ras <- terra::cover(editRas, canopy_ras)
    }

    # Remove 0 values
    canopy_ras[canopy_ras == 0] <- NA

    # Save output
    raster::writeRaster( canopy_ras, out_path, overwrite = overwrite, datatype = "INT1U")

    if(file.exists(out_path)) "Success" else stop("Failed to create output")

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
