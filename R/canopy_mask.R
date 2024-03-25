#' Create canopy mask
#'
#' @param canopy_classes integer. Raster values for `seg_class_rts` that correspond to canopy
#' @param openings the number of times that a morphological opening will be applied
#' @param opening_radius radius of morphological opening window
#'
#' @export

canopy_mask <- function(seg_class_rts, out_rts, canopy_classes,
                        canopy_edits = NULL, openings = 1, opening_radius = 0.5, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("CANOPY MASK")

  ### INPUT CHECKS ----

  # Check that inputs are complete
  .complete_input(seg_class_rts, buffered = TRUE)

  ts <- .tilescheme()

  if(!is.numeric(openings) || openings < 0) stop("Invalid input for 'openings':", openings)

  if(!is.null(canopy_edits)){

    # Temporary folder
    edit_folder <- file.path(tempdir(), "canopy_edits")
    dir.create(edit_folder, showWarnings = FALSE)
    withr::defer(unlink(edit_folder, recursive = TRUE))

    # Read canopy edits
    edits <- sf::st_read(canopy_edits@file_path, quiet = T)

    if(!all(edits$canopy %in% c(1,0))) stop("All values in the 'canopy' column should be either 1 or 0")
  }

  ### CREATE WORKER ----

  # Run process
  tile_worker <-function(tile_name){

    # Get paths
    out_path <- out_rts$tile_path(tile_name)

    # Buffered tile
    buff <- ts[tile_name][["buffs"]]

    # Get neighbours
    neib_names <- .tile_neibs(tile_name, ts)
    seg_class_ras_neibs <- lapply(neib_names, function(tile_name) terra::rast(seg_class_rts$tile_path(tile_name)))
    seg_class_ras <- seg_class_ras_neibs %>%
      terra::sprc() %>%
      terra::merge() %>%
      terra::crop(terra::ext(buff))

    # Create matrix for converting clsses into binary canopy mask
    convert_matrix <- cbind(canopy_classes, 1)

    # Reclassify raster
    canopy_ras <- terra::classify(seg_class_ras, convert_matrix, others = 0)

    if(openings > 0){

      # Make matrix
      mat <-  terra::focalMat(canopy_ras, opening_radius, fillNA = TRUE)
      if(!dim(mat)[1] > 1) stop("'opening_radius' is too small", call. = FALSE)
      mat[!is.na(mat)] <- 1

      while(openings > 0){

        # Apply morphological opening
        canopy_ras <- canopy_ras %>%
            terra::focal(mat, min, na.rm = TRUE, pad = TRUE) %>% # Erode
            terra::focal(mat, max, na.rm = TRUE, pad = TRUE)     # Dilate

        openings <- openings - 1
      }
    }

    if(!is.null(canopy_edits)){

      edit_file <- file.path(edit_folder, paste0(tile_name, ".tif"))

      gpal2::gdal_rasterize(
        a = 'canopy',
        a_nodata = -1,
        co = c("COMPRESS=LZW"),
        te = terra::ext(canopy_ras),
        tr = terra::res(canopy_ras),
        ot = "Int16",
        R.utils::getAbsolutePath(canopy_edits@file_path),
        edit_file
      )

      editRas <- terra::rast(edit_file)

      canopy_ras <- terra::cover(editRas, canopy_ras)
    }

    # Remove 0 values
    canopy_ras[canopy_ras == 0] <- NA

    # Save output
    terra::writeRaster( canopy_ras, out_path, overwrite = TRUE, datatype = "INT1U")

    if(file.exists(out_path)) "Success" else stop("Failed to create output")

  }

  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_rts)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)

}


#' Canopy edits (class)
#' @export

setClass(
  "canopy_edits",
  representation(
    file_path  = 'character'
  )
)


setMethod("show", "canopy_edits", function(object){

  vec_num <- 0
  if(file.exists(object@file_path)){

    info <- sf::st_layers(object@file_path)
    vec_num <- info$features[1]
  }

  cat(
    "CANOPY EDITS", "\n",
    "Vectors : ", vec_num,"\n",
    sep = ""
  )
})

#' Canopy Edits (constructor)
#' @export

canopy_edits <- function(dir, proj = getOption("misterRS.crs"), overwrite = FALSE){

  if(proj == "" | is.na(proj) | is.null(proj)) stop("Invalid CRS")

  file_path <- file.path(dir, "canopy_edits.gpkg")

  if(!file.exists(file_path) | overwrite){

    # Create Simple Feature object with blank geometry and empty attribute fields
    s <- sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(proj)), list(canopy = integer()))

    # Write to geopackage
    sf::st_write(s,  file_path, quiet = TRUE, layer = "edits", delete_layer = TRUE)

    # Set geometry type
    con = DBI::dbConnect(RSQLite::SQLite(),dbname= file_path)
    withr::defer(DBI::dbDisconnect(con))
    DBI::dbExecute(con, "UPDATE gpkg_geometry_columns SET geometry_type_name = 'POLYGON' WHERE table_name = 'edits'")
  }

  # Create new object
  new("canopy_edits", file_path = file_path)
}
