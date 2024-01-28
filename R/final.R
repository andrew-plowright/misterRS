#' Final Canopy
#'
#' Applies the following process:
#' \enumerate{
#' \item Clip to boundary
#' \item Remove any 'NotTree' classes from canopy
#' \item Merge into final TIF file
#' }
#'
#' @export

final_canopy <- function(trees_class_rts, canopyClasses, boundary, out_file,
                        boundary_buff = 0){

  .env_misterRS(list(tile_names = NULL, overwrite = TRUE))

  process_timer <- .headline("FINAL CANOPY")

  ### INPUT CHECKS ----

  # Check that inputs are complete
  .check_complete_input(trees_class_rts)

  # Get tile scheme
  ts <- .get_tilescheme()

  # Create temporary directories
  dirs <- list(
    temproot    = file.path(tempdir(), "final_canopy"),
    boundary_tif = file.path(tempdir(), "final_canopy", "boundary_tif"),
    canopy      = file.path(tempdir(), "final_canopy", "canopy")
  )
  for(dir in dirs) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(dirs$temproot, recursive = TRUE))

  # Get file paths
  trees_class_ras_paths <- .rts_tile_paths(trees_class_rts)
  tile_names  <- names(trees_class_ras_paths)
  out_paths   <- setNames(file.path(dirs$canopy, paste0(tile_names, ".tif")),tile_names)
  boundary_mask_path <- file.path(dirs$temproot, "boundary_mask.shp")

  ### BOUNDARY ----

    # Read boundary
    boundary_sp <- sf::st_read(boundary, quiet = TRUE)

    # Buffer
    if(boundary_buff > 0) boundary_sp <- sf::st_buffer(boundary_sp, boundary_buff)

    # Give boundary a masking value
    boundary_sp[["Mask"]] <- 1

    # Save
    sf::st_write(boundary_sp[,c("Mask")], boundary_mask_path, quiet = TRUE)

  ### CREATE WORKER ----

  # Run process
  tile_worker <-function(tile_name){

    # File paths
    trees_class_ras_path <- trees_class_ras_paths[tile_name]
    out_path          <- out_paths[tile_name]
    boundary_tif_path  <- file.path(dirs$boundary_tif, paste0(tile_name, ".tif"))

    # Read seg class raster
    trees_class_ras <- terra::rast(trees_class_ras_path)

    # Rasterize asset outline
    gpal2::gdal_rasterize(
      a = "Mask",
      a_nodata = 0,
      co = c("COMPRESS=LZW"),
      te = terra::ext(trees_class_ras),
      tr = terra::res(trees_class_ras),
      ot = "UInt16",
      boundary_mask_path,
      boundary_tif_path
    )

    # Read boundary mask tile
    boundary_mask <- terra::rast(boundary_tif_path)

    # Apply boundary
    trees_class_ras[is.na(boundary_mask)] <- NA

    # Remove 'NotTree' classes
    attTable <- terra::cats(trees_class_ras)[[1]]
    canopyClassesNum <- attTable[match(canopyClasses, as.character(attTable[,2])), 1]
    canopyClass <- terra::match(trees_class_ras, canopyClassesNum)

    canopyClass[!is.na(canopyClass)] <- 1

    # Write file
    terra::writeRaster(canopyClass, out_path, datatype = "INT1U")

    if(file.exists(out_path)) "Success" else stop("Failed to create output")

  }

  ### APPLY WORKER ----

    # Get tiles for processing
    queued_tiles <- .tile_queue(out_paths)

    # Process
    process_status <- .exe_tile_worker(queued_tiles, tile_worker)

    # Report
    .print_process_status(process_status)

  ### MERGE ----

    tempVRT <- .mosaic_vrt(out_paths, ts, overlap = "buffs" )

    gpal2::gdal_translate(
      co = c("BIGTIFF=YES", "COMPRESS=LZW"),
      tempVRT,
      R.utils::getAbsolutePath(out_file)
    )

    gpal2::gdaladdo(
      r = "average",
      ro = TRUE,
      R.utils::getAbsolutePath(out_file)
    )

    # Conclude
    .conclusion(process_timer)

}


#' @export

final_chm <- function(ndsm_rts, trees_class_rts, canopyClasses, boundary, out_file,
                     boundary_buff = 0){

  process_timer <- .headline("FINAL CANOPY HEIGHT MODEL")

  .env_misterRS(list(tile_names = NULL, overwrite = TRUE))

  ### INPUT CHECKS ----

  # Check that inputs are complete
  .check_complete_input(trees_class_rts)

  # Get tile scheme
  ts <- .get_tilescheme()

  # Create temporary directories
  dirs <- list(
    temproot     = file.path(tempdir(), "final_chm"),
    boundary_tif = file.path(tempdir(), "final_chm", "boundary_tif"),
    CHM          = file.path(tempdir(), "final_chm", "chm")
  )
  for(dir in dirs) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(dirs$temproot, recursive = TRUE))

  # Get file paths
  trees_class_ras_paths <- .rts_tile_paths(trees_class_rts)
  ndsm_paths <- .rts_tile_paths(ndsm_rts)
  tile_names <- names(trees_class_ras_paths)
  out_paths  <- setNames(file.path(dirs$CHM, paste0(tile_names, ".tif")),tile_names)
  boundary_mask_path <- file.path(dirs$temproot, "boundary_mask.shp")

  ### BOUNDARY ----

  # Read boundary
  boundary_sp <- sf::st_read(boundary, quiet = TRUE)

  # Give boundary a masking value
  boundary_sp[["Mask"]] <- 1

  # Buffer
  if(boundary_buff > 0) boundary_sp <- sf::st_buffer(boundary_sp, boundary_buff)

  # Save
  sf::st_write(boundary_sp, boundary_mask_path, quiet = TRUE)

  ### CREATE WORKER ----

  # Run process
  tile_worker <-function(tile_name){

    # File paths
    trees_class_ras_path <- trees_class_ras_paths[tile_name]
    out_path             <- out_paths[tile_name]
    ndsm_path            <- ndsm_paths[tile_name]
    boundary_tif_path    <- file.path(dirs$boundary_tif, paste0(tile_name, ".tif"))

    # Read seg class raster and nDSM
    trees_class_ras <- terra::rast(trees_class_ras_path)
    nDSM         <- terra::rast(ndsm_path)

    # Rasterize asset outline
    gpal2::gdal_rasterize(
      a = "Mask",
      a_nodata = 0,
      co = c("COMPRESS=LZW"),
      te = terra::ext(trees_class_ras),
      tr = terra::res(trees_class_ras),
      ot = "UInt16",
      boundary_mask_path,
      boundary_tif_path
    )

    # Read boundary mask tile
    boundary_mask <- terra::rast(boundary_tif_path)

    # Apply boundary
    nDSM[is.na(boundary_mask)] <- NA

    # Remove 'NotTree' classes
    attTable <- terra::cats(trees_class_ras)[[1]]
    canopyClassesNum <- attTable[match(canopyClasses, as.character(attTable[,2])), 1]
    canopyClass <- terra::match(trees_class_ras, canopyClassesNum)

    # Mask DSM
    nDSM[is.na(canopyClass)] <- NA

    # Write file
    terra::writeRaster(nDSM, out_path)

    if(file.exists(out_path)) "Success" else stop("Failed to create output")

  }

  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_paths)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  ### MERGE ----

  tempVRT <- .mosaic_vrt(out_paths, ts, overlap = "nbuffs" )

  gpal2::gdal_translate(
    co = c("BIGTIFF=YES", "COMPRESS=LZW"),
    tempVRT,
    R.utils::getAbsolutePath(out_file)
  )

  gpal2::gdaladdo(
    r = "average",
    ro = TRUE,
    R.utils::getAbsolutePath(out_file)
  )

  # Conclude
  .conclusion(process_timer)

}


#' @export

final_trees <- function(trees_class_poly_vts, reclassList, boundary, out_file, tile_names = NULL){

  process_timer <- .headline("FINAL TREES")

  .env_misterRS(list(tile_names = NULL, overwrite = TRUE))

  ### INPUT CHECKS ----

  # Check that inputs are complete
  .check_complete_input(trees_class_poly_vts)

  # Create temporary directories
  dirs <- list(
    temproot    = file.path(tempdir(), "final_trees")
  )
  for(dir in dirs) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(dirs$temproot, recursive = TRUE))

  # Get file paths
  trees_class_poly_paths <- .rts_tile_paths(trees_class_poly_vts)
  outTrees_gpkg <- file.path(dirs$temproot, "outTrees.gpkg")

  # Select tiles
  selected_tiles <- if(is.null(tile_names)){

    names(trees_class_poly_paths)

  }else{

    notExist <- !tile_names %in% names(trees_class_poly_paths)
    if(any(notExist)) stop("Following tile names do not exist:\n  ", paste(tile_names[notExist], collapse = "\n  "))

    tile_names
  }

  # Initiate ID counter
  IDcounter <- 0

  ### BOUNDARY ----

  # Read boundary
  boundary_sp <- sf::st_read(boundary, quiet = TRUE)
  boundary_sp$geom <- suppressPackageStartupMessages(lwgeom::lwgeom_make_valid(boundary_sp$geom))
  boundary_sp <- sf::st_transform(boundary_sp,   getOption("misterRS.crs"))

  ### CREATE WORKER ----

  for(tile_name in selected_tiles){

    # File paths
    trees_class_poly_path <- trees_class_poly_paths[tile_name]

    # Read in polygons
    trees_class_poly <- sf::st_read(trees_class_poly_path, quiet = TRUE)

    # Correct geometry
    is_valid <- sf::st_is_valid(trees_class_poly)
    if(any(!is_valid)){
      trees_class_poly <- sf::st_make_valid(trees_class_poly)
    }

    # Intersection with boundary
    ints <- sf::st_intersects(boundary_sp, trees_class_poly)
    trees_class_poly <- trees_class_poly[unique(unlist(ints)),]

    # Remove non trees
    if("NotTree" %in% names(reclassList)){

      trees_class_poly <- trees_class_poly[!trees_class_poly[["segClass"]] %in% reclassList[["NotTree"]],]

    }

    # Remove non-classified
    trees_class_poly <- trees_class_poly[!is.na(trees_class_poly[["segClass"]]),]

    if(nrow(trees_class_poly) > 0){

      # Reclassify trees
      treeClasses <- reclassList[names(reclassList) != "NotTree"]
      for(class in names(treeClasses)){
        trees_class_poly[trees_class_poly[["segClass"]] %in% treeClasses[[class]], "segClass"] <- class
      }

      # Add tree ID
      newIDs <- IDcounter + 1:nrow(trees_class_poly)
      IDcounter <- max(newIDs)
      trees_class_poly[["ID"]] <- newIDs
      #trees_class_poly[["FID"]] <- newIDs

      # Remove and rename attribute columns
      trees_class_poly <- trees_class_poly[,c("ID", "height", "crownArea", "segClass")]
      names(trees_class_poly)[names(trees_class_poly) == "segClass"] <- "class"

      # Write output
      sf::st_write(
        trees_class_poly,
        outTrees_gpkg,
        append = file.exists(  outTrees_gpkg ),
        quiet = TRUE,
        fid_column_name = "FID"
      )
    }
  }

  ### MERGE ----

  gpal2::ogr2ogr(
    "-overwrite",
    out_file,
    outTrees_gpkg
  )

  # Conclude
  .conclusion(process_timer)
}
