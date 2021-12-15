#' 1. Clip to boundary
#' 2. Remove any 'NotTree' classes from canopy
#' 3. Merge into final TIF file
#'
#' @export

FinalCanopy <- function(treeClassRas_RSDS, canopyClasses, boundary, out_file,
                        boundary_buff = 0,
                        tileNames = NULL){

  tim <- .headline("FINAL CANOPY")

  ### INPUT CHECKS ----

  # Check that inputs are complete
  .check_complete_input(treeClassRas_RSDS, tileNames)

  # Get tile scheme
  ts <- .get_tilescheme()

  # Create temporary directories
  dirs <- list(
    temproot    = file.path(tempdir(), "FinalCanopy"),
    boundaryTIF = file.path(tempdir(), "FinalCanopy", "BoundaryTIF"),
    canopy      = file.path(tempdir(), "FinalCanopy", "Canopy")
  )
  for(dir in dirs) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(dirs$temproot, recursive = TRUE))

  # Get file paths
  treeClassRas_paths <- .get_RSDS_tilepaths(treeClassRas_RSDS)
  tileNames          <- names(treeClassRas_paths)
  canopyMask_paths   <- setNames(file.path(dirs$canopy, paste0(tileNames, ".tif")),tileNames)
  boundary_mask_path <- file.path(dirs$temproot, "boundary_mask.shp")

  ### BOUNDARY ----

    # Read boundary
    boundary_sp <- sf::st_read(boundary, quiet = TRUE)

    # Buffer
    if(boundary_buff > 0) boundary_sp <- sf::st_buffer(boundary_sp, boundary_buff)

    # Give boundary a masking value
    boundary_sp[["Mask"]] <- 1

    # Save
    sf::st_write(boundary_sp, boundary_mask_path, quiet = TRUE)

  ### CREATE WORKER ----

  # Run process
  worker <- function(tileName){

    # File paths
    treeClassRas_path <- treeClassRas_paths[tileName]
    out_path          <- canopyMask_paths[tileName]
    boundaryTIF_path  <- file.path(dirs$boundaryTIF, paste0(tileName, ".tif"))

    # Read seg class raster
    treeClassRas <- raster::raster(treeClassRas_path)

    # Rasterize asset outline
    gpal2::gdal_rasterize(
      a = "Mask",
      a_nodata = 0,
      co = c("COMPRESS=LZW"),
      te = raster::extent(treeClassRas),
      tr = raster::res(treeClassRas),
      ot = "UInt16",
      boundary_mask_path,
      boundaryTIF_path
    )

    # Read boundary mask tile
    boundary_mask <- raster::raster(boundaryTIF_path)

    # Apply boundary
    treeClassRas[is.na(boundary_mask)] <- NA

    # Remove 'NotTree' classes
    attTable <- treeClassRas@data@attributes[[1]]
    canopyClassesNum <- attTable[match(canopyClasses, as.character(attTable$category)), "ID"]
    canopyClass <- raster::match(treeClassRas, canopyClassesNum)

    canopyClass[!is.na(canopyClass)] <- 1

    # Write file
    raster::writeRaster(canopyClass, out_path, datatype = "INT1U")

    if(file.exists(out_path)) "Success" else stop("Failed to create output")

  }

  ### APPLY WORKER ----

    # Get tiles for processing
    procTiles <- .processing_tiles(canopyMask_paths, overwrite = TRUE, tileNames)

    # Process
    status <- .doitlive(procTiles, worker)

    # Report
    .statusReport(status)

  ### MERGE ----

    tempVRT <- .mosaicVRT(canopyMask_paths, ts, overlap = "nbuffs" )

    gpal2::gdal_translate(
      co = c("BIGTIFF=YES", "COMPRESS=LZW"),
      tempVRT,
      R.utils::getAbsolutePath(out_file)
    )

    gpal2::gdaladdo(
      r = "average",
      ro = TRUE,
      R.utils::getAbsolutePath(out_file),
      c(2,4,8,16,32,64)
    )

    # Conclude
    .conclusion(tim)

}


#' @export

FinalCHM <- function(nDSM_RSDS, treeClassRas_RSDS, canopyClasses, boundary, out_file,
                     boundary_buff = 0,
                     tileNames = NULL){

  tim <- .headline("FINAL CANOPY HEIGHT MODEL")

  ### INPUT CHECKS ----

  # Check that inputs are complete
  .check_complete_input(treeClassRas_RSDS, tileNames)

  # Get tile scheme
  ts <- .get_tilescheme()

  # Create temporary directories
  dirs <- list(
    temproot    = file.path(tempdir(), "FinalCHM"),
    boundaryTIF = file.path(tempdir(), "FinalCHM", "BoundaryTIF"),
    CHM         = file.path(tempdir(), "FinalCHM", "CHM")
  )
  for(dir in dirs) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(dirs$temproot, recursive = TRUE))

  # Get file paths
  treeClassRas_paths <- .get_RSDS_tilepaths(treeClassRas_RSDS)
  nDSM_paths         <- .get_RSDS_tilepaths(nDSM_RSDS)
  tileNames          <- names(treeClassRas_paths)
  CHMmask_paths      <- setNames(file.path(dirs$CHM, paste0(tileNames, ".tif")),tileNames)
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
  worker <- function(tileName){

    # File paths
    treeClassRas_path <- treeClassRas_paths[tileName]
    out_path          <- CHMmask_paths[tileName]
    nDSM_path         <- nDSM_paths[tileName]
    boundaryTIF_path  <- file.path(dirs$boundaryTIF, paste0(tileName, ".tif"))

    # Read seg class raster and nDSM
    treeClassRas <- raster::raster(treeClassRas_path)
    nDSM         <- raster::raster(nDSM_path)

    # Rasterize asset outline
    gpal2::gdal_rasterize(
      a = "Mask",
      a_nodata = 0,
      co = c("COMPRESS=LZW"),
      te = raster::extent(treeClassRas),
      tr = raster::res(treeClassRas),
      ot = "UInt16",
      boundary_mask_path,
      boundaryTIF_path
    )

    # Read boundary mask tile
    boundary_mask <- raster::raster(boundaryTIF_path)

    # Apply boundary
    nDSM[is.na(boundary_mask)] <- NA

    # Remove 'NotTree' classes
    attTable <- treeClassRas@data@attributes[[1]]
    canopyClassesNum <- attTable[match(canopyClasses, as.character(attTable$category)), "ID"]
    canopyClass <- raster::match(treeClassRas, canopyClassesNum)

    # Mask DSM
    nDSM[is.na(canopyClass)] <- NA

    # Write file
    raster::writeRaster(nDSM, out_path)

    if(file.exists(out_path)) "Success" else stop("Failed to create output")

  }

  ### APPLY WORKER ----

  # Get tiles for processing
  procTiles <- .processing_tiles(CHMmask_paths, overwrite = TRUE, tileNames)

  # Process
  status <- .doitlive(procTiles, worker)

  # Report
  .statusReport(status)

  ### MERGE ----

  tempVRT <- .mosaicVRT(CHMmask_paths, ts, overlap = "nbuffs" )

  gpal2::gdal_translate(
    co = c("BIGTIFF=YES", "COMPRESS=LZW"),
    tempVRT,
    R.utils::getAbsolutePath(out_file)
  )

  gpal2::gdaladdo(
    r = "average",
    ro = TRUE,
    R.utils::getAbsolutePath(out_file),
    c(2,4,8,16,32,64)
  )

  # Conclude
  .conclusion(tim)

}


#' @export

FinalTrees <- function(treeClassPoly_RSDS, reclassList, boundary, out_file, tileNames = NULL){

  tim <- .headline("FINAL TREES")

  ### INPUT CHECKS ----

  # Check that inputs are complete
  .check_complete_input(treeClassPoly_RSDS, tileNames)

  # Create temporary directories
  dirs <- list(
    temproot    = file.path(tempdir(), "FinalTrees")
  )
  for(dir in dirs) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(dirs$temproot, recursive = TRUE))

  # Get file paths
  treeClassPoly_paths <- .get_RSDS_tilepaths(treeClassPoly_RSDS)
  outTrees_gpkg       <- file.path(dirs$temproot, "outTrees.gpkg")

  # Select tiles
  selected_tiles <- if(is.null(tileNames)){

    names(treeClassPoly_paths)

  }else{

    notExist <- !tileNames %in% names(treeClassPoly_paths)
    if(any(notExist)) stop("Following tile names do not exist:\n  ", paste(tileNames[notExist], collapse = "\n  "))

    tileNames
  }

  # Initiate ID counter
  IDcounter <- 0

  ### BOUNDARY ----

  # Read boundary
  boundary_sp <- sf::st_read(boundary, quiet = TRUE)
  boundary_sp$geom <- suppressPackageStartupMessages(lwgeom::lwgeom_make_valid(boundary_sp$geom))
  boundary_sp <- sf::st_transform(boundary_sp,   getOption("misterRS.crs"))

  ### CREATE WORKER ----

  for(tileName in selected_tiles){

    # File paths
    treeClassPoly_path <- treeClassPoly_paths[tileName]

    # Read in polygons
    treeClassPoly <- sf::st_read(treeClassPoly_path, quiet = TRUE)

    # Correct geometry
    is_valid <- sf::st_is_valid(treeClassPoly)
    if(any(!is_valid)){
      treeClassPoly <- sf::st_make_valid(treeClassPoly)
    }

    # Intersection with boundary
    ints <- sf::st_intersects(boundary_sp, treeClassPoly)
    treeClassPoly <- treeClassPoly[unique(unlist(ints)),]

    # Remove non trees
    if("NotTree" %in% names(reclassList)){

      treeClassPoly <- treeClassPoly[!treeClassPoly[["segClass"]] %in% reclassList[["NotTree"]],]

    }

    # Remove non-classified
    treeClassPoly <- treeClassPoly[!is.na(treeClassPoly[["segClass"]]),]

    if(nrow(treeClassPoly) > 0){

      # Reclassify trees
      treeClasses <- reclassList[names(reclassList) != "NotTree"]
      for(class in names(treeClasses)){

        treeClassPoly[treeClassPoly[["segClass"]] %in% treeClasses[[class]], "segClass"] <- class

      }

      # Add tree ID
      newIDs <- IDcounter + 1:nrow(treeClassPoly)
      IDcounter <- max(newIDs)
      treeClassPoly[["ID"]] <- newIDs
      #treeClassPoly[["FID"]] <- newIDs

      # Remove and rename attribute columns
      treeClassPoly <- treeClassPoly[,c("ID", "height", "crownArea", "segClass")]
      names(treeClassPoly)[names(treeClassPoly) == "segClass"] <- "class"


      sf::st_write(
        treeClassPoly,
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
  .conclusion(tim)

}


#' @export

FinalOrtho <- function(ortho_RSDS, boundary, out_file, tileNames = NULL){

  tim <- .headline("FINAL ORTHO")

  ### INPUT CHECKS ----

    mosaic_file <- .get_RSDS_mosaicpath(ortho_RSDS)
    if(!file.exists(mosaic_file)) stop("Create mosaic for RSDS '", ortho_RSDS@name, "'")

    # Create temporary directories
    dirs <- list(
      temproot    = file.path(tempdir(), "FinalOrtho")
    )
    for(dir in dirs) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    withr::defer(unlink(dirs$temproot, recursive = TRUE))

    # Get file paths
    boundary_mask_path <- file.path(dirs$temproot, "boundary_mask.shp")

  ### BOUNDARY ----

    # Read boundary
    boundary_sp <- sf::st_read(boundary, quiet = TRUE)

    # Give boundary a masking value
    boundary_sp[["Mask"]] <- 1

    # Buffer
    if(boundary_buff > 0) boundary_sp <- sf::st_buffer(boundary_sp, boundary_buff)

    # Save
    sf::st_write(boundary_sp, boundary_mask_path, quiet = TRUE, delete_dsn = TRUE)

  ### APPLY BOUNDARY

    gpal2::gdalwarp(
      cutline = boundary_mask_path,
      crop_to_cutline = TRUE,
      dstalpha = TRUE,
      mosaic_file,
      R.utils::getAbsolutePath(out_file)
    )

    gpal2::gdaladdo(
      r = "average",
      ro = TRUE,
      R.utils::getAbsolutePath(out_file),
      c(2,4,8,16,32,64)
    )

}
