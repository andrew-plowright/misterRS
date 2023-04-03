#' Mean Segment Shift 2 - Segment Into Single GeoPackage
#'
#' This variation of the MSS algorithm will write all segments into one
#' giant GeoPackage file. The CLI algorithm's internal tiling system is used to avoid
#' overloading memory, and the algorithm also does a decent job at stiching tiles together
#' afterwards.
#'
#' HOWEVER: the stitching process is sometimes interrupted by invalid geometry, leaving
#' weird-looking breaks in some of the larger polygons
#'
#' Once \code{MSS2} has been run, use \code{MSS2chop} to then re-tile the output
#'
#' @export

segment_mss <- function(img_rsds, out_gpkg,
                 spat = 19.5, spec = 17, mins = 40,
                 writeVectoreMode = "ulu",
                 seg_id = "polyID",
                 tile_names = NULL,
                 min_per_tile_est = 4.3){

  process_timer <- .headline("MEAN SHIFT SEGMENTATION")

  ### INPUT CHECKS ----

  bin_file <- file.path(getOption("misterRS.orfeo"), "bin", "otbcli_Segmentation.bat")
  if(!file.exists(bin_file)) stop("Orfeo Toolbox binary not found '", bin_file, "'")

  .check_extension(img_rsds, "tif")

  .check_complete_input(img_rsds, tile_names)

  # Get tiles
  ts <- .get_tilescheme()


  ### CREATE VRT MOSAIC ----

  tile_paths <- .get_rsds_tilepaths(img_rsds)
  if(!is.null(tile_names)) tile_paths <- tile_paths[tile_names]
  mosaic_vrt <- .mosaic_vrt(tile_paths, ts, overlap = "nbuffs")

  ### PROCESS ----

  cat(
    "  Number of tiles        : ", length(tile_paths), "\n",
    "  Estimated minutes/tile : ", min_per_tile_est, " minutes\n",
    "  Estimated total time   : ", round(length(tile_paths) * min_per_tile_est / 60,2), " hours\n\n",
    sep = "")

  mss_result <- .exe_mss(
    in_file  = mosaic_vrt,
    out_file = out_gpkg,
    spat    = spat,
    spec    = spec,
    mins    = mins,
    bin_file = bin_file,
    writeVectorMode = writeVectoreMode)

  # Filter out warnings about self-intersections
  mss_result <- mss_result[!grepl("Self-intersection", mss_result)]

  # Print message from algorithm
  cat("  ", paste(mss_result, collapse = "\n  "), "\n\n", sep = "")


  ### CONCLUDE ----

  # Calculate how long it took per tile
  total_time <- as.numeric(difftime(Sys.time(), process_timer, units="mins"))
  min_per_tile <- total_time / length(tile_paths)
  cat(crayon::bold("  Actual minutes/tile    : ", round(min_per_tile, 2), " minutes\n", sep = ""))

  .conclusion(process_timer)
}

.exe_mss <- function(in_file, out_file, spat, spec, mins,
                 writeVectorMode = NULL, bin_file){

  outExt <- toupper(tools::file_ext(out_file))

  writeMode <- if(outExt %in% c("TIF")){

    c("-mode", "raster", "vector",
      "-mode.raster.out", shQuote(out_file), "int32")

  }else if(outExt %in% c("SHP", "GPKG")){

    c("-mode", "vector",
      "-mode.vector.out", shQuote(out_file),
      if(!is.null(writeVectorMode)) paste("-mode.vector.outmode", writeVectorMode))

  }else stop("Unsupported output format")

  # Execute command
  system(paste(

    shQuote(bin_file),
    "-in", shQuote(in_file),
    paste(writeMode, collapse = " "),
    "-filter", "meanshift",
    "-filter.meanshift.spatialr", spat,
    "-filter.meanshift.ranger",   spec,
    "-filter.meanshift.minsize",  mins
  ), intern = TRUE)
}


#' Tile polygons
#'
#' Convert a single polygonal dataset (must be in GPKG format) to tiles
#'
#' @importFrom sf st_sf
#'
#' @export

tile_poly <- function(in_gpkg, seg_poly_rsds, seg_id = "polyID", tile_names = NULL, overwrite = FALSE){

  process_timer <- .headline("TILE POLYGONS")

  # Get tile scheme
  ts <- .get_tilescheme()

  crs <- getOption("misterRS.crs")

  # Get buffered areas as Simple Features
  ts_buffs  <- sf::st_as_sf(ts[["buffs" ]])
  ts_tiles  <- sf::st_as_sf(ts[["tiles" ]])
  ts_nbuffs <- sf::st_as_sf(ts[["nbuffs"]])

  # Get output paths
  out_paths <- .get_rsds_tilepaths(seg_poly_rsds)

  # Get GeoPackage layer name
  lyr_name <- sf::st_layers(in_gpkg)$name[1]

  # Run process
  tile_worker <-function(tile_name){

    out_path <- out_paths[tile_name]

    tile  <- ts_tiles [ts_tiles [["tileName"]] == tile_name,]
    buff  <- ts_buffs [ts_buffs [["tileName"]] == tile_name,]
    nbuff <- ts_nbuffs[ts_nbuffs[["tileName"]] == tile_name,]

    # Get bounding box
    bbox <- sf::st_bbox(tile)

    # Get statement for selecting polygons that intersect with tile
    sel = paste0("SELECT * FROM ", lyr_name ,
                 " WHERE fid IN (SELECT id FROM rtree_", lyr_name,
                 "_geom AS r WHERE r.miny < ", bbox["ymax"],
                 " AND r.maxy > ", bbox["ymin"],
                 " AND r.minx < ", bbox["xmax"],
                 " AND r.maxx > ", bbox["xmin"],
                 ")")

    # Read tiles
    polys <- sf::st_read(in_gpkg, query = sel, quiet = TRUE)

    # Force crs
    suppressWarnings(sf::st_crs(polys) <- sf::st_crs(crs))

    # Fix invalid geometry
    if(any(!sf::st_is_valid(polys))) polys$geom <- suppressPackageStartupMessages(lwgeom::lwgeom_make_valid(polys$geom))

    # Get vector of polygons that straddle tile borders
    crossborder <- !1:nrow(polys) %in% unlist(sf::st_contains(ts_buffs,  polys))

    if(any(crossborder)){

      # Break up polygons that straddle tile borders
      brokeup <- suppressWarnings(sf::st_intersection(polys[crossborder,], nbuff))

      # Get rid of slivers
      brokeup <- brokeup[sf::st_geometry_type(brokeup) %in% c('POLYGON', 'MULTIPOLYGON', 'GEOMETRYCOLLECTION'),]

      # Get "POLYGONS" from "GEOMETRYCOLLECTION
      brokeup <- suppressWarnings(sf::st_collection_extract(brokeup, "POLYGON"))

      # Swap cross-border polys with broken-up polys
      polys <- rbind(polys[!crossborder,], brokeup[,"DN"])

    }

    # Explode multipart polygons
    # As suggested by: https://github.com/r-spatial/sf/issues/763
    polys <- suppressWarnings(sf::st_cast(sf::st_cast(polys, "MULTIPOLYGON"), "POLYGON"))

    # Generate centroids
    cent <- suppressWarnings(sf::st_centroid(polys))

    # Subset segments with centroid within tile
    intrs <- sapply(sf::st_intersects(cent, nbuff), "[", 1)
    polys <- polys[!is.na(intrs),]

    # Assign numbers
    polys[[seg_id]] <- 1:nrow(polys)
    polys$FID <- as.numeric(polys[[seg_id]])

    # Write file
    sf::st_write(polys, out_path, quiet = TRUE, fid_column_name = "FID", delete_dsn = overwrite)

    if(file.exists(out_path)) "Success" else stop("Failed to create output")
  }

  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_paths, overwrite, tile_names)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)
}


#' Watershed Segmentation
#'
#' @export

segment_watershed <- function(out_rsds, chm_rsds, ttops_rsds,
                                  minCrownHgt = 0.2,
                                  tile_names = NULL, overwrite = FALSE){

  process_timer <- .headline("WATERSHED SEGMENTATION")

  ### INPUT CHECKS ----

  # Check extensions
  .check_extension(out_rsds, "shp")

  # Check that inputs are complete
  .check_complete_input(chm_rsds,   tile_names)
  .check_complete_input(ttops_rsds, tile_names)

  # Get tile scheme
  ts <- .get_tilescheme()

  # Get file paths
  CHM_paths   <- .get_rsds_tilepaths(chm_rsds)
  ttops_paths <- .get_rsds_tilepaths(ttops_rsds)
  out_paths   <- .get_rsds_tilepaths(out_rsds)

  ### CREATE WORKER ----

  # Run process
  tile_worker <-function(tile_name){

    # File paths
    out_path   <- out_paths[tile_name]
    ttops_path <- ttops_paths[tile_name]
    CHM_path   <- CHM_paths[tile_name]

    # Get tile
    tile <- sf::st_as_sf(ts[tile_name][["tiles"]])

    # Read in files
    CHM <- terra::rast(CHM_path)
    ttops <- sf::st_read(ttops_path, quiet = TRUE)

    if(nrow(ttops) > 0){

      # Apply 'marker-controlled watershed segmentation' algorithm
      seg_poly  <- ForestTools::mcws(ttops, CHM, minHeight = minCrownHgt, format = "polygon")

      # Subset only those segments that have treetops within tile boundaries
      ttops_tile   <- ttops[tile,]
      seg_poly_tile <- seg_poly[match(ttops_tile$treeID, seg_poly$treeID),]

      # Seg poly attributes
      seg_poly_tile[["height"]] <- ttops_tile$height
      seg_poly_tile[["crownArea"]] <- as.numeric(sf::st_area(seg_poly_tile))

      # Remove polygons with no 'treeID'
      # (This happens when a treetop does not have an associated polygon)
      seg_poly_tile <- seg_poly_tile[!is.na(seg_poly_tile$treeID),]

      # Subset desired columns
      seg_poly_tile <- seg_poly_tile[,c("treeID", "height", "crownArea", "geometry")]

    }else{

      # Create blank polygons
      seg_poly_tile <- sf::st_sf(data.frame(treeID = integer(), height = numeric(), crownArea = numeric()), geometry = sf::st_sfc(crs = sf::st_crs(ttops)))
    }

    # Write file
    sf::st_write(seg_poly_tile, out_path, layer_options = "SHPT=POLYGON", quiet = T, delete_dsn = overwrite & file.exists(out_path))

    if(file.exists(out_path)) "Success" else stop("Failed to create output")
  }

  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_paths, overwrite, tile_names)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)

}


#' Rasterize polygonal segments
#'
#' @export

poly_to_ras <- function(seg_poly_rsds, seg_ras_rsds, res, seg_id = "polyID",
                          tile_names = NULL, overwrite = FALSE){

  process_timer <- .headline("RASTER SEGMENTS")

  ### INPUT CHECKS ----

  .check_complete_input(seg_poly_rsds, tile_names)

  .check_extension(seg_ras_rsds, "tif")

  ts <- .get_tilescheme()

  # Get output file paths
  in_paths  <- .get_rsds_tilepaths(seg_poly_rsds)
  out_paths <- .get_rsds_tilepaths(seg_ras_rsds)

  ### CREATE WORKER ----

  tile_worker <-function(tile_name){

    in_path  <- in_paths[tile_name]
    out_path <- out_paths[tile_name]

    tile <- ts[tile_name][["buffs"]]

    # Rasterize asset outline
    gpal2::gdal_rasterize(
      a = seg_id,
      a_nodata = 0,
      co = c("COMPRESS=LZW"),
      te = raster::extent(tile),
      tr = c(res,res),
      ot = "UInt32",
      in_path,
      out_path
    )

    if(file.exists(out_path)) "Success" else stop("Failed")
  }

  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_paths, overwrite, tile_names)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)

}
