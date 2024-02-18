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

segment_mss <- function(img_rts, out_gpkg,
                 spat = 19.5, spec = 17, mins = 40,
                 writeVectoreMode = "ulu",
                 seg_id = "polyID",
                 min_per_tile_est = 3.5, ...){

  process_timer <- .headline("MEAN SHIFT SEGMENTATION")

  .env_misterRS(list(...))

  tile_names <- getOption("misterRS.tile_names")

  ### INPUT CHECKS ----

  bin_file <- file.path(getOption("misterRS.orfeo"), "bin", "otbcli_Segmentation.bat")
  if(!file.exists(bin_file)) stop("Orfeo Toolbox binary not found '", bin_file, "'")

  .complete_input(img_rts)

  # Get tiles
  ts <- .tilescheme()

  ### CREATE VRT MOSAIC ----

  tile_paths <- .rts_tile_paths(img_rts)
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

tile_poly <- function(in_gpkg, out_vts, seg_id = "polyID", ...){

  .env_misterRS(list(...))

  process_timer <- .headline("TILE POLYGONS")

  # Get tile scheme
  ts <- .tilescheme()

  crs <- getOption("misterRS.crs")

  # Get buffered areas as Simple Features
  ts_buffs  <- sf::st_as_sf(ts[["buffs" ]])
  ts_tiles  <- sf::st_as_sf(ts[["tiles" ]])
  ts_nbuffs <- sf::st_as_sf(ts[["nbuffs"]])

  # Get GeoPackage layer name
  lyr_name <- sf::st_layers(in_gpkg)$name[1]

  # Run process
  tile_worker <-function(tile_name){

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

    if(nrow(polys) > 0){

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

      # Explode multi-part polygons
      # As suggested by: https://github.com/r-spatial/sf/issues/763
      polys <- suppressWarnings(sf::st_cast(sf::st_cast(polys, "MULTIPOLYGON"), "POLYGON"))

      # Generate centroids
      cent <- suppressWarnings(sf::st_centroid(polys))

      # Subset segments with centroid within tile
      intrs <- sapply(sf::st_intersects(cent, nbuff), "[", 1)
      polys <- polys[!is.na(intrs),]

      # Assign numbers
      polys[[seg_id]] <- 1:nrow(polys)

      # Drop unwanted fields
      polys <- polys[,seg_id]
    }

    # Write file
    .vts_write(polys, out_vts = out_vts, tile_name = tile_name, overwrite = overwrite)

    return("Success")
  }

  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_vts)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Create index
  .vts_create_index(out_vts, "tile_name")

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)
}


#' Watershed Segmentation
#'
#' @export

segment_watershed <- function(out_vts, chm_rts, ttops_vts,
                                  minCrownHgt = 0.3, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("WATERSHED SEGMENTATION")

  # Override parallel processing
  withr::local_options("misterRS.clusters" = 1)

  ### INPUT CHECKS ----

  # Check that inputs are complete
  .complete_input(chm_rts)
  .complete_input(ttops_vts, buffered = TRUE)

  # Get tile scheme and projection
  ts <- .tilescheme()
  proj <- getOption("misterRS.crs")

  # Get file paths
  CHM_paths   <- .rts_tile_paths(chm_rts)

  ### CREATE WORKER ----

  # Run process
  tile_worker <-function(tile_name){

    # Read in CHM
    CHM_path <- CHM_paths[tile_name]
    CHM <- terra::rast(CHM_path)

    # Get buffered tile
    buff <- sf::st_as_sf(ts[tile_name][["buffs"]])

    # Empty output
    seg_poly_tile <- sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs( proj)),list(treeID = integer()))

    # Read treetops
    ttops <-  .vts_read(ttops_vts, geom = buff)

    if(nrow(ttops) > 0){

      # Awkward renumbering to enforce unique IDs
      ttops_tile <- ttops[ttops$tile_name == tile_name,]
      ttops_buff <- ttops[ttops$tile_name != tile_name,]

      if(nrow(ttops_tile) > 0){

        if(nrow(ttops_buff) > 0){

          max_id <- max(ttops_tile[["treeID"]], na.rm=T)

          ttops_buff[["treeID"]] <- 1:nrow(ttops_buff) + max_id
        }

        ttops <- rbind(ttops_tile, ttops_buff)

        # Apply 'marker-controlled watershed segmentation' algorithm
        seg_poly  <- ForestTools::mcws(ttops, CHM, minHeight = minCrownHgt, format = "polygon")

        # Subset only those segments that have treetops within non-buffered tile boundaries
        seg_poly_tile <-  seg_poly[match(ttops_tile[["treeID"]], seg_poly[["treeID"]]),]

        # Seg poly attributes
        seg_poly_tile[["height"]] <- ttops_tile$height
        seg_poly_tile[["crownArea"]] <- as.numeric(sf::st_area(seg_poly_tile))

        # Subset desired columns
        seg_poly_tile <- seg_poly_tile[,c("treeID", "height", "crownArea", "geometry")]
      }
    }

    .vts_write(in_sf = seg_poly_tile, out_vts = out_vts, tile_name = tile_name, overwrite=overwrite)

    # Write file
    return("Success")
  }

  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_vts)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Create index
  .vts_create_index(out_vts, "tile_name")

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)

}


#' Rasterize polygonal segments
#'
#' @export

poly_to_ras <- function(in_vts, out_rts, res, seg_id = "polyID", ...){

  .env_misterRS(list(...))

  process_timer <- .headline("RASTER SEGMENTS")

  ### INPUT CHECKS ----

  .complete_input(in_vts)

  ts <- .tilescheme()

  # Get output file paths
  out_paths <- .rts_tile_paths(out_rts)

  ### CREATE WORKER ----

  tile_worker <-function(tile_name){

    out_path <- out_paths[tile_name]

    tile <- sf::st_as_sf(ts[tile_name][["buffs"]])

    # Rasterize asset outline
    gpal2::gdal_rasterize(
      a = seg_id,
      a_nodata = 0,
      co = c("COMPRESS=LZW"),
      te = terra::ext(tile),
      tr = c(res,res),
      ot = "UInt32",
      sql = sprintf("SELECT * FROM layer WHERE tile_name = '%s'", tile_name),
      in_vts@gpkg,
      out_path
    )

    if(file.exists(out_path)) "Success" else stop("Failed")
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
