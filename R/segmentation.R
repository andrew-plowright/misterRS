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
                 writeVectorMode = "ulu",
                 seg_id = "polyID",
                 min_per_tile_est = 3.5, ...){

  process_timer <- .headline("MEAN SHIFT SEGMENTATION")

  .env_misterRS(list(...))

  tile_names <- getOption("misterRS.tile_names")

  ### INPUT CHECKS ----
  .complete_input(img_rts)

  # Get tiles
  ts <- .tilescheme()

  ### CREATE VRT MOSAIC ----

  if(is.null(tile_names)) tile_names <- img_rts$available_tiles()

  tile_paths <- img_rts$tile_path(tile_names)

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
    writeVectorMode = writeVectorMode)

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
                 writeVectorMode = NULL){

  orfeo_dir = getOption("misterRS.orfeo")
  if(!dir.exists(orfeo_dir)) stop("Orfeo Toolbox directory not found '", orfeo_dir, "'")

  profile_file <- file.path(orfeo_dir, "otbenv.profile")
  if(!file.exists(profile_file)) stop("Orfeo Toolbox profile file not found '", profile_file, "'")

  bin_file <- file.path(orfeo_dir, "bin", "otbcli_Segmentation")
  if(!file.exists(bin_file)) stop("Orfeo Toolbox segmentation command not found '", bin_file, "'")

  outExt <- toupper(tools::file_ext(out_file))

  writeMode <- if(outExt %in% c("TIF")){

    c("-mode", "raster", "vector",
      "-mode.raster.out", shQuote(out_file), "int32")

  }else if(outExt %in% c("SHP", "GPKG")){

    c("-mode", "vector",
      "-mode.vector.out", shQuote(out_file),
      if(!is.null(writeVectorMode)) paste("-mode.vector.outmode", writeVectorMode))

  }else stop("Unsupported output format")


  command <- paste(
    'bash -c "source',
    profile_file,
    '&&' ,
    bin_file,
    "-in", shQuote(in_file),
    paste(writeMode, collapse = " "),
    "-filter", "meanshift",
    "-filter.meanshift.spatialr", spat,
    "-filter.meanshift.ranger",   spec,
    "-filter.meanshift.minsize",  mins,
    "-mode.vector.simplify", 0.1,
    "-progress 1",
    '"'
  )


  # Execute command
  system(command, intern = TRUE)
}


#' Tile polygons
#'
#' Convert a single polygonal dataset (must be in GPKG format) to tiles
#'
#' @importFrom sf st_sf
#'
#' @export

tile_poly <- function(in_gpkg, out_vts,  ...){

  .env_misterRS(list(...))

  # Do not attempt to write to Geopackage using multiple clusters
  withr::local_options("misterRS.cluster" = 1)

  process_timer <- .headline("TILE POLYGONS")

  # Get tile scheme
  ts <- .tilescheme()

  crs <- getOption("misterRS.crs")

  # Get GeoPackage layer name
  lyr_name <- sf::st_layers(in_gpkg)$name[1]


  # Create tile directory
  geom_tiles_dir <- out_vts$temp_tile_dir("geom")
  if(!dir.exists(geom_tiles_dir)) dir.create(geom_tiles_dir)

  # Run process
  tile_worker <-function(tile_name){

    #tile <- ts[tile_name][["tiles"]]
    nbuff <- ts[tile_name][["nbuffs"]]

    # Get bounding box
    bbox <- sf::st_bbox(nbuff)

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
    sf::st_crs(polys) <- sf::st_crs(crs)

    if(nrow(polys) > 0){

      # Fix invalid geometry
      if(any(!sf::st_is_valid(polys))) polys$geom <- suppressPackageStartupMessages(lwgeom::lwgeom_make_valid(polys$geom))

      # Get vector of polygons that straddle tile borders
      crossborder <- !1:nrow(polys) %in% unlist(sf::st_contains(ts[["buffs"]], polys))

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
      polys[[out_vts$id_field]] <- 1:nrow(polys)

      # Drop unwanted fields
      polys <- polys[,out_vts$id_field]

    }else{
      # Rename empty ID field
      colnames(polys)[colnames(polys)=="DN"] <- out_vts$id_field
    }

    # Write file
    out_vts$write_geom_tile(polys, tile_name)

    return("Success")
  }

  ### APPLY WORKER ----

  out_vts %>%
    .tile_queue("geom") %>%
    .exe_tile_worker(tile_worker, cluster_vts = "out_vts") %>%
    .print_process_status()

  # Create index
  out_vts$index()

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
  .complete_input(ttops_vts, attribute = "geom", buffered = TRUE)

  # Get tile scheme and projection
  ts <- .tilescheme()
  proj <- getOption("misterRS.crs")

  out_vts$connect()
  ttops_vts$connect()

  # Add fields
  out_vts$add_field("height",     "REAL")
  out_vts$add_field("crown_area", "REAL")

  out_vts$disconnect()
  ttops_vts$disconnect()

  # Get ID field
  tree_id <- ttops_vts$id_field

  ### CREATE WORKER ----

  # Run process
  tile_worker <-function(tile_name){

    # Read in CHM
    CHM <- terra::rast(chm_rts$tile_path(tile_name))

    # Get buffered tile
    buff <- ts[tile_name][["buffs"]]

    # Empty output
    seg_poly_tile <- sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs( proj)), setNames(list(integer()), tree_id))

    # Read treetops
    ttops <- ttops_vts$read_from_polys( buff, fields = c(tree_id, "tile_name", "height", "geom"))[[1]]

    if(nrow(ttops) > 0){

      # Awkward renumbering to enforce unique IDs
      ttops_tile <- ttops[ttops$tile_name == tile_name,]
      ttops_buff <- ttops[ttops$tile_name != tile_name,]

      if(nrow(ttops_tile) > 0){

        if(nrow(ttops_buff) > 0){

          max_id <- max(ttops_tile[[tree_id]], na.rm=T)

          ttops_buff[[tree_id]] <- 1:nrow(ttops_buff) + max_id
        }

        ttops <- rbind(ttops_tile, ttops_buff)

        # Apply 'marker-controlled watershed segmentation' algorithm
        seg_poly  <- ForestTools::mcws(ttops, CHM, minHeight = minCrownHgt, format = "polygon", IDfield = tree_id)

        sf::st_geometry(seg_poly) <- "geom"

        # Subset only those segments that have treetops within non-buffered tile boundaries
        seg_poly_tile <-  seg_poly[match(ttops_tile[[tree_id]], seg_poly[[tree_id]]),]

        # Seg poly attributes
        seg_poly_tile[["height"]]     <- ttops_tile$height
        seg_poly_tile[["crown_area"]] <- as.numeric(sf::st_area(seg_poly_tile))

      }
    }

    out_vts$write_geom_tile(seg_poly_tile, tile_name = tile_name)

    # Write file
    return("Success")
  }

  ### APPLY WORKER ----

  out_vts %>%
    .tile_queue("geom") %>%
    .exe_tile_worker(tile_worker, cluster_vts = "out_vts") %>%
    .print_process_status()

  # Create index
  out_vts$index()

  # Conclude
  .conclusion(process_timer)

}


#' Rasterize polygonal segments
#'
#' @export

poly_to_ras <- function(in_vts, out_rts, res, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("RASTER SEGMENTS")

  ### INPUT CHECKS ----

  .complete_input(in_vts, attribute = "geom")

  ts <- .tilescheme()

  in_vts$connect()

  ### CREATE WORKER ----

  tile_worker <-function(tile_name){

    out_path <- out_rts$tile_path(tile_name)

    tile <- ts[tile_name][["buffs"]]

    # Rasterize asset outline
    gpal2::gdal_rasterize(
      a = in_vts$id_field,
      a_nodata = 0,
      co = c("COMPRESS=LZW"),
      te = terra::ext(tile),
      tr = c(res,res),
      ot = "UInt32",
      sql = sprintf("SELECT * FROM %s WHERE tile_name = '%s'", in_vts$geom_layer, tile_name),
      in_vts$gpkg,
      out_path
    )

    if(file.exists(out_path)) "Success" else stop("Failed")
  }

  ### APPLY WORKER ----

  out_rts %>%
    .tile_queue  %>%
    .exe_tile_worker(tile_worker) %>%
    .print_process_status()

  # Conclude
  .conclusion(process_timer)

}
