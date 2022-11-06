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
                 tile_names = NULL){

  process_timer <- .headline("MSS 2 - SEGMENT")

  ### INPUT CHECKS ----

  bin_file <- file.path(getOption("misterRS.orfeo"), "bin", "otbcli_Segmentation.bat")
  if(!file.exists(bin_file)) stop("Orfeo Toolbox binary not found '", bin_file, "'")

  .check_extension(img_rsds, "tif")

  .check_complete_input(img_rsds, tile_names)

  # Get tiles
  ts <- .get_tilescheme()


  ### CREATE VRT MOSAIC ---

  tile_paths <- .get_rsds_tilepaths(img_rsds)
  if(!is.null(tile_names)) tile_paths <- tile_paths[tile_names]

  mosaic_vrt <- .mosaic_vrt(tile_paths, ts, overlap = "nbuffs")

  mss_result <- .exe_mss(
    in_file  = mosaic_vrt,
    out_file = out_gpkg,
    spat    = spat,
    spec    = spec,
    mins    = mins,
    bin_file = bin_file,
    writeVectorMode = writeVectoreMode)

  print(mss_result)

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
#' @export

tile_poly <- function(in_gpkg, seg_poly_rsds, chunk_size = 2000, seg_id = "polyID"){

  process_timer <- .headline("TILE POLYGONS")

  # Get tile scheme
  ts <- .get_tilescheme()

  # Get buffered areas as Simple Features
  ts_buffs  <- sf::st_as_sf(ts[["buffs" ]])
  ts_tiles  <- sf::st_as_sf(ts[["tiles" ]])
  ts_nbuffs <- sf::st_as_sf(ts[["nbuffs"]])

  tilePaths <- .get_rsds_tilepaths(seg_poly_rsds)

  # Get GeoPackage layer name
  lyrName <- sf::st_layers(in_gpkg)$name[1]

  cat("  Retrieving feature IDs", "\n")

  # Get GeoPackage feature IDs
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = in_gpkg)
  res <- RSQLite::dbSendQuery(con, sprintf("SELECT fid FROM %s", lyrName))
  fid <- RSQLite::dbFetch(res)[,1]
  RSQLite::dbClearResult(res)
  RSQLite::dbDisconnect(con)

  # Get chunks of GeoPackage feature IDs
  chunks <- split(fid, ceiling(seq_along(fid)/chunk_size))

  cat(
    "  Segments   : ", formatC(length(fid),    format="f", big.mark=",", digits = 0), "\n",
    "  Chunk size : ", formatC(chunk_size,     format="f", big.mark=",", digits = 0), "\n",
    "  Chunks     : ", formatC(length(chunks), format="f", big.mark=",", digits = 0), "\n",
    sep = ""
  )

  pb <- .progbar(length(chunks))

  # Loop through chunks
  for(i in 1:length(chunks)){

    # Select polygons in chunk
    sel <- sprintf("SELECT * FROM %1$s WHERE FID IN (%2$s)", lyrName, paste(chunks[[i]], collapse = ", "))
    polys <- sf::st_read(in_gpkg, query = sel, quiet = TRUE)
    suppressWarnings(sf::st_crs(polys) <- sf::st_crs(ts_buffs))

    # Fix invalid geometry
    if(any(!sf::st_is_valid(polys))) polys$geom <- suppressPackageStartupMessages(lwgeom::lwgeom_make_valid(polys$geom))

    # Get vector of polygons that straddle tile borders
    crossborder <- !1:nrow(polys) %in% unlist(sf::st_contains(ts_buffs,  polys))

    if(any(crossborder)){

      # Break up polygons that straddle tile borders
      brokeup <- suppressWarnings(sf::st_intersection(polys[crossborder,], ts_nbuffs))

      brokeup <- brokeup[sf::st_geometry_type(brokeup) %in% c('POLYGON', 'MULTIPOLYGON', 'GEOMETRYCOLLECTION'),]

      if(nrow(brokeup) != 0){
        brokeup <- suppressWarnings(sf::st_collection_extract(brokeup, "POLYGON"))
        polys <- rbind(polys[!crossborder,], brokeup[,"DN"])
      }
    }

    # Explode multipart polygons
    # As suggested by: https://github.com/r-spatial/sf/issues/763
    polys <- suppressWarnings(sf::st_cast(sf::st_cast(polys, "MULTIPOLYGON"), "POLYGON"))

    # Generate centroids
    cent <- suppressWarnings(sf::st_centroid(polys))

    # Tile names (for centroids)
    intrs <- sapply(sf::st_intersects(cent, ts_nbuffs), "[", 1)
    polys$tile_names <- ts_nbuffs$tile_name[intrs]

    for(tile_name in  na.omit(unique(polys$tile_names))){

      # Set file path
      tilePath <- tilePaths[tile_name]

      # Get current number of segments in the tile
      n <- if(!file.exists(tilePath)) 0 else sf::st_layers(tilePath)$features[1]

      # Get subset of polygons
      tilePoly <- polys[polys$tile_names == tile_name,]

      # Set DN and FID values
      tilePoly[[seg_id]] <- 1:nrow(tilePoly) + n
      tilePoly$FID <- as.numeric(tilePoly[[seg_id]])

      sf::st_write(tilePoly, tilePath, append = TRUE, quiet = TRUE, fid_column_name = "FID")
    }

    pb$tick()
  }

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

      seg_poly_tile <- seg_poly_tile[,c("treeID", "height", "crownArea", "geometry")]

    }else{

      # Create blank polygons
      seg_poly_tile <- sf::st_sf(data.frame(treeID = integer(), height = numeric(), crownArea = numeric()), geometry = sf::st_sfc(crs = sf::st_crs(ttops)))
    }

    # Write file
    sf::st_write(seg_poly_tile, out_path, layer_options = "SHPT=POLYGON", quiet = T)

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
