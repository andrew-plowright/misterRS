#' Mean Segment Shift (deprecated)
#'
#' WARNING: As of 2019-12-11, this function is semi-deprecated.
#' This version of the MSS algorithm processes each tile individually.
#' It appears preferable to generate all segments at once AND THEN chop the
#' output into individual tiles, for which the \code{MSS2} and \code{MSS2chop}
#' have been made available.
#'
#' @export

MSS <- function(image_RSDS, segPoly_RSDS, segRas_RSDS,
                spat = 19.5, spec = 17, mins = 40,
                segID = "polyID",
                tileNames = NULL, clusters = 1, overwrite = FALSE){

  tim <- .headline("MEAN SEGMENT SHIFT")

  ### INPUT CHECKS ----

  binFile <- file.path(getOption("misterRS.orfeo"), "bin", "otbcli_Segmentation.bat")
  if(!file.exists(binFile)) stop("Orfeo Toolbox binary not found '", binFile, "'")

  .check_same_ts(image_RSDS, segPoly_RSDS, segRas_RSDS)

  .check_extension(image_RSDS,   "tif")
  .check_extension(segPoly_RSDS, "shp")
  .check_extension(segRas_RSDS,  "tif")

  .check_complete_input(image_RSDS, tileNames)


  ### CREATE WORKER ----

  worker <- function(tileName){

    tile <- image_RSDS@tileScheme[tileName,][["tiles"]]

    imagePath   <-   image_RSDS@tilePaths[tileName]
    segRasPath  <-  segRas_RSDS@tilePaths[tileName]
    segPolyPath <- segPoly_RSDS@tilePaths[tileName]

    # Force delete
    if(file.exists(segPolyPath)) unlink(APfun::APSHPfiles(segPolyPath))
    if(file.exists(segRasPath)) unlink(APfun::APrasterFiles(segRasPath))

    # Create segments
    mss_result <- .mss(
      inFile  = imagePath,
      outFile = segRasPath,
      spat    = spat,
      spec    = spec,
      mins    = mins,
      binFile = binFile)

    if(!file.exists(segRasPath)) stop("Failed to create raster segments")

    # Polygonize raster
    osgeopy::pyScript(
      "gdal_polygonize",
      paste(
        shQuote(segRasPath),
        "-f", shQuote("ESRI Shapefile"),
        shQuote(segPolyPath)
      )
    )

    if(!file.exists(segPolyPath)) stop("Failed to create polygon segments")

    # Read polygons and raster
    segPoly <- APfun::APSHPread(segPolyPath)
    segRas <- raster::raster(segRasPath)

    # Set unique ID name
    names(segPoly) <- segID

    # Get centroids
    segCent <- suppressWarnings(rgeos::gCentroid(segPoly, byid = TRUE))
    segCent <- sp::SpatialPointsDataFrame(segCent, segPoly@data)

    # Get centroids within tile
    segCentTile <- segCent[tile,]
    segPolyTile <- segPoly[segPoly[[segID]] %in% segCentTile[[segID]],]
    segRasTile <- segRas
    segRasTile[!segRas[] %in% segPolyTile[[segID]]] <- NA

    # Overwrite subset segments
    APfun::APSHPsave(segPolyTile, segPolyPath, overwrite = TRUE)
    raster::writeRaster(segRasTile, segRasPath, overwrite = TRUE)

    if(!file.exists(segRasPath)) stop("Failed to create raster segments")
    if(!file.exists(segPolyPath)) stop("Failed to create polygon segments")

    return("Success")
  }


  ### APPLY WORKER ----

  # Get tiles for processing
  procTiles <- .processing_tiles(segPoly_RSDS, overwrite, tileNames)

  # Process
  status <- .doitlive(procTiles, clusters, worker)

  # Report
  .statusReport(status)

  # Conclude
  .conclusion(tim)

}


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

MSS2 <- function(image_RSDS, out_GPKG,
                 spat = 19.5, spec = 17, mins = 40,
                 writeVectoreMode = "ulu",
                 segID = "polyID",
                 tileNames = NULL){

  tim <- .headline("MSS 2 - SEGMENT")

  ### INPUT CHECKS ----

  binFile <- file.path(getOption("misterRS.orfeo"), "bin", "otbcli_Segmentation.bat")
  if(!file.exists(binFile)) stop("Orfeo Toolbox binary not found '", binFile, "'")

  .check_extension(image_RSDS, "tif")

  .check_complete_input(image_RSDS, tileNames)


  ### CREATE VRT MOSAIC ---

  mosaicVRT <- .mosaicVRT(image_RSDS, overlap = "nbuffs", tileNames = tileNames)

  mss_result <- .mss(
    inFile  = mosaicVRT,
    outFile = out_GPKG,
    spat    = spat,
    spec    = spec,
    mins    = mins,
    binFile = binFile,
    writeVectorMode = writeVectoreMode)

  print(mss_result)

  .conclusion(tim)
}

#' Mean Segment Shift 2 - Chop GeoPackage Into Tiles
#'
#' After having run \code{MSS2}, use this function to chop the output
#' into tiles while managing polygons that cross into the tiles' buffers effectively
#'
#' @export

MSS2chop <- function(in_GPKG, segPoly_RSDS, chunk_size = 2000, segID = "polyID"){

  tim <- .headline("MSS 2 - CHOP TILES")

  # Get buffered areas as Simple Features
  ts_buffs  <- sf::st_as_sf(segPoly_RSDS@tileScheme[["buffs" ]])
  ts_tiles  <- sf::st_as_sf(segPoly_RSDS@tileScheme[["tiles" ]])
  ts_nbuffs <- sf::st_as_sf(segPoly_RSDS@tileScheme[["nbuffs"]])

  # Get GeoPackage layer name
  lyrName <- sf::st_layers(in_GPKG)$name[1]

  cat("  Retrieving feature IDs", "\n")

  # Get GeoPackage feature IDs
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = in_GPKG)
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
    polys <- sf::st_read(in_GPKG, query = sel, quiet = TRUE)

    # Fix invalid geometry
    if(any(!sf::st_is_valid(polys))) polys$geom <- suppressPackageStartupMessages(lwgeom::lwgeom_make_valid(polys$geom))

    # Get vector of polygons that straddle tile borders
    crossborder <- !1:nrow(polys) %in% unlist(sf::st_contains(ts_buffs,  polys))

    if(any(crossborder)){

      # Break up polygons that straddle tile borders
      brokeup <- suppressWarnings(sf::st_intersection(polys[crossborder,], ts_nbuffs))

      if(nrow(brokeup) != 0) brokeup <- suppressWarnings(sf::st_collection_extract(brokeup, "POLYGON"))

      polys <- rbind(polys[!crossborder,], brokeup[,"DN"])
    }

    # Explode multipart polygons
    # As suggested by: https://github.com/r-spatial/sf/issues/763
    polys <- suppressWarnings(sf::st_cast(sf::st_cast(polys, "MULTIPOLYGON"), "POLYGON"))

    # Generate centroids
    cent <- suppressWarnings(sf::st_centroid(polys))

    # Tile names (for centroids)
    intrs <- sapply(sf::st_intersects(cent, ts_nbuffs), "[", 1)
    polys$tileNames <- ts_nbuffs$tileName[intrs]

    for(tileName in unique(polys$tileNames)){

      # Set file path
      tilePath <- segPoly_RSDS@tilePaths[tileName]

      # Get current number of segments in the tile
      n <- if(!file.exists(tilePath)) 0 else sf::st_layers(tilePath)$features[1]

      # Get subset of polygons
      tilePoly <- polys[polys$tileNames == tileName,]

      # Set DN and FID values
      tilePoly[[segID]] <- 1:nrow(tilePoly) + n
      tilePoly$FID <- as.numeric(tilePoly[[segID]])

      sf::st_write(tilePoly, tilePath, update = TRUE, quiet = TRUE, fid_column_name = "FID")
    }

    pb$tick()
  }

  .conclusion(tim)
}


.mss <- function(inFile, outFile, spat, spec, mins,
                 writeVectorMode = NULL, binFile){

  outExt <- toupper(tools::file_ext(outFile))

  writeMode <- if(outExt %in% c("TIF")){

    c("-mode", "raster", "vector",
      "-mode.raster.out", shQuote(outFile), "int32")

  }else if(outExt %in% c("SHP", "GPKG")){

    c("-mode", "vector",
      "-mode.vector.out", shQuote(outFile),
      if(!is.null(writeVectorMode)) paste("-mode.vector.outmode", writeVectorMode))

  }else stop("Unsupported output format")

  # Execute command
  system(paste(

    shQuote(binFile),
    "-in", shQuote(inFile),
    paste(writeMode, collapse = " "),
    "-filter", "meanshift",
    "-filter.meanshift.spatialr", spat,
    "-filter.meanshift.ranger",   spec,
    "-filter.meanshift.minsize",  mins
  ), intern = TRUE)
}

#' Watershed Segmentation
#'
#' @export

WatershedSegmentation <- function(out_RSDS, CHM_RSDS, ttops_RSDS,
                                  OSGeoPath = "C:/OSGeo4W64",
                                  clusters = 1, tileNames = NULL, overwrite = FALSE){


  tim <- .headline("WATERSHED SEGMENTATION")

  ### INPUT CHECKS ----

  # Check that all RSDS have same tileScheme
  .check_same_ts(out_RSDS, CHM_RSDS, ttops_RSDS)

  # Check extensions
  .check_extension(out_RSDS, "shp")

  # Check that inputs are complete
  .check_complete_input(CHM_RSDS,  tileNames)
  .check_complete_input(ttops_RSDS, tileNames)

  ### CREATE WORKER ----

  # Run process
  worker <- function(tileName){

    # Get tile
    tile <- out_RSDS@tileScheme[tileName][["tiles"]]

    # File paths
    out_file   <- out_RSDS@tilePaths[tileName]
    ttops_file <- ttops_RSDS@tilePaths[tileName]
    CHM_file   <- CHM_RSDS@tilePaths[tileName]

    # CHM
    CHM <- raster::raster(CHM_file)

    if(suppressWarnings(rgdal::ogrInfo(ttops_file)$have_features)){

      # Read in files
      ttops <- APfun::APSHPread(ttops_file)
      raster::crs(ttops) <- raster::crs(CHM)

      # Apply 'marker-controlled watershed segmentation' algorithm
      segPoly  <- ForestTools::mcws(
        ttops, CHM,
        minHeight = 0.1,
        format    = "polygon",
        OSGeoPath = OSGeoPath)

      # Subset only those segments that have treetops within tile boundaries
      ttops_tile   <- ttops[tile,]
      segPoly_tile <- segPoly[segPoly$treeID %in% ttops_tile$treeID,]

    }else{

      # Create blank polygons
      segPoly_tile <- sp::SpatialPolygonsDataFrame(
        sp::SpatialPolygons(list(), proj4string = raster::crs(CHM)),
        data.frame(height = numeric(), winRadius = numeric(), treeID = integer(), crownArea = numeric())
      )
    }

    # Write file
    APfun::APSHPsave(segPoly_tile, out_file, overwrite = overwrite)

    if(file.exists(out_file)) "Success" else stop("Failed to create output")

  }

  ### APPLY WORKER ----

  # Get tiles for processing
  procTiles <- .processing_tiles(out_RSDS, overwrite, tileNames)

  # Process
  status <- .doitlive(procTiles, clusters, worker)

  # Report
  .statusReport(status)

  # Conclude
  .conclusion(tim)

}


#' Rasterize polygonal segments
#'
#' @export

RasterSegment <- function(segPoly_RSDS, segRas_RSDS, res, segID = "polyID",
                          tileNames = NULL, clusters = 1, overwrite = FALSE){

  tim <- .headline("RASTER SEGMENTS")

  ### INPUT CHECKS ----

  .check_same_ts(segRas_RSDS, segPoly_RSDS)

  .check_complete_input(segPoly_RSDS, tileNames)

  .check_extension(segRas_RSDS, "tif")

  ### CREATE WORKER ----

  worker <- function(tileName){

    in_file  <- segPoly_RSDS@tilePaths[tileName]
    out_file <- segRas_RSDS@tilePaths[tileName]

    tile <- segRas_RSDS@tileScheme[tileName][["buffs"]]

    # Rasterize asset outline
    gpal2::gdal_rasterize(
      a = segID,
      a_nodata = 0,
      co = c("COMPRESS=LZW"),
      te = raster::extent(tile),
      tr = c(res,res),
      ot = "UInt16",
      in_file,
      out_file
    )

    if(file.exists(out_file)) "Success" else stop("Failed")
  }

  ### APPLY WORKER ----

  # Get tiles for processing
  procTiles <- .processing_tiles(segRas_RSDS, overwrite, tileNames)

  # Process
  status <- .doitlive(procTiles, clusters, worker)

  # Report
  .statusReport(status)

  # Conclude
  .conclusion(tim)


}

