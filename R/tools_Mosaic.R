#' Mosaic a RS Dataset
#'
#' @export

Mosaic <- function(RSDS, overlap = "nbuffs", shp_clip = FALSE, outFile = NULL, overwrite = FALSE, tileNames = NULL){

  tim <- .headline("MOSAIC")

  # Get tiles
  ts <- .get_tilescheme()

  # Get output file
  if(is.null(outFile)) outFile <- .get_RSDS_mosaicpath(RSDS)

  if(!dir.exists(dirname(outFile))) stop("Output directory not found")

  if(file.exists(outFile)){
    if(overwrite) unlink(outFile) else stop("Output file already exists")
  }

  # # Get CRS
  crs <- getOption("misterRS.crs")

  # Get extension of input RSDS
  ext <- RSDS@ext

  # Check input
  .check_complete_input(RSDS, tileNames = tileNames)

  # Get input paths
  in_paths <- .get_RSDS_tilepaths(RSDS)
  if(!is.null(tileNames)) in_paths <- in_paths[tileNames]

  # Merge rasters
  if(ext %in% c("tif")){

    # Generate VRT mosaic
    cat("  Making VRT", "\n")
    tempVRT <- .mosaicVRT(in_paths, ts,  overlap)

    # Merge
    cat("  Merging VRT to TIFF", "\n")
    gpal2::gdal_translate(
      co = c("BIGTIFF=YES", "COMPRESS=LZW"),
      tempVRT,
      outFile
    )

    if(!file.exists(outFile)) stop("Failed to merge VRT into single mosaic file")

    # Pyramids
    cat("  Generating pyramids", "\n")
    gpal2::gdaladdo(
      r = "average",
      ro = TRUE,
      outFile,
      c(2,4,8,16,32,64)
    )

  # Merge shape files
  }else if(ext %in% c("shp")){

    # Check if output is a GPKG
    is_gpkg <- toupper(tools::file_ext(outFile)) == "GPKG"

    # If not, create at temporary file
    if(is_gpkg){
      gpkg_path <- outFile
    }else{
      gpkg_path <- tempfile(fileext = ".gpkg")
    }

    # NOTE: This doesn't seem to work. It might be because the
    # 'in_paths' is too long for a command line call
    #
    # gpal2::ogrmerge(
    #   o            = c(outFile, in_paths),
    #   single       = TRUE,
    #   overwrite_ds = overwrite
    # )

    # Read and append files to GPKG
    for(tileName in names(in_paths)){

      # Get SHP file path
      in_path <- in_paths[tileName]

      # Read SHP file
      in_sf <- sf::st_read(in_path, quiet = TRUE)

      # OPTIONAL: Clip out features outside of tile
      if(shp_clip){

        # Get class
        cls <- substr(class(in_sf$geometry)[1], 5, nchar(class(in_sf$geometry)[1]))

        #print(cls)

        if(cls == "GEOMETRY" & nrow(in_sf) == 0){

          # Do nothing with empty geometry

        }else if(cls == "POINT"){

          # Get tile
          tile <- ts[tileName][[overlap]]

          # Crop to tile
          ints <- sf::st_intersects(sf::st_as_sf(tile), in_sf)
          in_sf <- in_sf[unlist(ints),]

        }else{
          stop("Sorry! Only POINT files are currently supported with the 'shp_clip' argument on")
        }
      }

      sf::st_write(
        in_sf,
        gpkg_path,
        append = file.exists(  gpkg_path ),
        quiet = TRUE,
        fid_column_name = "FID"
      )

    }

    # Convert GPKG if necessary
    if(!is_gpkg) gpal2::ogr2ogr("-overwrite", outFile, gpkg_path)


  }else stop("Unrecognized file extension for input RSDS: '", ext, "'")

  .conclusion(tim)

}


.mosaicVRT <- function(tilePaths, ts, overlap){

  overlapTypes <- c("buffs", "tiles", "nbuffs")
  if(!overlap %in% overlapTypes) stop("'overlap' argument should be: ", paste(overlapTypes, collapse = ", "))

  # Create temporary directory
  tempDir <- file.path(tempdir(), "mosaicVRT")
  dir.create(tempDir)
  withr::defer(unlink(tempDir, recursive = TRUE), envir = parent.frame(1))

  # Paths for temporary files
  tempList  <- file.path(tempDir, "tileList.txt")
  tempVRT   <- file.path(tempDir, "mosaic.vrt")

  # Create list of temporary VRT tiles
  tempTiles <- setNames(file.path(tempDir, paste0(names(tilePaths), ".vrt")), names(tilePaths))

  # Create VRT tiles with non-overlapping buffer
  for(tileName in names(tilePaths)){

    tile <- ts[tileName,]

    gpal2::gdalbuildvrt(
      te = raster::extent(tile[[overlap]]),
      tempTiles[tileName],
      tilePaths[tileName]
    )
  }

  # Write list of temporary VRT tiles
  write(tempTiles, tempList)

  # Create mosaic of tiled VRTs
  gpal2::gdalbuildvrt(
    input_file_list = tempList,
    tempVRT
  )

  return(tempVRT)
}
