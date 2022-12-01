#' Mosaic a RS Dataset
#'
#' @export

mosaic <- function(rsds, overlap = "nbuffs", shp_clip = FALSE, outFile = NULL, overwrite = FALSE, tile_names = NULL){

  process_timer <- .headline("MOSAIC")

  # Get tiles
  ts <- .get_tilescheme()

  # Get output file
  if(is.null(outFile)) outFile <- .get_rsds_mosaicpath(rsds)

  if(!dir.exists(dirname(outFile))) stop("Output directory not found")

  if(file.exists(outFile)){
    if(overwrite) unlink(outFile) else stop("Output file already exists")
  }

  # # Get CRS
  crs <- getOption("misterRS.crs")

  # Get extension of input RSDS
  ext <- rsds@ext

  # Check input
  .check_complete_input(rsds, tile_names = tile_names)

  # Get input paths
  in_paths <- .get_rsds_tilepaths(rsds)
  if(!is.null(tile_names)) in_paths <- in_paths[tile_names]

  # Merge rasters
  if(ext %in% c("tif")){

    # Generate VRT mosaic
    cat("  Making VRT", "\n")
    temp_vrt <- .mosaic_vrt(in_paths, ts,  overlap)

    # Merge
    cat("  Merging VRT to TIFF", "\n")
    gpal2::gdal_translate(
      co = c("BIGTIFF=YES", "COMPRESS=LZW"),
      temp_vrt,
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
    for(tile_name in names(in_paths)){

      # Get SHP file path
      in_path <- in_paths[tile_name]

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
          tile <- ts[tile_name][[overlap]]

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

  .conclusion(process_timer)

}


.mosaic_vrt <- function(tilePaths, ts, overlap){

  overlap_types <- c("buffs", "tiles", "nbuffs")
  if(!overlap %in% overlap_types) stop("'overlap' argument should be: ", paste(overlap_types, collapse = ", "))

  # Create temporary directory
  temp_dir <- file.path(tempdir(), "mosaicVRT")
  dir.create(temp_dir, showWarnings = FALSE)
  withr::defer(unlink(temp_dir, recursive = TRUE), envir = parent.frame(1))

  # Paths for temporary files
  temp_list  <- file.path(temp_dir, "tileList.txt")
  temp_vrt   <- file.path(temp_dir, "mosaic.vrt")

  # Create list of temporary VRT tiles
  temp_tiles <- setNames(file.path(temp_dir, paste0(names(tilePaths), ".vrt")), names(tilePaths))

  # Create VRT tiles with non-overlapping buffer
  for(tile_name in names(tilePaths)){

    tile <- ts[tile_name,]

    gpal2::gdalbuildvrt(
      te = raster::extent(tile[[overlap]]),
      temp_tiles[tile_name],
      tilePaths[tile_name]
    )
  }

  # Write list of temporary VRT tiles
  write(temp_tiles, temp_list)

  # Create mosaic of tiled VRTs
  gpal2::gdalbuildvrt(
    input_file_list = temp_list,
    temp_vrt
  )

  return(temp_vrt)
}
