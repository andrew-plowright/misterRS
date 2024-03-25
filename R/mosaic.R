#' Mosaic a RS Dataset
#'
#' @export

mosaic_rts <- function(in_rts, overlap = "nbuffs", shp_clip = FALSE, out_file = NULL, resample = "average", ...){

  .env_misterRS(list(...))

  process_timer <- .headline("MOSAIC")

  # Check input
  .complete_input(in_rts)

  # Options
  overwrite  <- getOption("misterRS.overwrite")
  tile_names <- getOption("misterRS.tile_names")
  crs        <- getOption("misterRS.crs")

  # Get tiles
  ts <- .tilescheme()

  # Get output file
  if(is.null(out_file)) out_file <- in_rts$mosaic_path()

  if(!dir.exists(dirname(out_file))) stop("Output directory not found")

  if(file.exists(out_file)){
    if(overwrite) unlink(out_file) else stop("Output file already exists")
  }

  # Get input paths
  if(is.null(tile_names)) tile_names <- ts[["tile_name"]]
  in_paths <- in_rts$tile_path(tile_names)

  # Generate VRT mosaic
  cat("  Making VRT", "\n")
  temp_vrt <- .mosaic_vrt(in_paths, ts,  overlap)

  # Merge
  cat("  Merging VRT to TIFF", "\n")
  gpal2::gdal_translate(
    co = c("BIGTIFF=YES", "COMPRESS=LZW"),
    temp_vrt,
    out_file
  )

  if(!file.exists(out_file)) stop("Failed to merge VRT into single mosaic file")

  # Pyramids
  cat("  Generating pyramids", "\n")
  gpal2::gdaladdo(
    r = resample,
    ro = TRUE,
    out_file
  )

  .conclusion(process_timer)
}


.mosaic_vrt <- function(tile_paths, ts, overlap){

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
  temp_tiles <- setNames(file.path(temp_dir, paste0(names(tile_paths), ".vrt")), names(tile_paths))

  # Create VRT tiles with non-overlapping buffer
  for(tile_name in names(tile_paths)){

    tile <- ts[tile_name][[overlap]]

    gpal2::gdalbuildvrt(
      te = terra::ext(tile),
      temp_tiles[tile_name],
      tile_paths[tile_name]
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
