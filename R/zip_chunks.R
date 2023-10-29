#' Zip a RSDS by chunks
#'
#' @export

zip_chunks <- function(in_rsds, out_dir, prefix, overlap = "buffs", chunk_size = 25){

  out_dir <- tools::file_path_as_absolute(out_dir)

  in_tile_paths <- .get_rsds_tilepaths(in_rsds)

  cat("ZIP CHUNKS", "\n\n",
      "  Chunk size : ", chunk_size,"\n",
      "  Chunk no.  : ", ceiling(length(in_tile_paths) / chunk_size),
      "\n\n", sep=""
  )

  # Get tiles
  ts <- .get_tilescheme()

  if(!overlap %in% c("buffs", "nbuffs", "tiles")) stop("Invalid input for 'overlap'")

  if(in_rsds@ext != "tif") stop("Only raster RSDS are currently supported")

  # Crop tiles if needed
  zip_tile_paths <- if(overlap == "buffs"){

    # No cropping needed for "buffs"
    in_tile_paths

  }else if(overlap %in% c("nbuffs", "tiles")){

    cat("  Overlap setting: '", overlap, "'\n", "  Cropping tiles...", "\n", sep= "")

    # Get tiles
    ts <- .get_tilescheme()

    # Create temporary directory
    temp_dir <- file.path(tempdir(), "zip_chunks")
    dir.create(temp_dir, showWarnings = FALSE)
    withr::defer(unlink(temp_dir, recursive = TRUE), envir = parent.frame(1))

    tile_names <- setNames(names(in_tile_paths), names(in_tile_paths))

    sapply(tile_names, function(tile_name){

      ext <- raster::extent(ts[tile_name][[overlap]])

      out_file <- file.path(temp_dir, paste0(tile_name, ".tif"))

      # Crop to selected extent
      gpal2::gdalwarp(
        te = ext,
        in_tile_paths[tile_name],
        out_file
      )

      out_file
    })
  }

  chunks <- split(zip_tile_paths, ceiling(seq_along(zip_tile_paths)/chunk_size))
  n_chunks <- length(chunks)

  for(i in 1:n_chunks){

    cat("  Zipping chunk ", i, " of ", n_chunks, "\n", sep="")

    out_file <- file.path(out_dir, paste0(prefix, i, "of", n_chunks, ".zip"))

    zip(
      zipfile = out_file,
      files   = chunks[[i]],
      extras  = "-j -q")
  }

  cat("\n", "Finished", "\n", sep="")
}
