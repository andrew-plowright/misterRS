#' Zip a RSDS by chunks
#'
#' @export

zip_chunks <- function(in_rsds, out_dir, prefix, chunk_size = 100){

  out_dir <- tools::file_path_as_absolute(out_dir)

  tile_paths <- .get_rsds_tilepaths(in_rsds)

  chunks <- split(tile_paths, ceiling(seq_along(tile_paths)/chunk_size))
  n_chunks <- length(chunks)

  cat("ZIP CHUNKS", "\n\n",
      "  Chunk size : ", chunk_size,"\n",
      "  Chunk no.  : ", n_chunks,
      "\n\n", sep=""
      )

  for(i in 1:n_chunks){

    cat("  Zipping chunk", i, "\n", sep="")

    out_file <- file.path(out_dir, paste0(prefix, i, ".zip"))

    zip(
      zipfile = out_file,
      files   = chunks[[i]],
      extras  = "-j -q")
  }

  cat("\n", "Finished", "\n", sep="")
}
