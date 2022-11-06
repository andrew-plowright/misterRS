#' Zip a RSDS by chunks
#'
#' @export

zip_chunks <- function(rsds, out_dir, prefix, chunk_size = 100){

  out_dir <- tools::file_path_as_absolute(out_dir)

  chunks <- split(rsds@tilePaths, ceiling(seq_along(rsds@tilePaths)/chunk_size))

  for(i in 1:length(chunks)){

    out_file <- file.path(out_dir, paste0(prefix, i, ".zip"))

    zip(
      zipfile = out_file,
      files   = chunks[[i]],
      extras  = "-j -q")
  }
}
