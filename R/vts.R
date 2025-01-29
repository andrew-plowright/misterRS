
#' Back up Vector tileset
#' @export

vts_backup <- function(in_vts, backup_dir, tag, overwrite = FALSE){

  process_timer <- .headline("VTS BACKUP")

  # Set output file path
  dest_file <- file.path(backup_dir, paste0(in_vts$id, "_", tag, ".tar.gz"))

  cat(
    "  VTS  : ", in_vts$name, "\n",
    "  File : ", dest_file, "\n",
    sep = ""
  )

  # Check if file exists already
  if(file.exists(dest_file)){
    if(overwrite){
      unlink(dest_file)
    }else{
      return(cat(crayon::yellow("File already exists. Set 'overwrite' to TRUE\n")))
    }
  }

  # Compress geopackage
  .compress(in_vts$gpkg, dest_file)

  if(file.exists(dest_file)){
    cat(
      "  Size : ", round(file.info(dest_file)$size / (1024 * 1024),2), " MB", "\n",
      crayon::green("  Backup file created"),"\n",
      sep=""
    )
  }else{
    stop("Failed to create back-up file")
  }

  # Conclude rpocess
  .conclusion(process_timer)
}


#' Count numbers of rows in vectir tileset per tile
#' @export

vts_row_count <- function(in_vts, out_file, overwrite = FALSE){

  process_timer <- .headline("VTS ROW COUNT")

  cat(
    "  VTS  : ", in_vts$name, "\n",
    "  File : ", out_file, "\n",
    sep = "")

  if(file.exists(out_file) & !overwrite){
    return(cat(crayon::yellow("File already exists. Set 'overwrite' to TRUE\n")))
  }

  tiles <- .tilescheme()[["tiles"]]

  con <- in_vts$temp_con()

  tiles[["seg_count"]] <- sapply(tiles[["tile_name"]], function(tile_name){

    DBI::dbGetQuery(con, sprintf("SELECT COUNT(fid) FROM layer layer WHERE tile_name = '%s'", tile_name))[,1]
  })

  sf::st_write(tiles, out_file, quiet = TRUE, delete_dsn = TRUE )

  # Conclude rpocess
  .conclusion(process_timer)
}


