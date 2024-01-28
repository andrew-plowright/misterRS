
.metadata <- function(xts, attribute){

  metadata <- .metadata_read(xts)

  if(attribute %in% names(metadata)){

    metadata_attribute <- metadata[[attribute]]

  }else{

    metadata_attribute <- .metadata_calc(xts, attribute)

    metadata[[attribute]] <- metadata_attribute

    .metadata_save(xts, metadata)
  }

  return(metadata_attribute)
}

.metadata_path <- function(xts){

  # Get file path
  metadata_path <- file.path(xts@dir, paste0(rts@id, "_metadata.json"))

  # Get absolute path
  metadata_path <- suppressMessages(R.utils::getAbsolutePath(metadata_path))

  return(metadata_path)
}

.metadata_read <- function(xts){

  metadata_path <- .metadata_path(xts)

  if(file.exists(metadata_path)){
    metadata <- jsonlite::read_json(metadata_path)
  }else{
    metadata <- list()
  }
  return(metadata)
}

.metadata_calc <- function(xts, attribute){
  if(attribute == "range"){
    value <- .rts_range(xts)
  }else{
    stop("Unrecognized metadata attribute '", attribute, "'")
  }
  return(value)
}

.metadata_save <- function(xts, metadata){

  metadata_path <- .metadata_path(xts)

  jsonlite::write_json(metadata, metadata_path, auto_unbox=T)
}

# Compute the raster range of RTS
.rts_range <- function(ts){

  tile_paths <- .rts_tile_paths(ts)

  mm <- lapply(tile_paths, function(path){
    ras <- terra::rast(path)
    terra::minmax(ras)
  })

  nlyrs <- ncol(mm[[1]])

  out_range <- list()

  for(i in 1:nlyrs){

    out_range[[i]] <- list(
      min = min(sapply(mm, function(m) m["min", i]), na.rm=T),
      max = max(sapply(mm, function(m) m["max", i]), na.rm=T)
    )
  }

  return(out_range)
}
