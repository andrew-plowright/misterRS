
.metadata <- function(rsds, attribute){

  metadata <- .metadata_read(rsds)

  if(attribute %in% names(metadata)){

    metadata_attribute <- metadata[[attribute]]

  }else{

    metadata_attribute <- .metadata_calc(rsds, attribute)

    metadata[[attribute]] <- metadata_attribute

    .metadata_save(rsds, metadata)
  }

  return(metadata_attribute)
}

.metadata_read <- function(rsds){

  metadata_path <- .rsds_metadata_path(rsds)

  if(file.exists(metadata_path)){
    metadata <- jsonlite::read_json(metadata_path)
  }else{
    metadata <- list()
  }
  return(metadata)
}

.metadata_calc <- function(rsds, attribute){
  if(attribute == "range"){
    value <- .rsds_range(rsds)
  }else{
    stop("Unrecognized metadata attribute '", attribute, "'")
  }
  return(value)
}

.metadata_save <- function(rsds, metadata){

  metadata_path <- .rsds_metadata_path(rsds)

  jsonlite::write_json(metadata, metadata_path, auto_unbox=T)
}

# Compute the raster range of the RSDS
.rsds_range <- function(rsds){

  tile_paths <- .rsds_tile_paths(rsds)

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
