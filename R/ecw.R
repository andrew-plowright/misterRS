ecw_to_tiff <- function(ecw_file, out_file, extent = NULL, overwrite = TRUE){

  timer <- .headline("ECW to TIFF")

 ## EXTRACT INFO FROM FILE ----

  # Get info
  ras_info <- gpal2::gdalinfo(ecw_file)

  # Get crs
  epsgs <- ras_info[grepl("EPSG", ras_info)]

  epsg_spc_counts <- stringr::str_count(epsgs, " ")
  epsg_i <- which(epsg_spc_counts == 4)
  if(length(epsg_i) != 1) stop("Found multiple or no adequate EPSG strings")

  epsg_num <- epsgs[epsg_i] %>%
    strsplit(",") %>%
    `[[`(1) %>%
    `[`(2) %>%
    stringr::str_extract("\\d+") %>%
    as.integer

  ras_crs <- terra::crs(paste0("epsg:", epsg_num))

  # Get extent
  ul <- ras_info[grepl("Upper Left", ras_info)]
  lr <- ras_info[grepl("Lower Right", ras_info)]

  ras_ext <- terra::ext(
    c(
      .extract_coord_grepl(ul),
      .extract_coord_grepl(lr)
    )[c(1,3,4,2)]
  )

  # Get resolution
  ras_res <- ras_info[grepl("Pixel Size", ras_info)] %>%
    .extract_coord_grepl %>%
    abs

  # Create raster
  ras <- terra::rast(extent = ras_ext, resolution = ras_res)

  cat(
    "  CRS        : EPSG ", epsg_num, "\n",
    "  Extent     : ", paste(as.vector( ras_ext), collapse=", "), " (xmin, xmax, ymin, ymax)\n" ,
    "  Resolution : ", ras_res[1], ", ", ras_res[2], "\n",
    sep=""
  )



  ## CROP TO EXTENT ----
  if(!is.null(extent)){

    ras <- terra::crop(ras, extent, snap = "out")
    ras_ext <- terra::ext(ras)

  }

  ## WARP ----

  if(overwrite & file.exists(out_file)) unlink(out_file)

  msg <- gpal2::gdalwarp(
    te        = ras_ext,
    tr        = ras_res,
    overwrite = FALSE,
    ecw_file,
    out_file

  )

  if(file.exists(out_file)){
    cat(crayon::green("  Success\n"))
  }else{
    cat(msg)
    cat(crayon::red("  Failure\n"))
  }

  ## CONCLUDE ----
  .conclusion(timer)
}

.extract_coord_grepl <- function(x){

 x %>%
    stringr::str_match("\\((.*?)\\)")[,2] %>%
    strsplit(",") %>%
    `[[`(1) %>%
    as.numeric()
}


