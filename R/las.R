#' Filter noise from LAS files
#'
#' @export

noise_filter <- function(in_files, out_dir, filter_noise_alg, remove_noise = TRUE){

  process_timer <- .headline("LAS NOISE FILTER")

  if(!all(file.exists(in_files))){
    stop("Could not find all 'in_files'")
  }

  cat("  Filtering files...", "\n")
  pb <- .progbar(length(las_files))

  for(i in 1:length(las_files)){


    las_file <- las_files[i]
    out_file <- file.path(out_dir, basename(las_file))

    if(!file.exists(out_file)){
      las_file %>%
        lidR::readLAS() %>%
        lidR::classify_noise(filter_noise_alg) %>%
        {ifelse(remove_noise ,lidR::filter_poi(., Classification != lidR::LASNOISE), .) } %>%
        lidR::writeLAS(out_file)
    }
    pb$tick()
  }

  .conclusion(process_timer)
}

#' Quality check for LAS files
#'
#' @export

las_qc <- function(las_cat){


  process_timer <- .headline("LAS QUALITY CHECK")

  las_class_nums = list(
    "Unclassified" = lidR::LASUNCLASSIFIED,
    "Ground"    = lidR::LASGROUND,
    "Buildings" = lidR::LASBUILDING,
    "Low Veg"   = lidR::LASLOWVEGETATION,
    "Med Veg"   = lidR::LASMEDIUMVEGETATION,
    "High Veg"  = lidR::LASHIGHVEGETATION,
    "Noise"     = lidR::LASNOISE
  )
  las_class_nchar = max(sapply(names(las_class_nums), nchar))

  # LAS files are valid ----

  file_n <- length(las_cat@data$filename)
  file_status <- list()

  cat("  Reading files...", "\n")
  pb <- .progbar(file_n)

  for(i in 1:file_n){

    filename <- las_cat@data$filename[i]
    res <- tryCatch({
      lidR::readLAS(filename)
      "Valid"
    },
    error = function(e){e$message},
    warning = function(w){w$message}
    )
    file_status[[filename]] <- res

    pb$tick()
  }
  cat("\n")

  files_valid <- sapply(file_status, function(x) x == "Valid")

  cat("  All files valid : ")
  if(all(files_valid)){
    cat(crayon::green("Yes"), "\n")
  }else{
    cat(crayon::red("Following files are invalid\n\n     ",
                    paste(basename(names(files_valid[!files_valid])), collapse = "\n     "), "\n\n",
                    sep=""
    ))
  }

  # LAS Sample ----

  # Read biggest LAS file as a sample
  las_file_biggest <- las_cat@data$filename[which.max(file.size(las_cat@data$filename))]
  las_sample <- lidR::readLAS(las_file_biggest)

  # LAS files have RGB ----

  has_rgb <- all(c("R", "G", "B") %in% names(las_sample))

  cat("  Has RGB         : ")
  if(has_rgb){
    cat(crayon::green("Yes"), "\n")
  }else{
    cat(crayon::yellow("No"), "\n")
  }

  # LAS files are classified ----

  existing_classes <- names(table(las_sample$Classification))

  cat("  Classes\n")
  for(class_name in names(las_class_nums)){
    class_num <- las_class_nums[[class_name]]

    has_class <- class_num %in% existing_classes

    cat("    ", stringr::str_pad(class_name, 12, side = "right"), ": ")
    if(has_class) cat(crayon::green("Yes\n")) else cat(crayon::yellow("No\n"))

  }

  .conclusion(process_timer)
}

#' Coverage of LAS files
#'
#' @export

las_coverage <- function(in_cat, boundary){

  # Get boundary
  boundary <- sf::st_read(boundary, quiet = TRUE)

  # Get LAS geometry
  las_geom <- in_cat@data$geometry

  # Check which tiles intersect project boundary
  intersects_boundary = lengths(sf::st_intersects(las_geom, boundary)) > 0

  # Create SF object
  las_sf <- sf::st_as_sf(
    las_geom,
    data.frame(
      intersects_boundary = intersects_boundary,
      filename = in_cat@data$filename
  ))

  # Dissolve LAS tile coverage
  las_dissolve <- las_sf %>% sf::st_buffer(0.05) %>% sf::st_union()

  # Get portion of boundary that exceeds the LAS coverage
  boundary_exceeds <- sf::st_difference(sf::st_geometry(boundary), las_dissolve)
  boundary_exceeds_area <- as.numeric(sf::st_area(boundary_exceeds))
  if(length(boundary_exceeds_area) == 0) boundary_exceeds_area <- 0

  # Get which LAS tiles are in boundary and which aren't
  las_sf_in_boundary  <- las_sf[las_sf[["intersects_boundary"]],]
  las_sf_out_boundary <- las_sf[!las_sf[["intersects_boundary"]],]

  # Plot
  plot(sf::st_geometry(las_sf_in_boundary), axes = T, border = "blue", col = "lightblue", main = "LAS Data Coverage")
  plot(sf::st_geometry(las_sf_out_boundary), border = "red4", col= "tomato", add = T)
  plot(sf::st_geometry(boundary_exceeds), add = T, col = "orange", border = "darkorange")
  plot(sf::st_geometry(boundary), add = T, border = "purple", lwd=2)

  # Print results
  in_crayon <- if(nrow(las_sf_in_boundary) > 0) crayon::green else crayon::red
  out_crayon <- if(nrow(las_sf_out_boundary) == 0) crayon::green else crayon::red
  exceed_crayon <- if(boundary_exceeds_area == 0) crayon::green else crayon::red

  cat(
    "  Boundary exceeding LAS : ", exceed_crayon(boundary_exceeds_area, "m^2"), "\n",
    "  LAS tiles in boundary  : ", in_crayon(nrow(las_sf_in_boundary)), "\n",
    "  LAS tiles out boundary : ", out_crayon(nrow(las_sf_out_boundary)), "\n",
    "\n",
    "  Returning 'sf' object of unused LAS tiles",
    sep = ""
  )

  return(las_sf_out_boundary)

}


#' Make LAX files for a LAS dataset
#'
#' @export

lax <- function(in_cat){

  # Get initial list of LAS files
  las_files <- in_cat@data[["filename"]]
  if(length(las_files) == 0) stop("Did not find any LAS files in this LAS dataset", call. = FALSE)

  # Get list of LAX files
  lax_files <- gsub("\\.las$|\\.laz$", "\\.lax", las_files)

  # Subset only those LAS files without lax files
  las_files <- las_files[!file.exists(lax_files)]
  if(length(las_files) == 0) stop("All lax files already created", call. = FALSE)

  pb <- .progbar(length(las_files))

  for(las_file in las_files){
    capture.output(rlas::writelax(las_file), type = "message")
    pb$tick()
  }
}


.is_las_ground_classified <- function(inLAS){

  return(!is.null(inLAS$Classification) && lidR:::fast_count_equal(inLAS$Classification, lidR::LASGROUND))
}

.is_las_full_classified <- function(inLAS){

  return(!is.null(inLAS$Classification) && lidR:::fast_count_equal(inLAS$Classification, lidR::LASBUILDING))
}

.is_las_intensity <- function(inLAS){

  return(!is.null(inLAS$Intensity) && lidR:::fast_countover(inLAS$Intensity, 0))
}

.read_las_tile <- function(in_cat, tile, select, classes = NULL){

  # Tile buffer
  buff_sf <- tile[["buffs"]]

  # LAS catalog geometry
  las_grid <- in_cat$geometry

  if(is.na(sf::st_crs(in_cat))) stop("Can't select LAS tiles since this LAS Catalog has no projection info")

  # Reproject grid to tile
  las_grid <- sf::st_transform(las_grid, sf::st_crs(buff_sf))

  # Get intersection between tile and LAS catalog
  las_intrsc <- lengths(sf::st_intersects(las_grid, buff_sf)) > 0

  if(all(!las_intrsc)) return(NULL)

  # Get LAS files
  las_files <- in_cat@data$filename[las_intrsc]

  if(any(!file.exists(las_files))) stop("Missing LAS files")

  # Create extent filter from buffer extent
  buff_xt   <- terra::ext(buff_sf)
  buff_filt <- paste("-keep_xy", buff_xt[1], buff_xt[3], buff_xt[2], buff_xt[4])

  # Create class filter
  class_filt <- if(!is.null(classes)) paste(c("-keep_class", classes), collapse = " ")

  # Read LAS files
  inLAS <- lidR::readLAS(las_files, select = select, filter = paste(buff_filt, class_filt))

  if(lidR::is.empty(inLAS)) return(NULL) else return(inLAS)

}



