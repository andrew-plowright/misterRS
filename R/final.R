#' @export

final_trees <- function(seg_vts, iteration, class_table, boundary, out_file, boundary_buff = 0){

  process_timer <- .headline("FINAL TREES")

  .env_misterRS(list(tile_names = NULL, overwrite = TRUE))

  ### INPUT CHECKS ----

  # Iteration label
  class_label <- paste0("class_", iteration)

  # Check that inputs are complete
  .complete_input(seg_vts, attribute = class_label)

    # Tile names
  ts <- .tilescheme()

  # Read boundary
  boundary_sp      <- sf::st_read(boundary, quiet = TRUE)
  boundary_sp$geom <- suppressPackageStartupMessages(lwgeom::lwgeom_make_valid(boundary_sp$geom))
  boundary_sp      <- sf::st_transform(boundary_sp, getOption("misterRS.crs"))

  ### CREATE WORKER ----

  pb <- .progbar(length( ts[["tile_name"]]))

  for(tile_name in ts[["tile_name"]]){

    # File paths
    polys <- seg_vts$read_tile(tile_name = tile_name, fields = c("height", "crown_area", class_label, "geom"))

    if(nrow(polys) == 0){
      pb$tick()
      next
    }

    # Correct geometry
    is_valid <- sf::st_is_valid(polys)
    if(any(!is_valid)) polys <- sf::st_make_valid(polys)

    # Intersection with boundary
    ints <- sf::st_intersects(boundary_sp, polys)
    polys <- polys[unique(unlist(ints)),]

    # Reclass
    polys[["class"]] <- class_table[["label"]][match(polys[[class_label]], class_table[,'id'])]

    # Remove NA
    polys <- polys[!is.na(polys[["class"]]),]

    if(nrow(polys) == 0){
      pb$tick()
      next
    }

    # Remove non-classified
    polys <- polys[,c("height", "crown_area", "class")]

    # Write output
    sf::st_write(
      polys,
      out_file,
      append = file.exists(out_file),
      quiet = TRUE,
      fid_column_name = "FID"
    )

    pb$tick()
  }

  # Conclude
  .conclusion(process_timer)
}



#' Final Canopy
#'
#' Applies the following process:
#' \enumerate{
#' \item Clip to boundary
#' \item Remove any 'NotTree' classes from canopy
#' \item Merge into final TIF file
#' }
#'
#' @export


final_canopy <- function(seg_class_rts, class_table, boundary, out_file, boundary_buff = 0){

  .env_misterRS(list(tile_names = NULL, overwrite = TRUE))

  process_timer <- .headline("FINAL CANOPY")

  ### INPUT CHECKS ----

  cat("  - Clip to boundary\n  - Remove non-tree classes\n  - Merge into single binary mask\n\n")

  # Check that inputs are complete
  .complete_input(seg_class_rts)

  # Tile names
  ts <- .tilescheme()

  # Create temporary directories
  dirs <- list(
    temproot     = file.path(tempdir(), "final_canopy"),
    boundary_tif = file.path(tempdir(), "final_canopy", "boundary_tif"),
    canopy       = file.path(tempdir(), "final_canopy", "canopy")
  )
  for(dir in dirs) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(dirs$temproot, recursive = TRUE))

  out_paths  <- setNames(file.path(dirs$canopy, paste0(ts[["tile_name"]], ".tif")),ts[["tile_name"]])
  boundary_mask_path <- file.path(dirs$temproot, "boundary_mask.shp")

  ### BOUNDARY ----

  # Read boundary
  boundary_sp      <- sf::st_read(boundary, quiet = TRUE)
  boundary_sp$geom <- suppressPackageStartupMessages(lwgeom::lwgeom_make_valid(boundary_sp$geom))
  boundary_sp      <- sf::st_transform(boundary_sp, getOption("misterRS.crs"))

  # Buffer
  if(boundary_buff > 0) boundary_sp <- sf::st_buffer(boundary_sp, boundary_buff)

  # Give boundary a masking value
  boundary_sp[["Mask"]] <- 1

  # Save
  sf::st_write(boundary_sp[,c("Mask")], boundary_mask_path, quiet = TRUE)

  ### CREATE WORKER ----

  pb <- .progbar(length( ts[["tile_name"]]))

  for(tile_name in ts[["tile_name"]]){

    # File paths
    out_path          <- out_paths[tile_name]
    boundary_tif_path <- file.path(dirs$boundary_tif, paste0(tile_name, ".tif"))

    # Read seg class raster
    trees_class_ras <-  seg_class_rts$tile_path(tile_name) %>% terra::rast()

    # Rasterize asset outline
    gpal2::gdal_rasterize(
      a = "Mask",
      a_nodata = 0,
      co = c("COMPRESS=LZW"),
      te = terra::ext(trees_class_ras),
      tr = terra::res(trees_class_ras),
      ot = "UInt16",
      boundary_mask_path,
      boundary_tif_path
    )

    # Read boundary mask tile
    boundary_mask <- terra::rast(boundary_tif_path)

    # Apply boundary
    trees_class_ras[is.na(boundary_mask)] <- NA

    # Get table for reclassification
    canopy_table <- class_table %>% subset(is_tree, c("id","is_tree"))

    # Reclassify to binary tree vs non-tree
    canopy_class_ras <- terra::classify(trees_class_ras, canopy_table, others = NA)

    # Write file
    terra::writeRaster(canopy_class_ras, out_path, datatype = "INT1U")

    pb$tick()

  }


  ### MERGE ----

  tempVRT <- .mosaic_vrt(out_paths, ts, overlap = "buffs" )

  gpal2::gdal_translate(
    co = c("BIGTIFF=YES", "COMPRESS=LZW"),
    tempVRT,
    R.utils::getAbsolutePath(out_file)
  )

  gpal2::gdaladdo(
    r = "average",
    ro = TRUE,
    R.utils::getAbsolutePath(out_file)
  )

  # Conclude
  .conclusion(process_timer)

}
