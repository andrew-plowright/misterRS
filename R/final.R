#' @export

final_trees <- function(seg_vts,iteration,class_table, boundary, out_file,
                        boundary_buff = 0){

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
  boundary_sp <- sf::st_read(boundary, quiet = TRUE)
  boundary_sp$geom <- suppressPackageStartupMessages(lwgeom::lwgeom_make_valid(boundary_sp$geom))
  boundary_sp <- sf::st_transform(boundary_sp, getOption("misterRS.crs"))

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

