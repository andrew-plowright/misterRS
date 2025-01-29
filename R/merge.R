#' Merge multiple VTS into a single one according to zones
#'
#' The geographical bounds of zones are defined in \code{zone}. The name of each zone is identified in the \code{zone_field}. The contents of the
#' \code{in_vts_list} are merged according to their intersection with the zones. If a vts is identified as "<none>", it will be selected where
#' no zones are present.
#'
#' @param zones a sf class MultiPolygon (or a file path). Should contain an attribute named \code{zone_field}
#' @param zone_field character. Name of attribute which identifies the name of each zone
#' @param in_vts_list named list of vts. The names should correspond to either 1) "<none>" or 2) values in the \code{zone_field} of the \code{zones}
#' @export

merge_vts <- function(in_vts_list, out_vts, zones, zone_field, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("MERGE")

  ### INPUT CHECKS ----

  for(in_vts in in_vts_list) .complete_input(in_vts, attribute = 'geom')

  # Get file paths

  # Read zones
  if(is.character(zones)) zones <- sf::st_read(zones, quiet = TRUE)

  # Check zones
  if(!'sf' %in% class(zones)) stop("Input 'zones' should be of class 'sf'")

  if(!sf::st_geometry_type(zones) %in% c("MULTIPOLYGON", "POLYGON")) stop("Input 'zones' should be polygonal")

  acceptable_zones <- c("<none>", unique(zones[[zone_field]]))
  if(!all(names(in_vts_list) %in% acceptable_zones)) stop("Could match list names of 'in_vts_list' to the values in the '", zone_field, "' attribute of 'zones'")

  # Create tile directory
  out_vts$temp_tile_dir_create("geom")

  ### CREATE WORKER ----

  tile_worker <-function(tile_name){

    # Read in files and merge according to zones
    in_sfs <- lapply(names(in_vts_list), function(zone_name){

      in_vts <- in_vts_list[[zone_name]]

      in_sf <- in_vts$read_tile(tile_name)

      # Intersect with no zone
      if(zone_name == "<none>"){

        intersec <- sapply(sf::st_intersects(in_sf, zones), function(x) if(length(x) >0) FALSE else TRUE)

      # Intersect with a given zone
      }else{

        zone <- zones[zones[[zone_field]] == zone_name, ]

        intersec <- sapply(sf::st_intersects(in_sf, zone), function(x) if(length(x) >0) TRUE else FALSE)
      }

      if(length(intersec) > 0 && any(intersec)){
        return(in_sf[intersec,])
      }else{
        return(in_sf[c(),])
      }
    })

    out_sf <- do.call(dplyr::bind_rows, in_sfs)

    # Create new field ID
    if(nrow(out_sf) > 0) out_sf[[in_vts$id_field]] <- 1:nrow(out_sf)

    # Output file
    out_file <- out_vts$temp_tile_path(tile_name, "geom", "gpkg")

    # Write output
    sf::st_write(out_sf, out_file, quiet=TRUE)

    return("Success")

  }

  ### APPLY WORKER ----
  out_vts %>%
    .tile_queue(attribute_set_name="geom") %>%
    .exe_tile_worker(tile_worker) %>%
    .print_process_status()

  ### ABSORB TEMP FILES ----
  absorb_tiles <- out_vts$temp_absorb_queue("geom", "geom", "gpkg")

  for(tile_name in names(absorb_tiles)){

    data <- sf::st_read(absorb_tiles[tile_name], quiet=TRUE)
    out_vts$append_geom(data = data, tile_name = tile_name)
  }

  # Create index
  out_vts$index()

  # Conclude
  .conclusion(process_timer)

}
