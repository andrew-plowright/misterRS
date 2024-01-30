#' Merge multiple VTS into a single one according to zones
#'
#' @export

merge_vts <- function(in_vts_list, out_vts, zones_path, zone_field,
                    id_field = NULL, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("MERGE")

  ### INPUT CHECKS ----

  for(in_vts in in_vts_list) .complete_input(in_vts)

  # Get file paths

  # Read zones
  zones <- sf::st_read(zones_path, quiet = TRUE)

  if(!all(setdiff(names(in_vts_list), "<none>") %in% unique(zones[[zone_field]]))) stop(
    "Could match list names of 'in_vts' to the values in the '", zone_field,
    "' attribute of '", basename(zones_path), "'")

  ### CREATE WORKER ----

  tile_worker <-function(tile_name){

    # Read in files and merge according to zones
    in_sfs <- lapply(names(in_vts_list), function(zone_name){

      in_vts <- in_vts_list[[zone_name]]

      in_sf <- .vts_read(in_vts, tile_name = tile_name)

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
    if(!is.null(id_field) & nrow(out_sf) > 0){
      out_sf[[id_field]] <- 1:nrow(out_sf)
    }

    .vts_write(out_sf, out_vts = out_vts, tile_name, overwrite=overwrite)

    return("Success")

  }
  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_vts)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)

}
