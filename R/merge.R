#' Merge multiple VTS into a single one according to zones
#'
#' @export

merge_vts <- function(in_vts, out_vts, zones_path, zone_field,
                    id_field = NULL, ...){

  .env_misterRS(list(...))

  process_timer <- .headline("MERGE")

  ### INPUT CHECKS ----

  for(in_vts_i in in_vts) .check_complete_input(in_vts_i)

  # Get tiles
  #ts <- .get_tilescheme()

  # Get file paths
  out_files  <- .rts_tile_paths(out_vts)
  in_files <- lapply(in_vts, function(in_vts_i){.rts_tile_paths(in_vts_i)})

  # Read zones
  zones <- sf::st_read(zones_path, quiet = TRUE)

  if(!all(setdiff(names(in_vts), "<none>") %in% unique(zones[[zone_field]]))) stop(
    "Could match list names of 'in_vts' to the values in the '", zone_field,
    "' attribute of '", basename(zones_path), "'")

  ### CREATE WORKER ----

  tile_worker <-function(tile_name){

    # Output file
    out_file  <- out_files[tile_name]

    # Read in files and merge according to zones
    in_sps <- lapply(names(in_files), function(zone_name){

      in_file <- in_files[[zone_name]][tile_name]

      in_sp <- sf::st_read(in_file, quiet = TRUE)

      # Intersect with no zone
      if(zone_name == "<none>"){

        intersec <- sapply(sf::st_intersects(in_sp, zones), function(x) if(length(x) >0) FALSE else TRUE)

      # Intersect with a given zone
      }else{

        zone <- zones[zones[[zone_field]] == zone_name, ]

        intersec <- sapply(sf::st_intersects(in_sp, zone), function(x) if(length(x) >0) TRUE else FALSE)
      }

      if(length(intersec) > 0 && any(intersec)){
        return(in_sp[intersec,])
      }else{
        in_sp[c(),]
      }
    })
    out_sp <- do.call(dplyr::bind_rows, in_sps)

    # Create new field ID
    if(!is.null(id_field) & nrow(out_sp) > 0){
      out_sp[[id_field]] <- 1:nrow(out_sp)
    }


    sf::st_write(out_sp, out_file, quiet=TRUE, delete_dsn = overwrite)

    if(file.exists(out_file)){
      return("Success")
    }else{
      stop("Failed to create tile")
    }
  }
  ### APPLY WORKER ----

  # Get tiles for processing
  queued_tiles <- .tile_queue(out_files)

  # Process
  process_status <- .exe_tile_worker(queued_tiles, tile_worker)

  # Report
  .print_process_status(process_status)

  # Conclude
  .conclusion(process_timer)

}
