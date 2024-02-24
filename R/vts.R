
#' Vetor tileset (class)
#' @export

setClass(
  "vts",
  representation(
    id       = 'character',
    name     = 'character',
    dir      = 'character',
    gpkg     = 'character',
    tile_reg = 'character'
  )
)

setMethod("show", "vts", function(object){

  ts <- .tilescheme()
  total_tiles <- length(ts$tileName)

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = object@tile_reg)
  withr::defer(DBI::dbDisconnect(con))

  tile_reg <- DBI::dbReadTable(con, "tile_reg")

  cat(
    "REMOTE SENSING DATASET", "\n",
    "ID        : ", object@id ,  "\n",
    "Name      : ", object@name, "\n",
    "Dir       : ", object@dir,  "\n",
    "Tiles     : ", nrow(tile_reg), "/", total_tiles, "\n",
    sep = ""
  )

  attr_count <- NULL
  attrs <- setdiff(names(tile_reg), "tile_name")
  for(attr in attrs){
    cat( "  ",  stringr::str_pad(attr, 8, "right"), ": ",  nrow(tile_reg[tile_reg[, attr] %in% 1,]),  "/", total_tiles, "\n", sep = "")
  }

})


#' Vector tileset (constructor)
#' @export

vts <- function(id, name, dir, gpkg, proj = getOption("misterRS.crs")){

  # Create folder
  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  # Set GeoPackage path
  vts_path      <- file.path(dir, paste0(id, ".gpkg"))
  tile_reg_path <- file.path(dir, paste0(id, "-tile_reg.sqlite"))

  # Create GeoPackage
  if(!file.exists(vts_path)){

    blank_sf <- sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs( proj)), list(tile_name = character()))

    sf::st_write(blank_sf, vts_path, quiet = TRUE, layer = "empty")
  }

  # Create tile registry
  if(!file.exists(tile_reg_path)){

    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = tile_reg_path)
    withr::defer(DBI::dbDisconnect(con))

    DBI::dbExecute(con, "CREATE TABLE tile_reg (tile_name varchar(50) NOT NULL, UNIQUE(tile_name))")
  }

  # Create new object
  new("vts", id = id, name = name, dir = dir, gpkg = vts_path, tile_reg = tile_reg_path)
}


#' Back up Vector tileset
#' @export

vts_backup <- function(in_vts, backup_dir, tag, overwrite = FALSE){

  process_timer <- .headline("BACKUP VTS")

  # Set output file path
  dest_file <- file.path(backup_dir, paste0(in_vts@id, "_", tag, ".rar"))

  cat(
    "  VTS  : ", in_vts@name, "\n",
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

  # Source files
  src_files <-  c(in_vts@gpkg, in_vts@tile_reg)

  # Compress files
  .rar(src_files, dest_file)

  if(!file.exists(dest_file)) stop("Failed to create back-up file")

  # Conclude rpocess
  .conclusion(process_timer)
}


#' Count numbers of rows in vectir tileset per tile
#' @export

vts_row_count <- function(in_vts, out_file, overwrite = FALSE){

  cat(
    "VTS: COUNT ROWS\n",
    "  VTS  : ", in_vts@name, "\n",
    "  File : ", out_file, "\n",
    sep = "")

  if(file.exists(out_file) & !overwrite){
    return(cat(crayon::yellow("File already exists. Set 'overwrite' to TRUE\n")))
  }

  ts <- sf::st_as_sf( misterRS:::.tilescheme()[["tiles"]])

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = in_vts@gpkg)

  withr::defer(  DBI::dbDisconnect(con))

  ts[["seg_count"]] <- sapply(ts$tileName, function(tile_name){

    DBI::dbGetQuery(con, sprintf("SELECT COUNT(fid) FROM layer layer WHERE tile_name = '%s'", tile_name))[,1]
  })

  sf::st_write(ts, out_file, quiet = TRUE, delete_dsn = TRUE )
}


#' Clear tileset reg attributes
#'

vts_clear_attribute_set <- function(in_vts, attribute_set, tile_name = NULL){

  cat(
    "VTS: CLEAR ATTRIBUTE SET\n",
    "  VTS           : ", in_vts@name, "\n",
    "  Attribute set : ", attribute_set, "\n",
    "  Tile subset   : ", !is.null(tile_name), "\n",
    sep = "")

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname =  in_vts@tile_reg)

  vts_fields <- DBI::dbListFields(con, "tile_reg")

  if(!attribute_set %in% vts_fields) stop("Attribute set '", attribute_set,"' not found in VTS")

  sql_query <- sprintf("SELECT tile_name, %s FROM tile_reg", attribute_set)
  if(tile_name){
    sql_query <- paste(sql_query, sprintf("WHERE tile_name IN ('%s')", paste(tile_name, collapse= "', '")))
  }

  current_tiles <- DBI::dbGetQuery(con, sql_query)

  # How many will change
  will_change <- nrow(current_tiles[current_tiles[[attribute_set]] %in% 1  ,])

  cat("  Clear ", will_change, " tiles for attribute set '", attribute_set ,"'?\n", sep ="")
  answer <- tolower(readline("  y/n"))

  if(answer == "y"){

    sql_update <- sprintf("UPDATE tile_reg SET %s=null", attribute_set)
    if(tile_name){
      sql_update <- paste(sql_update, sprintf("WHERE tile_name IN ('%s')", paste(tile_name, collapse= "', '")))
    }

    DBI::dbExecute(con, sql_update)
    cat("  Cleared ", will_change, " tiles\n", sep ="")
  }

}




.vts_write <- function(in_sf, out_vts, tile_name, overwrite = FALSE){

  if(overwrite){

    .vts_tile_delete(out_vts, tile_name)

  }else{

    has_tile <- .vts_has_tiles(out_vts, tile_name)
    if(has_tile) stop("Tile already exists")
  }

  # Write geometry
  if(nrow(in_sf) > 0){

    in_sf[["tile_name"]] <- tile_name

    sf::st_write(in_sf, out_vts@gpkg, layer="layer", append = TRUE, quiet=TRUE)

  }

  # Write to tile registry
  .vts_tile_reg_add(out_vts, tile_name)
}


# .vts_write_attribute_set <- function(data, out_vts, id_field, attribute_set_field, tile_name){
#
#   # Insert data (if there is any)
#   if(!is.null(data) && nrow(data) > 0){
#
#     # Create expression for inserting tabular data
#     data_fields <- paste(names(data), collapse = ", ")
#
#     values <- paste(sapply(1:nrow(data), function(i) paste0("(", paste(data[i,], collapse=", "), ")")  ), collapse=", ")
#
#     field_mapping <- paste(sapply(setdiff(names(data), id_field), function(field){
#       sprintf("%s = (SELECT %s FROM Tmp WHERE layer.%s = Tmp.%s)", field, field, id_field, id_field)
#     }), collapse = ", ")
#
#     insert_sql <- sprintf(
#       "WITH Tmp(%s) AS (VALUES%s) UPDATE layer SET %s WHERE %s IN (SELECT %s FROM Tmp) AND tile_name = '%s';",
#       data_fields, values, field_mapping, id_field, id_field, tile_name
#     )
#
#     # Set path to Spatialite DLL
#     withr::with_envvar(list(PATH = getOption("misterRS.mod_spatialite")), {
#
#       con <- DBI::dbConnect(RSQLite::SQLite(), dbname = out_vts@gpkg)
#
#       DBI::dbExecute(con, "SELECT load_extension('mod_spatialite')")
#       DBI::dbExecute(con, insert_sql)
#
#       DBI::dbDisconnect(con)
#     })
#
#   }
#
#   # Mark this tile as complete
#   con <- DBI::dbConnect(RSQLite::SQLite(), dbname = out_vts@tile_reg)
#
#   DBI::dbExecute(con, sprintf("UPDATE tile_reg SET %s = TRUE where tile_name = '%s'", attribute_set_field, tile_name))
#
#   DBI::dbDisconnect(con)
# }


.vts_write_attribute_set <- function(data, out_vts, id_field, attribute_set_field, tile_name){

  # Insert data (if there is any)
  if(!is.null(data) && nrow(data) > 0){


    # Get rows that will be deleted
    delete_rows <- .vts_read(out_vts, tile_name = tile_name, field = "fid")[,"fid"]

    # Write data
    sf::st_write(data, out_vts@gpkg, layer = "layer", append = TRUE, quiet = TRUE)

    # Delete old layer
    con_gpkg <- DBI::dbConnect(RSQLite::SQLite(), dbname = out_vts@gpkg)
    withr::defer( DBI::dbDisconnect(con_gpkg))
    DBI::dbExecute(con_gpkg, sprintf("DELETE FROM layer WHERE fid IN (%s)", paste(delete_rows, collapse= ",")))
  }

  # Mark this tile as complete
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = out_vts@tile_reg)
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbExecute(con, sprintf("UPDATE tile_reg SET %s = TRUE where tile_name = '%s'", attribute_set_field, tile_name))
}


.vts_tile_delete <- function(in_vts, tile_name){

  # Delete tiles from tile registry
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = in_vts@tile_reg)
  DBI::dbExecute(con, sprintf("DELETE FROM tile_reg WHERE tile_name = '%s'", tile_name))
  DBI::dbDisconnect(con)

  # Delete geometry
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = in_vts@gpkg)
  if("layer"%in% DBI::dbListTables(con)){
    DBI::dbExecute(con, sprintf("DELETE FROM layer WHERE tile_name = '%s'",    tile_name))
  }
  DBI::dbDisconnect(con)
}

# Insert a new tile row to the register
.vts_tile_reg_add <- function(in_vts, tile_name){

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = in_vts@tile_reg)
  DBI::dbExecute(con, sprintf("INSERT INTO tile_reg (tile_name) VALUES ('%s')", tile_name))
  DBI::dbDisconnect(con)
}

# Add the attribute set column to the register
.vts_tile_reg_attribute_set <- function(in_vts, attribute_set_name){

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = in_vts@tile_reg)

  if(!attribute_set_name %in% DBI::dbListFields(con, "tile_reg")){

    sql <- paste("ALTER TABLE tile_reg ADD", attribute_set_name, "boolean;")

    DBI::dbExecute(con, sql)
  }

  DBI::dbDisconnect(con)
}


.vts_read <- function(in_vts, tile_name = NULL, geom = NULL, field = NULL){

  if(is.null(tile_name) & is.null(geom)) stop("Subset VTS either by tile_name or by geometry")

  # by tilename
  if(!is.null(tile_name)){

    if(is.null(field)){
      output <- sf::st_read(in_vts@gpkg, quiet = TRUE, query = sprintf("SELECT * FROM layer WHERE tile_name = '%s'", tile_name))

    }else{

      if("geom" %in% field){
        output <- sf::st_read(in_vts@gpkg, quiet = TRUE, query = sprintf("SELECT %s FROM layer WHERE tile_name = '%s'", paste(field, collapse=","), tile_name))
      }else{
        con <- DBI::dbConnect(RSQLite::SQLite(), dbname = in_vts@gpkg)
        output <- DBI::dbGetQuery(con, sprintf("SELECT %s FROM layer layer WHERE tile_name = '%s'", paste(field, collapse=",") ,tile_name))
        DBI::dbDisconnect(con)
      }

    }

  }else if(!is.null(geom)){

    if(!is.null(field)) stop("Subsetting specific field not implemented")

    bbox_wkt <- sf::st_as_text(sf::st_geometry(geom))

    output <- sf::st_read(in_vts@gpkg, layer="layer", quiet = TRUE, wkt_filter = bbox_wkt)
  }

  return(output)
}


.vts_has_tiles <- function(in_vts, tile_names, attribute_set_name = NULL){

  if(is.null(tile_names) | length(tile_names) == 0){

    return(character())

  }else{

    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = in_vts@tile_reg)

    # Get tiles that are registered
    if(is.null(attribute_set_name)){
      sql_query <- "SELECT tile_name FROM tile_reg"

      # Get tiles that are registered AND which have a valid attribute set
    }else{
      sql_query <- sprintf("SELECT tile_name FROM tile_reg WHERE %s = 1", attribute_set_name)
    }

    existing_tiles <- DBI::dbGetQuery(con, sql_query)[,1]

    DBI::dbDisconnect(con)

    return(setNames(tile_names %in% existing_tiles, tile_names))
  }
}

.vts_has_output_layer <- function(in_vts){
  "layer" %in% sf::st_layers(in_vts@gpkg)$name
}

.vts_add_fields <- function(in_vts, fields, field_type = "MEDIUMINT"){

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = in_vts@gpkg)

  existing_fields <- DBI::dbListFields(con, "layer")

  missing_fields <- fields[!fields %in% existing_fields]

  for(missing_field in missing_fields){
    sql <- paste("ALTER TABLE layer ADD", missing_field, field_type, ";")

    DBI::dbExecute(con, sql)
  }

  DBI::dbDisconnect(con)
}

.vts_create_index <- function(in_vts, col_name, table_name = "layer"){

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = in_vts@gpkg)
  withr::defer(DBI::dbDisconnect(con))

  exists <- DBI::dbGetQuery(con, sprintf("SELECT * FROM sqlite_master WHERE type = 'index' and tbl_name = '%1$s' and name = '%1$s_%2$s'", table_name, col_name))

  if(nrow(exists) == 0){
    DBI::dbExecute(con, sprintf("CREATE INDEX %1$s_%2$s ON %1$s (%2$s)", table_name, col_name))
  }
}

