#' @title Generic tileset
#' @description Generic tileset.
#' @export
xts = R6::R6Class("xts",

  public = list(

    #' @description Initialize XTS.
    #' @param id ID of XTS.
    #' @param name Name of XTS.
    #' @param dir Path to directory in which files will be created.
    initialize = function(id, name, dir){

      stopifnot(is.character(id), length(id) == 1)
      stopifnot(is.character(name), length(name) == 1)
      stopifnot(is.character(dir), length(dir) == 1)

      private$name <- name
      private$id <- id
      private$dir <- dir

      # Create folder
      if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    }
  ),

  private = list(
    id   = NA_character_,
    name = NA_character_,
    dir  = NA_character_
  )
)

#' @title Vector tileset
#' @description Vector tileset.
#' @export
vts = R6::R6Class("vts",

  inherit = xts,

  public = list(

    #' @description Initialize VTS.
    #' @param id ID of VTS.
    #' @param name Name of VTS.
    #' @param dir Path to directory in which files will be created.
    #' @param geom_type Type of geometry (ex.: POLYGON, POINT).
    #' @param geom_layer Name of layer for geometry.
    #' @param proj EPSG number.
    #' @param ts Tileset.
    initialize = function(id, name, dir, geom_type, geom_layer = "layer", proj = getOption("misterRS.crs"), ts = getOption("misterRS.ts")) {

      super$initialize(id, name, dir)

      # Name of geometry layer
      private$geom_layer <- geom_layer

      # Set GeoPackage path
      private$gpkg <- file.path(dir, paste0(id, ".gpkg"))

      # Create GeoPackage
      if(!file.exists(private$gpkg)){

        blank_sf <- sf::st_sf(
          geometry  = sf::st_sfc(crs = sf::st_crs(proj)),
          tile_name = character()
        )

        sf::st_write(blank_sf, dsn = private$gpkg, quiet = TRUE, layer = private$geom_layer)

        # Set geometry type
        con_gpkg <- DBI::dbConnect(RSQLite::SQLite(), dbname = private$gpkg)
        DBI::dbExecute(con_gpkg, sprintf("UPDATE gpkg_geometry_columns SET geometry_type_name = '%s' WHERE table_name = '%s'", geom_type, private$geom_layer))
      }

      # Create tile registry
      if(!exists("con_gpkg")) con_gpkg <- DBI::dbConnect(RSQLite::SQLite(), dbname = private$gpkg)

      if(!"tile_reg" %in% DBI::dbListTables(con_gpkg)){

        # Get tile names
        if(is.null(ts)) stop("No tilescheme set for 'misterRS.ts' option")
        tile_names <- ts@data$tileName
        stopifnot(!is.null(tile_names), length(tile_names)>0)

        DBI::dbExecute(con_gpkg, "CREATE TABLE tile_reg (tile_name varchar(50) NOT NULL, UNIQUE(tile_name))")

        DBI::dbExecute(con_gpkg, sprintf("INSERT INTO tile_reg (tile_name) VALUES ('%s')", paste(tile_names, collapse = "'), ('")))
      }
      DBI::dbDisconnect(con_gpkg)
    },

    #' @description Finalize.
    finalize = function() {
      self$disconnect()
    },

    #' @description Print.
    print = function(){

      # Make separate connection for this
      con <- self$temp_con()

      # Get tile count
      tile_reg <- DBI::dbReadTable(con, "tile_reg")
      total_tiles <- nrow(tile_reg)
      attribute_names <- setdiff(names(tile_reg), "tile_name")
      if(length(attribute_names) > 0){
        print_nums <- sapply(attribute_names, function(x){
          num <- length(tile_reg[[x]][tile_reg[[x]]==1])
          crayon_col <- if(num == total_tiles) crayon::green else crayon::yellow
          return(crayon_col(num))
        })
        print_totals <- paste0("  ", stringr::str_pad(attribute_names,7,"right"), " : ", print_nums, "\n")
      }else{
        print_totals <- NULL
      }

      cat(
        "VECTOR TILESET", "\n",
        "ID        : ", private$id ,  "\n",
        "Name      : ", private$name, "\n",
        "Dir       : ", private$dir,  "\n",
        "Tiles     : ", total_tiles, "\n",
        print_totals,
        sep = ""
      )
    },

    #' @description Connect to Geopackage DB.
    #' @param mod_spatialite Path to Spatialite directory.
    connect = function(mod_spatialite = getOption("misterRS.mod_spatialite")){

      withr::with_envvar(list(PATH =  mod_spatialite), {

        private$db_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = private$gpkg)

        DBI::dbExecute(private$db_con, "PRAGMA busy_timeout=50000;")
        DBI::dbExecute(private$db_con, "SELECT load_extension('mod_spatialite')")
      })

      invisible(self)
    },

    #' @description Generate a temporary connection that will close itself after
    #' parent environment exits
    temp_con = function(){
      con <- DBI::dbConnect(RSQLite::SQLite(), dbname = private$gpkg)
      withr::defer({DBI::dbDisconnect(con)}, envir = parent.frame(1))
      return(con)
    },

    #' @description Disconnect from Gepackage DB
    disconnect = function(){

      if(class(private$db_con) == 'SQLiteConnection') {
        if(DBI::dbIsValid(private$db_con)){
          DBI::dbDisconnect(private$db_con)
        }
      }

      invisible(self)
    },

    #' @description Execute SQL.
    #' @param code Code for transaction
    transact = function(code) {

      con_gpkg <- self$con

      DBI::dbExecute(con_gpkg, "BEGIN IMMEDIATE TRANSACTION")

      tryCatch(
        {
          res <- force(code)
          DBI::dbExecute(con_gpkg, "COMMIT TRANSACTION")
          res
        },
        db_abort  = private$rollback,
        error     = private$rollback,
        interrupt = private$rollback
      )

      invisible(self)
    },

    #' @description Check if has tiles.
    #' @param tile_names Name of tile.
    #' @param attribute Name of attribute.
    has_tiles = function(tile_names, attribute){

      if(!attribute %in% DBI::dbListFields(self$con, "tile_reg")) stop("Attribute '", attribute, "' not found in VTS '", private$id,"'")

      if(is.null(tile_names) | length(tile_names) == 0){

        return(character())

      }else{

        # Get tile registry for this attribute
        sql <- sprintf("SELECT tile_name, %s FROM tile_reg", attribute)
        tile_reg <- DBI::dbGetQuery(self$con, sql)

        # Check that all input tile_names are registered
        if(any(!tile_names %in% tile_reg[,"tile_name"])) stop("One or more 'tile_names' were missing from tile registry")

        # Check if the tiles exist for this attribute
        tiles_exist <- setNames(tile_reg[match(tile_names, tile_reg[,"tile_name"] ), attribute] == 1, tile_names)

        return(tiles_exist)
      }
    },

    #' @description Write tile.
    #' @param data sf object being written.
    #' @param tile_name Name of tile.
    #' @param attribute Name of attribute.
    #' @param overwrite Overwrite this tile.
    write_tile = function(data, tile_name, attribute, overwrite = FALSE){

      # Check if tile exists
      tile_exists <-  self$has_tiles(tile_name, attribute)

      if(tile_exists & !overwrite) stop("Tile '", tile_name ,"' exists for VTS '", private$id ,"' and 'overwrite' is set to FALSE", call. = FALSE)

      # Get rows for deletion
      delete_rows <- DBI::dbGetQuery(self$con, sprintf("SELECT fid FROM %s WHERE tile_name = '%s'", private$geom_layer, tile_name))[,"fid"]

      self$transact({

        # Write new rows
        sf::st_write(data, dsn = self$con, driver = "GPKG", layer = private$geom_layer, append = TRUE, quiet = TRUE)

        # Delete old rows
        if(length(delete_rows) > 0) DBI::dbExecute(self$con, sprintf("DELETE FROM %s WHERE fid IN (%s)", private$geom_layer, paste(delete_rows, collapse= ",")))

        # Update tile registry
        DBI::dbExecute(self$con, sprintf("UPDATE tile_reg SET %s = TRUE where tile_name = '%s'", attribute, tile_name))
      })

      invisible(self)
    },

    #' @description Add new attribute to tile registry.
    #' @param attribute Name of attribute being added.
    add_attribute = function(attribute){

      existing_attributes <- DBI::dbListFields(self$con, "tile_reg")

      if(!attribute %in% existing_attributes){

        sql <- paste("ALTER TABLE tile_reg ADD", attribute, "boolean NOT NULL DEFAULT(0);")

        self$transact({
          DBI::dbExecute(self$con, sql)
        })
      }
      invisible(self)
    },

    #' @description Add new field to geometry layer.
    #' @param field_name Name of field being added.
    #' @param field_type Data type of field.
    add_field = function(field_name, field_type){

      existing_fields <- DBI::dbListFields(self$con, private$geom_layer)

      if(!field_name %in% existing_fields){

        sql <- sprintf("ALTER TABLE %s ADD %s %s;" , private$geom_layer, field_name, field_type)

        self$transact({
          DBI::dbExecute(self$con, sql)
        })
      }
      invisible(self)
    },

    #' @description Create a database index on a column.
    #' @param field_name Name of field being indexed.
    index = function(field_name = "tile_name"){

      index_exists <- DBI::dbGetQuery(self$con, sprintf("SELECT * FROM sqlite_master WHERE type = 'index' and tbl_name = '%1$s' and name = '%1$s_%2$s'", private$geom_layer, field_name))

      if(nrow(index_exists) == 0){

        self$transact({
          DBI::dbExecute(self$con, sprintf("CREATE INDEX %1$s_%2$s ON %1$s (%2$s)", private$geom_layer, field_name))
        })
      }

      invisible(self)
    },

    #' @description Is this VTS complete?
    #' @param attribute Name of attribute.
    #' @param tile_names Name of tiles. If set to NULL, all tiles will be selected.
    complete = function(attribute, tile_names = NULL){

      if(is.null(tile_names)) tile_names <- DBI::dbGetQuery(self$con, "SELECT tile_name FROM tile_reg")[,"tile_name"]

      return(all(self$has_tiles(tile_names, attribute)))
    }
  ),

  private = list(

    gpkg       = NA_character_,
    geom_layer = NA_character_,
    proj       = NA_character_,
    db_con     = NULL,

    rollback = function(e){

      call <- DBI::dbExecute(self$con, "ROLLBACK")

      if (identical(call, FALSE)) {

        stop(paste(
          "Failed to rollback transaction.",
          "Tried to roll back because an error occurred:",
          conditionMessage(e)

        ), call. = FALSE)
      }
      if (inherits(e, "error")) stop(e)

      warning("Database transaction rolled back", call. = FALSE)

    }
  ),

  active = list(

    #' @field con to database.
    con = function(value){

      if(missing(value)){

        if(class(private$db_con) == 'SQLiteConnection'){

          return(private$db_con)

        }else{

          stop("VTS '", private$id, "' is not connected", call. = FALSE)

        }

      }else{

        stop("Read only", call. = FALSE)
      }
    }
  )
)
