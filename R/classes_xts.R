#' @title Generic tileset
#' @description Generic tileset.
#' @export
xts = R6::R6Class("xts",

  public = list(

    name = NA_character_,
    id   = NA_character_,
    dir  = NA_character_,

    #' @description Initialize XTS.
    #' @param id ID of XTS.
    #' @param name Name of XTS.
    #' @param dir Path to directory in which files will be created.
    initialize = function(id, name, dir){

      stopifnot(is.character(id), length(id) == 1)
      stopifnot(is.character(name), length(name) == 1)
      stopifnot(is.character(dir), length(dir) == 1)

      self$name <- name
      self$id <- id
      self$dir <- dir

      # Create folder
      if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    }
  )
)

#' @title Vector tileset
#' @description Vector tileset.
#' @export
vts = R6::R6Class("vts",

  inherit = xts,

  cloneable = TRUE,

  public = list(

    gpkg = NA_character_,
    geom_layer = NA_character_,
    id_field = NA_character_,
    crs = NA_integer_,

    #' @description Initialize VTS.
    #' @param id ID of VTS.
    #' @param name Name of VTS.
    #' @param dir Path to directory in which files will be created.
    #' @param geom_type Type of geometry (ex.: POLYGON, POINT).
    #' @param geom_layer Name of layer for geometry.
    #' @param crs EPSG number.
    #' @param ts Tileset.
    initialize = function(id, name, dir, geom_type, geom_layer = "layer", id_field = "poly_id", crs = getOption("misterRS.crs"), ts = getOption("misterRS.ts")) {

      super$initialize(id, name, dir)


      self$geom_layer <- geom_layer
      self$gpkg <- file.path(dir, paste0(id, ".gpkg"))
      self$id_field <- id_field
      self$crs <- crs

      # Create GeoPackage
      if(!file.exists(self$gpkg)){

        # Parameters for creating GeoPackage
        creation_params <- list(
          geometry  = sf::st_sfc(crs = sf::st_crs(crs)),
          tile_name = character(),
          id_field = integer()
        )

        # Set the name of the ID field
        names(creation_params)[names(creation_params)=="id_field"] <- id_field

        # Creation blank sf
        blank_sf <- do.call(sf::st_sf, creation_params)

        # Write
        sf::st_write(blank_sf, dsn = self$gpkg, quiet = TRUE, layer = self$geom_layer)

        # Set geometry type
        con_gpkg <- DBI::dbConnect(RSQLite::SQLite(), dbname = self$gpkg)
        DBI::dbExecute(con_gpkg, sprintf("UPDATE gpkg_geometry_columns SET geometry_type_name = '%s' WHERE table_name = '%s'", geom_type, self$geom_layer))
      }

      # Create tile registry
      if(!exists("con_gpkg")) con_gpkg <- DBI::dbConnect(RSQLite::SQLite(), dbname = self$gpkg)

      if(!"tile_reg" %in% DBI::dbListTables(con_gpkg)){

        # Get tile names
        if(is.null(ts)) stop("No tilescheme set for 'misterRS.ts' option")
        tile_names <- ts[["tile_name"]]
        stopifnot(!is.null(tile_names), length(tile_names)>0)

        DBI::dbExecute(con_gpkg, "CREATE TABLE tile_reg (tile_name varchar(50) NOT NULL UNIQUE, geom boolean NOT NULL DEFAULT(0))")

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

      cat("VECTOR TILESET", "\n")

      if(self$connected()){

        # Get tile count
        tile_reg <- DBI::dbReadTable(self$con, "tile_reg")
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
          "ID        : ", self$id ,  "\n",
          "Name      : ", self$name, "\n",
          "Dir       : ", self$dir,  "\n",
          "Tiles     : ", total_tiles, "\n",
          print_totals,
          sep = ""
        )

      }else{

        cat("(unconnected)","\n")

      }
    },

    temp_con = function(){

      con <- DBI::dbConnect(RSQLite::SQLite(), dbname = self$gpkg)
      withr::defer(DBI::dbDisconnect(con), envir = parent.frame(1))
      return(con)
    },

    #' @description Connect to Geopackage DB.
    #' @param mod_spatialite Path to Spatialite directory.
    connect = function(mod_spatialite = getOption("misterRS.mod_spatialite")){

      withr::with_envvar(list(PATH =  mod_spatialite), {

        private$db_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = self$gpkg)

        DBI::dbExecute(private$db_con, "PRAGMA busy_timeout=500000;")
        DBI::dbExecute(private$db_con, "PRAGMA journal_mode=WAL;")
        DBI::dbExecute(private$db_con, "SELECT load_extension('mod_spatialite')")
      })

      invisible(self)
    },

    #' temp_con = function(){
    #'   con <- DBI::dbConnect(RSQLite::SQLite(), dbname = self$gpkg)
    #'   withr::defer({DBI::dbDisconnect(con)}, envir = parent.frame(1))
    #'   return(con)
    #' },

    connected = function(){

      class(private$db_con) == 'SQLiteConnection' && DBI::dbIsValid(private$db_con)

    },

    #' @description Disconnect from Gepackage DB
    disconnect = function(){

      if(self$connected()) DBI::dbDisconnect(private$db_con)

      invisible(self)
    },

    #' @description Execute SQL.
    #' @param code Code for transaction
    transact = function(code) {

      con_gpkg <- self$con

      DBI::dbExecute(con_gpkg, "BEGIN IMMEDIATE")

      res <- tryCatch(
        {
          res <- force(code)
          DBI::dbExecute(con_gpkg, "COMMIT")
          res
        },
        db_abort  = private$rollback,
        error     = private$rollback,
        interrupt = private$rollback
      )

      return(res)
    },

    #' @description Check if has tiles.
    #' @param tile_names Name of tile.
    #' @param attribute Name of attribute.
    has_tiles = function(tile_names, attribute){

      con <- self$temp_con()

      if(!attribute %in% DBI::dbListFields(con, "tile_reg")) stop("Attribute '", attribute, "' not found in VTS '", self$id,"'")

      if(is.null(tile_names) | length(tile_names) == 0){

        return(character())

      }else{

        # Get tile registry for this attribute
        sql <- sprintf("SELECT tile_name, %s FROM tile_reg", attribute)
        tile_reg <- DBI::dbGetQuery(con, sql)

        # Check that all input tile_names are registered
        if(any(!tile_names %in% tile_reg[,"tile_name"])) stop("One or more 'tile_names' were missing from tile registry")

        # Check if the tiles exist for this attribute
        tiles_exist <- setNames(tile_reg[match(tile_names, tile_reg[,"tile_name"] ), attribute] == 1, tile_names)

        return(tiles_exist)
      }
    },

    fields = function(){

      return(DBI::dbListFields(self$con, self$geom_layer))
    },

    read_tile = function(tile_name, fields = NULL){

      sql_fields <- if(is.null(fields)) "*" else paste(fields, collapse=",")
      sql_query <-  sprintf("SELECT %s FROM %s WHERE tile_name = '%s'", sql_fields, self$geom_layer, tile_name)

      if(is.null(fields) || "geom" %in% fields){

        output <- sf::st_read(self$gpkg, query = sql_query, quiet = TRUE)

      }else{

        output <- DBI::dbGetQuery(self$con, sql_query)
      }
      return(output)
    },

    read_from_pts = function(pts, fields = NULL){

      # Template for query
      sql_template <- "SELECT %1$s FROM %4$s WHERE fid IN (SELECT id FROM rtree_%4$s_geom WHERE minx <= %2$s AND maxx >= %2$s AND miny <= %3$s AND maxy >= %3$s)"

      # Select fields
      if(is.null(fields)){

        sql_fields <- '*'

      }else{

        # "fid" and "geom" is always needed so that duplicates can be removed and geometry intersected
        sql_fields <- paste(union(c(paste0(self$geom_layer, ".fid"), paste0(self$geom_layer, ".geom")), setdiff(fields, c("fid", "geom"))), collapse =", ")
      }

      # Get initial geometry (subset using r_tree)
      initial_geom <- do.call(rbind, lapply(1:nrow(pts), function(i){

        coords <- sf::st_coordinates(pts[i,])

        x <- coords[,"X"]
        y <- coords[,"Y"]

        sql_query <- sprintf(sql_template, sql_fields, x, y, self$geom_layer)

        sf::st_read(self$gpkg, query = sql_query, quiet = TRUE, fid_column_name = "fid")
      }))

      # Drop duplicates
      initial_geom <- initial_geom[!duplicated(initial_geom$fid),]

      # Refine selection using st intersection tools
      out_data <- initial_geom[sapply(sf::st_intersects(pts, initial_geom), "[", 1),]

      # Drop geometry if requested
      if(!is.null(fields) && (!"geom" %in% fields)){
        out_data <- sf::st_drop_geometry(out_data)
      }

      # Select only requested fields
      out_data <- out_data[,fields]

      return(out_data)
    },


    read_from_polys = function(polys, fields = NULL){

      # Select fields
      #sql_template <- "WITH in_poly(geom) AS (VALUES(ST_GeomFromText('%1$s'))) SELECT %2$s FROM %3$s, in_poly WHERE fid IN (SELECT id FROM rtree_%3$s_geom, in_poly WHERE minx <= MbrMaxX(in_poly.geom) AND maxx >= MbrMinX(in_poly.geom) AND miny <= MbrMaxY(in_poly.geom) AND maxy >= MbrMinY(in_poly.geom))"
      sql_template <- "SELECT %1$s FROM %2$s WHERE fid IN (SELECT id FROM rtree_%2$s_geom WHERE minx <= %3$s AND maxx >= %4$s AND miny <= %5$s AND maxy >= %6$s)"

      # Select fields
      if(is.null(fields)){

        sql_fields <- '*'

      }else{

        # "fid" and "geom" is always needed so that duplicates can be removed and geometry intersected
        sql_fields <- paste(union(c(paste0(self$geom_layer, ".fid"), paste0(self$geom_layer, ".geom")), setdiff(fields, c("fid", "geom"))), collapse =", ")
      }

      # Get initial geometry (subset using r_tree)
      initial_geom <- dplyr::bind_rows(lapply(1:nrow(polys), function(i){

        #sql_poly <- sf::st_as_text(sf::st_geometry(polys[i,]))
        bbox <- sf::st_bbox(polys[i,])

        sql_query <- sprintf(sql_template, sql_fields, self$geom_layer, bbox['xmax'], bbox['xmin'], bbox['ymax'], bbox['ymin'])

        sf::st_read(self$gpkg, query = sql_query, quiet = TRUE, fid_column_name = "fid")
      }))

      # Drop duplicates
      initial_geom <- initial_geom[!duplicated(initial_geom$fid),]

      # Refine selection using st intersection tools
      geom_intrsc <- sf::st_intersects(polys, initial_geom)

      lapply(geom_intrsc, function(intrsc){

        out_data <- initial_geom[intrsc,]

        # Select only requested fields
        if(!is.null(fields)){

          # Drop geometry if requested
          if(!"geom" %in% fields){
            out_data <- sf::st_drop_geometry(out_data)
          }

          out_data <- out_data[,fields]
        }
        return(out_data)
      })
    },

    #' @description Write tile.
    #' @param data sf object being written.
    #' @param tile_name Name of tile.
    #' @param attribute Name of attribute.
    #' @param overwrite Overwrite this tile.
    append_geom = function(data, tile_name){

      # Check if tile exists
      tile_exists <-  self$has_tiles(tile_name, "geom")
      if(tile_exists) stop("Tile '", tile_name ,"' exists for VTS '", self$id ,"'", call. = FALSE)

      if(nrow(data) > 0){

        # Check for unique identifier
        if(!self$id_field %in% names(data)) stop("Input data does not contain unique identifier '", self$id_field, "'")

        # Check for geometry
        if(!"geom" %in% names(data)) stop("Input data does not geometry")
        if(!"sfc" %in% class(data$geom)) stop("Input data geometry is not of correct class")

        # Add tile_name
        data[["tile_name"]] <- tile_name

        # Check for duplicates
        if(any(duplicated(data[[self$id_field]]))) stop("Input data contains duplicates for unique identifier '", self$id_field, "'")
      }

      # Write new rows
      if(nrow(data) > 0){

        # Get output fields. It may be necessary to order the output data correctly
        gpkg_fields <-  setdiff(DBI::dbListFields(self$con, self$geom_layer), "fid")

        sf::st_write(data[,gpkg_fields], dsn = self$gpkg, layer = self$geom_layer, append = TRUE, quiet = TRUE)
      }

      # Update tile registry
      self$transact({

        DBI::dbExecute(self$con, sprintf("UPDATE tile_reg SET %s = TRUE where tile_name = '%s'", "geom", tile_name))
      })

      invisible(self)
    },

    update_data = function(data, tile_name, attribute, overwrite = FALSE){

      tile_exists <-  self$has_tiles(tile_name, attribute)
      if(tile_exists & !overwrite) stop("Tile '", tile_name ,"' exists for VTS '", self$id ,"' attribute '", attribute, "' and overwrite is set to FALSE", call. = FALSE)

      if(nrow(data) > 0){

        # Check for unique identifier
        if(!self$id_field %in% names(data)) stop("Input data does not contain unique identifier '", self$id_field, "'")

        # ID field and geometry layer
        id_field <- self$id_field
        geom_layer <- self$geom_layer

        # Data fields (excluding id field)
        data_fields <- setdiff(names(data), id_field)

        # Add quotes to characters
        for(i in 1:ncol(data)){
          if(class(data[[i]]) == "character") data[[i]] <- sQuote(data[[i]], FALSE)
        }

        # SQL
        sql_data_fields <- paste(c(id_field, data_fields), collapse = ", ")

        sql_values <- paste(sapply(1:nrow(data), function(i) paste0("(", paste(data[i,c(id_field, data_fields)], collapse=", "), ")")  ), collapse=", ")

        sql_field_mapping <- paste(sapply(data_fields, function(field){
          sprintf("%s = (SELECT %s FROM Tmp WHERE %s.%s = Tmp.%s)", field, field, geom_layer, id_field, id_field)
        }), collapse = ", ")

        sql_insert <- sprintf(
          "WITH Tmp(%s) AS (VALUES%s) UPDATE %s SET %s WHERE %s IN (SELECT %s FROM Tmp) AND tile_name = '%s';",
          sql_data_fields, sql_values, geom_layer, sql_field_mapping, id_field, id_field, tile_name
        )
      }

      if(nrow(data) > 0){

        # Execute insertion
        self$transact({ DBI::dbExecute(self$con, sql_insert) })
      }

      # Update tile registry
      self$transact({ DBI::dbExecute(self$con, sprintf("UPDATE tile_reg SET %s = TRUE where tile_name = '%s'", attribute, tile_name)) })
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

      existing_fields <- DBI::dbListFields(self$con, self$geom_layer)

      if(!field_name %in% existing_fields){

        sql <- sprintf("ALTER TABLE %s ADD %s %s;" , self$geom_layer, field_name, field_type)

        self$transact({
          DBI::dbExecute(self$con, sql)
        })
      }
      invisible(self)
    },

    #' @description Create a database index on a column.
    #' @param field_name Name of field being indexed.
    index = function(field_name = "tile_name"){

      con <- self$temp_con()

      index_exists <- DBI::dbGetQuery(con, sprintf("SELECT * FROM sqlite_master WHERE type = 'index' and tbl_name = '%1$s' and name = '%1$s_%2$s'", self$geom_layer, field_name))

      if(nrow(index_exists) == 0){

          DBI::dbExecute(con, sprintf("CREATE INDEX %1$s_%2$s ON %1$s (%2$s)", self$geom_layer, field_name))
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

          if(DBI::dbIsValid(private$db_con)){

            return(private$db_con)

          }else stop("VTS '", self$id ,"' is not connected")

        }else stop("VTS '", self$id ,"' is not connected")

      }else  stop("Read only", call. = FALSE)

    }
  )
)
