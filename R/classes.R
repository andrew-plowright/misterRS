#' Remote Sensing Dataset class
#'
#' @importClassesFrom TileManager tileScheme
#' @export

setClass(
  "rsds",
  representation(
    id   = 'character',
    name = 'character',
    dir  = 'character',
    ext  = 'character',
    archive = 'logical'
  )
)


setMethod("show", "rsds", function(object){

  filePaths <- .get_rsds_tilepaths(object)

  cat(
    "REMOTE SENSING DATASET", "\n",
    "ID      : ", object@id ,  "\n",
    "Name    : ", object@name, "\n",
    "Dir     : ", object@dir,  "\n",
    "Ext     : ", object@ext,  "\n",
    "Tiles   : ", length(filePaths[file.exists(filePaths)]), "/", length(filePaths), "\n",
    "Archive : ", object@archive, "\n",
    sep = ""
  )

})

#' Remote Sensing Dataset
#' @export

rsds <- function(id, name, dir, ext, archive = FALSE){

  # Create folder
  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  # Create tile folder
  dirTiles <- file.path(dir, "tiles")
  if(!dir.exists(dirTiles)) dir.create(dirTiles, recursive = TRUE)

  # Create new object
  new("rsds", id = id, name = name, dir = dir, ext = ext, archive = archive)
}


#' Training Data (class)
#' @export

setClass(
  "trainingdata",
  representation(
    id = 'character',
    file_path = 'character'
  )
)


setMethod("show", "trainingdata", function(object){

  if(file.exists(object@file_path)){

    lyrs <- sf::st_layers(object@file_path)

    features_points <- lyrs$features[lyrs$name == "points"]
    features_polys  <- lyrs$features[lyrs$name == "polygons"]
    features_data   <- if("data" %in% lyrs$name) lyrs$features[lyrs$name == "data"] else 0

  }else{

    features_points <- 0
    features_polys  <- 0
    features_data   <- 0
  }

  cat(
    "TRAINING DATASET", "\n",
    "ID        : ", object@id,  "\n",
    "File      : ", object@file_path, "\n",
    "Points    : ", features_points, "\n",
    "Polygons  : ", features_polys, "\n",
    "Data rows : ", features_data,

    sep = ""
  )
})

#' Training Data (constructor)
#' @export

training_data <- function(id, dir, proj = getOption("misterRS.crs"), overwrite = FALSE){

  if(proj == "" | is.na(proj) | is.null(proj)) stop("Invalid CRS")

  file_path <- file.path(dir, paste0(id, ".gpkg"))

  if(!file.exists(file_path) | overwrite){

    if(overwrite) file.remove(file_path)

    dir <- dirname(file_path)
    if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)

    # Create Simple Feature object with blank geometry and empty attribute fields
    pts  <- poly <- sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs( proj)), list(segClass = character()))

    sf::st_write(pts,  file_path, quiet = TRUE, layer = "points",   delete_layer = TRUE)
    sf::st_write(poly, file_path, quiet = TRUE, layer = "polygons", delete_layer = TRUE)

    con = DBI::dbConnect(RSQLite::SQLite(),dbname= file_path)
    withr::defer(DBI::dbDisconnect(con))

    DBI::dbExecute(con, "UPDATE gpkg_geometry_columns SET geometry_type_name = 'POINT' WHERE table_name = 'points'")
    DBI::dbExecute(con, "UPDATE gpkg_geometry_columns SET geometry_type_name = 'POLYGON' WHERE table_name = 'polygons'")
  }

  # Create new object
  new("trainingdata", id = id,  file_path = file_path)
}

#' Classification edits (class)
#' @export

setClass(
  "class_edits",
  representation(
    SHPfile  = 'character'
  )
)


setMethod("show", "class_edits", function(object){

  if(file.exists(object@SHPfile)){
    info <- suppressWarnings(rgdal::ogrInfo(object@SHPfile))
    vecNum <- info$nrows
  }else{
    vecNum <- 0
  }

  cat(
    "CLASSIFICATION EDITS", "\n",
    "Vectors : ", vecNum,"\n",
    sep = ""
  )
})

#' Remote Sensing Dataset
#' @export

class_edits <- function(SHPpath, proj = getOption("misterRS.crs"), overwrite = FALSE){

  if(tools::file_ext(SHPpath) != "shp") stop("Classification Edits path should be a SHP file")

  if(proj == "" | is.na(proj) | is.null(proj)) stop("Invalid CRS")

  # Get absolute SHP file path
  SHPpath <- suppressMessages(R.utils::getAbsolutePath(SHPpath))

  # Check folder
  folder <- dirname(SHPpath)
  if(!dir.exists(folder)) dir.create(folder, recursive = TRUE)

  if(!file.exists(SHPpath) | overwrite){

    # Create Simple Feature object with blank geometry and empty attribute fields
    s <- sf::st_sf(geometry = sf::st_sfc(crs = proj), list(toClass = character(), fromClass = character()))

    sf::write_sf(s, SHPpath, layer_options = "SHPT=POLYGON", quiet = TRUE, delete_dsn = overwrite)
  }

  # Create new object
  new("class_edits", SHPfile = SHPpath)
}

