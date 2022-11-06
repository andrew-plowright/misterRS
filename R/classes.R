#' Remote Sensing Dataset class
#'
#' @importClassesFrom TileManager tileScheme
#' @export

setClass(
  "RSDS",
  representation(
    id   = 'character',
    name = 'character',
    dir  = 'character',
    ext  = 'character',
    archive = 'logical'
  )
)


setMethod("show", "RSDS", function(object){

  filePaths <- .get_RSDS_tilepaths(object)

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

RSDS <- function(id, name, dir, ext, archive = FALSE){

  # Create folder
  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  # Create tile folder
  dirTiles <- file.path(dir, "tiles")
  if(!dir.exists(dirTiles)) dir.create(dirTiles, recursive = TRUE)

  # Create new object
  new("RSDS", id = id, name = name, dir = dir, ext = ext, archive = archive)
}


#' Training Data (class)
#' @export

setClass(
  "TrainingData",
  representation(
    id       = 'character',
    SHPfile  = 'character',
    datafile = 'character'
  )
)


setMethod("show", "TrainingData", function(object){

  if(file.exists(object@datafile)){
    csv <- read.csv(object@datafile)
    varNum <- ncol(csv)
    rowNum <- nrow(csv)
  }else{
    varNum <- 0
    rowNum <- 0
  }

  if(file.exists(object@SHPfile)){
    info <- suppressWarnings(rgdal::ogrInfo(object@SHPfile))
   vecNum <- info$nrows
  }else{
   vecNum <- 0
  }

  cat(
    "TRAINING DATASET", "\n",
    "Name     : ", object@id,  "\n",
    "Folder   : ", dirname(object@SHPfile), "\n",
    "Vec pts  : ", vecNum,"\n",
    "Data pts : ", rowNum,"\n",
    "Data var : ", varNum,"\n",
    sep = ""
  )
})

#' Training Data (constructor)
#' @export

TrainingData <- function(id, dir, proj = getOption("misterRS.crs"), overwrite = FALSE){

  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  if(proj == "" | is.na(proj) | is.null(proj)) stop("Invalid CRS")

  # Create SHP path
  SHPpath <- file.path(dir, paste0(id, ".shp"))

  # Get absolute SHP file path
  SHPpath <- suppressMessages(R.utils::getAbsolutePath(SHPpath))

  # Get CSV file
  dataPath <- gsub("shp$", "csv", SHPpath)

  if(!file.exists(SHPpath) | overwrite){

    # Create Simple Feature object with blank geometry and empty attribute fields
    s <- sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs( proj)), list(segClass = character()))

    sf::st_write(s, SHPpath, layer_options = "SHPT=POINT", quiet = TRUE, delete_dsn = overwrite)
  }

  # Create new object
  new("TrainingData", id = id, SHPfile  = SHPpath, datafile = dataPath)
}



#' Classification edits (class)
#' @export

setClass(
  "ClassEdits",
  representation(
    SHPfile  = 'character'
  )
)


setMethod("show", "ClassEdits", function(object){

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

ClassEdits <- function(SHPpath, proj = getOption("misterRS.crs"), overwrite = FALSE){

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
  new("ClassEdits", SHPfile = SHPpath)
}

