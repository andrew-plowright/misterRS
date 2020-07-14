#' Remote Sensing Dataset class
#' @importClassesFrom TileManager tileScheme
#' @export

setClass(
  "RSDS",
  representation(
    name       = 'character',
    mosaic     = 'character',
    tileScheme = 'tileScheme',
    tilePaths  = 'character'
  )
)


setMethod("show", "RSDS", function(object){

  exists <- length(object@tilePaths[file.exists(object@tilePaths)])

  cat(
    "REMOTE SENSING DATASET", "\n",
    "Name   : ", object@name,  "\n",
    "Folder : ", dirname(object@tilePaths)[1], "\n",
    "Tiles  : ", exists, " out of ", length(object@tilePaths), "\n",
    sep = ""
  )

})

#' Remote Sensing Dataset
#' @export

RSDS <- function(name, folder, tileScheme, ext, mosaic = NA_character_, prefix = NULL, suffix = NULL){

  # Create folder
  if(!dir.exists(folder)) dir.create(folder, recursive = TRUE)

  # Get file paths
  tileNames <- tileScheme$tileName
  tilePaths <- setNames(suppressMessages(R.utils::getAbsolutePath(file.path(folder, paste0(prefix, tileNames, suffix, ".", ext)))), tileNames)

  # Create new object
  new("RSDS", name = name, mosaic = mosaic, tileScheme = tileScheme, tilePaths = tilePaths)
}



#' Training Data (class)
#' @export

setClass(
  "TrainingData",
  representation(
    name     = 'character',
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
    "Name     : ", object@name,  "\n",
    "Folder   : ", dirname(object@SHPfile), "\n",
    "Vec pts  : ", vecNum,"\n",
    "Data pts : ", rowNum,"\n",
    "Data var : ", varNum,"\n",
    sep = ""
  )
})

#' Training Data (constructor)
#' @export

TrainingData <- function(name, SHPpath, proj, overwrite = FALSE){

  if(tools::file_ext(SHPpath) != "shp") stop("Training data path should be a SHP file")

  # Get absolute SHP file path
  SHPpath <- suppressMessages(R.utils::getAbsolutePath(SHPpath))

  # Check folder
  folder <- dirname(SHPpath)
  if(!dir.exists(folder)) dir.create(folder, recursive = TRUE)

  # Get CSV file
  dataPath <- gsub("shp$", "csv", SHPpath)

  if(!file.exists(SHPpath) | overwrite){

    # Create Simple Feature object with blank geometry and empty attribute fields
    s <- sf::st_sf(geometry = sf::st_sfc(crs = proj), list(segClass = character()))

    sf::write_sf(s, SHPpath, layer_options = "SHPT=POINT", quiet = TRUE, delete_dsn = overwrite)
  }

  # Create new object
  new("TrainingData", name = name, SHPfile  = SHPpath, datafile = dataPath)
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

ClassEdits <- function(SHPpath, proj, overwrite = FALSE){

  if(tools::file_ext(SHPpath) != "shp") stop("Classification Edits path should be a SHP file")

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

