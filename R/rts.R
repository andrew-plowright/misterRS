#' Raster tileset
#'
#' @importClassesFrom TileManager tileScheme
#' @export

setClass(
  "rts",
  representation(
    id   = 'character',
    name = 'character',
    dir  = 'character',
    ext  = 'character'
  )
)


setMethod("show", "rts", function(object){

  filePaths <- .rts_tile_paths(object)

  cat(
    "REMOTE SENSING DATASET", "\n",
    "ID      : ", object@id ,  "\n",
    "Name    : ", object@name, "\n",
    "Dir     : ", object@dir,  "\n",
    "Ext     : ", object@ext,  "\n",
    "Tiles   : ", length(filePaths[file.exists(filePaths)]), "/", length(filePaths), "\n",
    sep = ""
  )

})


#' Raster tileset
#' @export

rts <- function(id, name, dir, ext ="tif"){

  # Create folder
  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  # Create tile folder
  dir_tiles <- file.path(dir, "tiles")
  if(!dir.exists(dir_tiles)) dir.create(dir_tiles, recursive = TRUE)

  # Create new object
  new("rts", id = id, name = name, dir = dir, ext = ext)
}







