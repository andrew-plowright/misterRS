library("RSQLite")
library(magrittr)

xts <- R6::R6Class(

  "xts",

  private = list(
    id   = NA_character_,
    name = NA_character_,
    dir  = NA_character_
  ),


  public = list(

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
  )
)


