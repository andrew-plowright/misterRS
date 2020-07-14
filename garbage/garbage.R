
#' Get data value from ID
.get_value <- function(RSproj, dataID){

  dataType <- .data_IDs(RSproj)[dataID, "dataType"]

  if(is.na(dataType)) stop("Could not retrieve value from data ID '", dataID, "'")

  d <- slot(RSproj, dataType)[[dataID]]

  # For 'tile_schemes' and 'SHP_files', return file path
  if(dataType %in% c("tile_schemes", "SHP_files")){

    file.path(RSproj@root_folder, d)

  }else if(dataType %in% "RSDS"){

    tsID <- d@tile_scheme

    if(tsID %in% RSproj@tile_schemes) stop("Could not find tile scheme ID '", tsID, "' in project")

    tsPath <- gsub("shp$", "dbf", file.path(RSproj@root_folder, RSproj@tile_schemes[[tsID]]))

    if(!file.exists(tsPath)) stop("Could not find SHP files for tile scheme ID '", tsID, "'")

    tsDF <- foreign::read.dbf(tsPath, as.is = TRUE)

    tiles <- subset(tsDF, type == "tile", "tileName", drop = TRUE)

    setNames(file.path(RSproj@root_folder, d@folder, paste0(d@tile_prefix, tiles, d@tile_suffix, ".", d@tile_ext)), tiles)

    # Other wise return whatever is in the slot (like a 'variable')
  }else d

}


#' Get data IDs
.data_IDs <- function(RSproj){

  data_IDs <- rbind(
    data.frame(ID = names(RSproj@variables),    dataType = "variables",    stringsAsFactors = FALSE),
    data.frame(ID = names(RSproj@tile_schemes), dataType = "tile_schemes", stringsAsFactors = FALSE),
    data.frame(ID = names(RSproj@SHP_files),    dataType = "SHP_files",    stringsAsFactors = FALSE),
    data.frame(ID = names(RSproj@RSDS),         dataType = "RSDS",         stringsAsFactors = FALSE)
  )

  dup <- duplicated(data_IDs[["ID"]])
  if(any(dup)) stop("Duplicated data IDs:\n  ", paste(data_IDs[dup, "ID"], collapse = "\n  "))

  row.names(data_IDs) <- data_IDs[["ID"]]

  return(data_IDs)
}


#' Generate data

generateData <- function(RSproj, func, inputs, outputs, overwrite = FALSE){

  ### ARGUMENT TABLE ----

  # Get table of arguments
  argt <- rbind(
    data.frame(arg = names(inputs),  dataID = inputs,  put = "input",  stringsAsFactors = FALSE),
    data.frame(arg = names(outputs), dataID = outputs, put = "output", stringsAsFactors = FALSE)
  )

  # Check data IDs (will check for duplicate IDs through '.data_IDs')
  data_IDs <- .data_IDs(RSproj)
  noID <- !argt$dataID %in% data_IDs$ID
  if(any(noID)) stop("Following inputs IDs were not found:\n  ", paste(argt$dataID[noID], collaps = "\n  "))

  # Get data types
  argt$dataType <- data_IDs[argt$dataID, "dataType"]

  # Check that all necessary arguments have been provided
  func_args <- formalArgs(func)
  noArg <- !func_args %in% argt$arg
  if(any(noArg)) stop("Following function arguments were not provided:\n  ", paste(func_args[noArg], collaps = "\n  "))

  # Get output type
  outType <- unique(argt[argt$put == "output", "dataType"])
  if(length(outType) > 1) stop("Cannot have multiple output data types")

  # Get each argument's tile scheme
  argt$TS <- sapply(row.names(argt), function(arg){
    if(argt[arg, "dataType"] == "RSDS") RSproj@RSDS[[argt[arg,]$dataID]]@tile_scheme else NA
  })

  if(outType == "RSDS" & length(unique(argt[argt$put == "output", "TS"])) > 1){
    stop("Outputs cannot have multiple tile schemes")
  }


  ### ARGUMENT VALUES ----

  argv <- setNames(lapply(argt$dataID, function(dataID) .get_value(RSproj, dataID)), argt$arg)

  # Check inputs (for RSDS and SHP_files, check if file exists)
  noVal <- sapply(argt$arg, function(arg){
    if(argt[arg, "put"] == "input"){
      av <- argv[[arg]]
      if(argt[arg, "dataType"] %in% c("RSDS", "SHP_files")){
        any(!file.exists(av))
      }else FALSE
    }else FALSE
  })
  if(any(noVal)) stop("Following data IDs had missing values or files:\n  ", paste(argt$dataID[noVal], collapse = "\n  "))


  ### GET TILE SCHEMES ----

  # Get unique tile scheme IDs
  TS_IDs <- unique(na.omit(argt$TS))

  # NOTE
  # Checking that tile schemes exist in the project and have valid paths has ALREADY
  # been performed by the ',get_value()' function above
  TS <- lapply(setNames(TS_IDs, TS_IDs), function(TS_ID){
    TileManager::tileLoad(.get_value(RSproj, TS_ID))
  })

  # Choose a tileset on which to iterate
  # 1. The output tileset
  # 2. The most common tileset
  # 3. The first tileset

  # Determine whether or not process will use tiles
  useTiles <- if(length(TS) > 0){

    # Select which tile scheme will be iterated over
    # First priority: the one used by the output
    # Second priority: whichever was entered first
    procTS_ID <- argt[argt$put == "output", "TS"][1]
    if(is.na(procTS_ID)){
      procTS_ID <- argt[argt$put == "input", "TS"][1]
    }
    procTS <- TS[[procTS_ID]]

    # Get names of tiles that need to be processed
    # If output data type is 'RSDS': get all tiles that don't yet exist
    # If output data type is something else: use all tiles
    tileNames <- if(outType == "RSDS"){

      tileExists <- apply(do.call(cbind, lapply(argt[argt$put == "output", "arg"], function(arg){
        !file.exists(argv[[arg]])
      })),1, all)

      procTS$tileName[tileExists]

    }else procTS$tileName

  }else FALSE


  ### PROCESS ----

  # Process with tiles
  if(useTiles){

    cat(
      'Tile scheme ID   : ', procTS_ID, "\n",
      'Total tiles      : ', length(procTS), "\n",
      'Processing tiles : ', length(tileNames), "\n",
      sep = ""
    )

    pb <- progress::progress_bar$new(
      format = "  Progress [:bar] :percent complete. ETA: :eta",
      total = length(tileNames),
      width = 80)

    for(tileName in tileNames){

      # Get buffered polygon of this tile
      buffPoly <- procTS[["buffs"]][tileName,]

      # Get final list of function arguments
      func_args <- lapply(setNames(argt$arg, argt$arg), function(arg){

        # For any tiled dataset, return a 'tileScheme' object. This will be a SUBSET
        # of the entire tileScheme corresponding to the tiles that intersect with the current
        # tile being processed
        if(argt[arg, "dataType"] == "RSDS"){

          argTS_ID <- argt[arg, "TS"]
          argTS    <- TS[[argTS_ID]]

          # If this dataset is using the same tileScheme as the tileScheme being iterated, then just return
          # the current tile
          argTileNames <- if(argTS_ID == procTS_ID){ tileName

            # If the tileSchemes are NOT the same, then return an intersection
          }else argTS[["nbuffs"]][buffPoly,]$tileName

          argTiles <- argTS[argTileNames,]

          argTiles$path <- argv[[arg]][argTileNames]

          return(argTiles)

          # For a non-tiled dataset, return whatever the argument value is
        }else argv[[arg]]

      })

      # RUN THE FUNCTION!!
      do.call(func, func_args)

      pb$tick()

    }

    # Process without tiles
  }else{
    stop("Processing without tiles not yet implemented!")
  }




}


# Get paths
.data_paths <- function(RSproj){

  # Internal function for getting SHp file paths
  .path_table <- function(slotName){

    data <- slot(RSproj, slotName)

    if(length(data) > 0){

      do.call(rbind, lapply(names(data), function(ID){

        path <- file.path(RSproj@root_folder,  data[[ID]])

        data.frame(
          row.names = ID,
          path      = path,
          exists    = file.exists(path),
          stringsAsFactors = FALSE
        )}))

    }else NULL
  }

  # Get paths for tile schemes and SHP files
  ts  <- .path_table("tile_schemes")
  shp <- .path_table("SHP_files")

  # Get list of tiles from tile schemes
  tiles <- lapply(setNames(row.names(ts), row.names(ts)), function(ID){
    if(ts[ID,"exists"]){
      tiles_df <- foreign::read.dbf(gsub("shp$", "dbf", ts[ID, "path"]), as.is = TRUE)
      subset(tiles_df, type == "tile", "tileName", drop = TRUE)
    }else NULL
  })

  # Get paths for RS dataset tiles
  rsds <- lapply(RSproj@RSDS, function(rs){

    rs_tiles <- tiles[[rs@tile_scheme]]

    if(!is.null(rs_tiles)){

      paths <- file.path(RSproj@root_folder, rs@folder, paste0(rs@tile_prefix, rs_tiles, rs@tile_suffix, ".", rs@tile_ext))

      data.frame(
        row.names = rs_tiles,
        path      = paths,
        exists    = file.exists(paths),
        stringsAsFactors = FALSE
      )

    }else NULL
  })


  return(
    list(
      tile_schemes = ts,
      SHP_files    = shp,
      RSDS         = rsds
    ))
}


#' Check outputs
#'
#' Still not entirely sure what I want this to do....
#'
#' So far:
#' 1) Checks tileset IDs
#' 2) ...?

.get_outputs <- function(RSproj, outputs, data_status, data_IDs){

  if(is.null(names(outputs))) stop("'outputs' must be a named vector")

  # Check for unfound data IDs
  not_found <- !outputs %in% data_IDs$ID
  if(any(not_found)) stop("Following output IDs were not found:\n  ", paste(outputs[not_found], collaps = "\n  "))

  if("RSDS" %in% data_IDs[outputs, "dataType"]){

    # Get tileset ID
    tsID <- unique(sapply(RSproj@RSDS[outputs], slot, "tile_scheme"))

    # Perform checks
    if(length(tsID) > 1)  stop("Outputs cannot have multiple tile schemes")
    if(length(tsID) == 0) stop("No tile schemes found")
    if(!tsID %in% row.names(data_status$tile_schemes)) stop("Tile scheme ID '", tsID, "' not found in RSproj")
    if(!data_status$tile_schemes[tsID, "exists"])      stop("Tile scheme ID '", tsID, "' has not been created yet")
  }

  outputs <- lapply(outputs, function(ID){

    dtype <- data_IDs[ID, "dataType"]

    if(dtype == "SHP_file"){
      data_status$SHP_files[ID, "path"]
    }else if(dtype == "RSDS"){
      data_status$RSDS[[ID]]
    }
  })

  return(outputs)
}


#' Check inputs

.get_inputs <- function(RSproj, inputs, data_status, data_IDs){

  if(is.null(names(inputs))) stop("'inputs' must be a named vector")

  # Check for unfound data IDs
  not_found <- !inputs %in% data_IDs$ID
  if(any(not_found)) stop("Following inputs IDs were not found:\n  ", paste(inputs[not_found], collaps = "\n  "))

  # Check for incomplete inputs
  inputs <- lapply(inputs, function(ID){

    dtype <- data_IDs[ID, "dataType"]

    if(dtype == "SHP_file"){
      if(data_status$SHP_files[ID, "exists"]) data_status$SHP_files[ID, "path"] else NULL
    }else if(dtype == "variable"){
      RSproj@variables[[ID]]
    }else if(dtype == "RSDS"){
      if(all(!data_status$RSDS[[ID]][,"exists"])) data_status$RSDS[[ID]] else NULL
    }
  })

  incomplete <- sapply(inputs, is.null)

  if(any(incomplete)) stop("Following inputs had missing or incomplete files:\n  ", paste(inputs[incomplete], collaps = "\n  "))

  return(inputs)
}


setClass(
  "RSproject",
  representation(
    name          = 'character',
    root_folder   = 'character',
    crs           = 'character',
    variables     = 'list',
    tile_schemes  = 'list',
    SHP_files     = 'list',
    RSDS          = 'list'
  )
)

RSproject <- function(name, ...){

  args <- as.list( match.call() )
  args[[1]] <- as.character(args[[1]])
  do.call(new, args)

}

setClass(
  "RSDS",
  representation(
    folder        = 'character',
    tile_scheme   = 'character',
    tile_ext      = 'character',
    tile_prefix   = 'character',
    tile_suffix   = 'character'
  )
)

RSDS <- function(name, ...){

  args <- as.list( match.call() )
  args[[1]] <- as.character(args[[1]])
  do.call(new, args)

}

setMethod("show", signature = "RSproject", function(object){

  # List of variables
  var <- if(length(object@variables) > 0){

    maxChar <-  max(nchar(names(object@variables)))

    varName <- stringr::str_pad(names(object@variables), width = maxChar, "right")
    varVal <- sapply(object@variables, function(v){
      v <- paste(v, collapse = ", ")
      if(nchar(v) > 30 ) paste0(substr(v, 1,27), "...") else v
    })
    paste(paste(varName, ":", varVal), collapse = "\n  ")

  }else "None"

  # List of tile schemes
  ts <- if(length(object@tile_schemes) > 0){
    paste(names(object@tile_schemes), collapse = "\n  ")
  }else "None"

  # List of SHP files
  SHP <- if(length(object@SHP_files) > 0){
    paste(names(object@SHP_files), collapse = "\n  ")
  }else "None"

  # List of RSDS
  RSDS <- if(length(object@RSDS) > 0){
    paste(names(object@RSDS), collapse = "\n  ")
  }else "None"

  cat(
    "RS PROJECT","\n\n",
    "Name : ", object@name,"\n",
    "Root : ", object@root_folder,"\n",
    "CRS  : ", object@crs, "\n\n",
    "Variables", "\n",
    "  ", var, "\n\n",
    "Tile schemes", "\n",
    "  ", ts, "\n\n",
    "SHP files", "\n",
    "  ", SHP, "\n\n",
    "RS datasets", "\n",
    "  ", RSDS, "\n",
    sep = ""
  )
})
