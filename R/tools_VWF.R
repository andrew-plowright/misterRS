#' Variable window filter
#' @export

VWF <- function(CHM_RSDS, ttops_RSDS, winFun, minHeight,
                tileNames = NULL, overwrite = FALSE){

  tim <- misterRS:::.headline("Variable Window Filter (VWF)")

  ### INPUT CHECKS ----

  misterRS:::.check_same_ts(CHM_RSDS, ttops_RSDS)
  misterRS:::.check_complete_input(CHM_RSDS,  tileNames)
  misterRS:::.check_extension(ttops_RSDS, "shp")


  ### CREATE WORKER ----

  worker <- function(tileName){

    out_file  <- ttops_RSDS@tilePaths[tileName]
    CHM_file  <- CHM_RSDS@tilePaths[tileName]

    # Get tile and buff
    tile <- CHM_RSDS@tileScheme[tileName][["tiles"]]
    buff <- CHM_RSDS@tileScheme[tileName][["buffs"]]

    # Read raster
    CHM  <- raster::raster(CHM_file)

    # Function for creating blank treetop SHP file
    noTrees <- function(e){
      sp::SpatialPointsDataFrame(
        sp::SpatialPoints(data.frame(x = 0, y = 0), proj4string = raster::crs(CHM))[-1,],
        data.frame(height = numeric(), winRadius = numeric(), treeID = integer())
      )}

    # Detect new treetops
    det_ttops <- tryCatch(ForestTools::vwf(CHM, winFun, minHeight), error = noTrees)

    # Save output
    APfun::APSHPsave( det_ttops, out_file, overwrite = overwrite)

    if(file.exists(out_file)){
      return("Success")
    }else{
      stop("Failed to create tile")
    }

  }


  ### APPLY WORKER ----

  # Get tiles for processing
  procTiles <- misterRS:::.processing_tiles(out_files, overwrite, tileNames)

  # Process
  status <- misterRS:::.doitlive(procTiles, worker)

  # Report
  misterRS:::.statusReport(status)

  # Conclude
  misterRS:::.conclusion(tim)
}
