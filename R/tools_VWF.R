#' Variable window filter
#' @export

VWF <- function(CHM_RSDS, ttops_RSDS, winFun, minHeight,
                tileNames = NULL, overwrite = FALSE){

  tim <- misterRS:::.headline("Variable Window Filter (VWF)")

  ### INPUT CHECKS ----

  # Check extensions
  .check_extension(CHM_RSDS,   "tif")
  .check_extension(ttops_RSDS, "shp")

  # Check that inputs are complete
  .check_complete_input(CHM_RSDS, tileNames)

  # Get tile scheme
  ts <- .get_tilescheme()

  # Get file paths
  CHM_paths <- .get_RSDS_tilepaths(CHM_RSDS)
  out_paths <- .get_RSDS_tilepaths(ttops_RSDS)

  ### CREATE WORKER ----

  worker <- function(tileName){

    # Paths
    CHM_path  <- CHM_paths[tileName]
    out_path  <- out_paths[tileName]

    # Get tile and buff
    tile <- ts[tileName][["tiles"]]
    buff <- ts[tileName][["buffs"]]

    # Read raster
    CHM  <- raster::raster(CHM_path)

    # Function for creating blank treetop SHP file
    noTrees <- function(e){
      sp::SpatialPointsDataFrame(
        sp::SpatialPoints(data.frame(x = 0, y = 0), proj4string = raster::crs(CHM))[-1,],
        data.frame(height = numeric(), winRadius = numeric(), treeID = integer())
      )}

    # Detect new treetops
    det_ttops <- tryCatch(ForestTools::vwf(CHM, winFun, minHeight), error = noTrees)

    # Save output
    APfun::APSHPsave( det_ttops, out_path, overwrite = overwrite)

    if(file.exists(out_path)){
      return("Success")
    }else{
      stop("Failed to create tile")
    }

  }


  ### APPLY WORKER ----

  # Get tiles for processing
  procTiles <- misterRS:::.processing_tiles(out_paths, overwrite, tileNames)

  # Process
  status <- misterRS:::.doitlive(procTiles, worker)

  # Report
  misterRS:::.statusReport(status)

  # Conclude
  misterRS:::.conclusion(tim)
}
