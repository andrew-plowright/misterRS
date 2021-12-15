# NOTES
#
# http://www.cyto.purdue.edu/cdroms/micro2/content/education/wirth10.pdf
#
# Compactness = Easy
# Elongation = needs a bounding box
# Circularity = Needs convex hull
# Convexity = Needs convex hull
# Solidity = Needs convex hull
# Rectangularity = needs a bounding box

in_path <- "D:/By/DerivedData/RoughSegments/Iterations/It1/poly/tiles/R12C16.gpkg"
outDir <- "C:/Users/Andrew/Desktop/scratch"
buff <- 2

# TO DO:
#
# 1. "Smooth" is too complex: replace with simplify. Check if all geometry is valid!

tm <- APfun::APtimer()

  # Read file
  in_SF <- sf::st_read(in_path, quiet = TRUE)

  # Remove holes
  #in_SF <- sf::st_multipolygon(lapply(sf::st_geometry(in_SF), function(x) x[1]))

  # Buffer
  smooth_SF <- sf::st_buffer(in_SF, buff)

  # Apply smoothing
  #smooth_SF <- smoothr::smooth(in_SF, method = "ksmooth", smoothness = smoothness)

  # Enforce validity
  #smooth_SF <- sf::st_make_valid(smooth_SF)

  # Check if valid
  smooth_SF[["isValid"]] <- sf::st_is_valid(smooth_SF)
  if(any(!smooth_SF[["isValid"]])) stop("Invalid geometry")

  # sf::st_write(smooth_SF, "C:/Users/Andrew/Desktop/scratch/isvalid.shp")
  # plot(sf::st_geometry(smooth_SF))
  # plot(sf::st_geometry(!smooth_SF[smooth_SF$isValid,]), add = T, col ="red")

  # Get bounding box and convex hull measurements
  measures <- do.call(dplyr::bind_rows, lapply(1:nrow(smooth_SF), function(i){

    # Get shape
    smooth_shape <- smooth_SF[i,]
    smooth_line  <- sf::st_cast(smooth_shape, "LINESTRING", warn = FALSE)

    # Get shape stats
    smooth_area  <- sf::st_area(smooth_shape)
    smooth_perim <- sf::st_length(smooth_line)
    smooth_tib   <- tibble::tibble(smooth_area, smooth_perim)

    # Get coordinates
    smooth_coords <- sf::st_coordinates(smooth_shape)[,1:2]

    # Remove non-finite coordinates (otherwise chull won't work)
    # NOTE: This shouldn't be necessary

    #smooth_coords <- smooth_coords[which(is.finite(smooth_coords[,1])),]

    # Get convex hull
    hull_pts  <- chull(smooth_coords)
    hull_poly <- sf::st_polygon(list(smooth_coords[c(hull_pts, hull_pts[1]),]))
    hull_line <- sf::st_cast(hull_poly, "LINESTRING", warn = FALSE)

    # Get hull stats
    hull_area  <- sf::st_area(hull_poly)
    hull_perim <- sf::st_length(hull_line)
    hull_tib   <- tibble::tibble(hull_area, hull_perim)

    # Get min bounding box
    mbox_stats      <- minbox(smooth_coords, hull_pts)
    mbox_tib        <- tibble::as_tibble(mbox_stats[c("width", "height", "FoM", "diag", "angle")] )
    names(mbox_tib) <- paste0("mmbox_", names(mbox_tib))

    # Out tibble
    out_tib <- dplyr::bind_cols(mbox_tib, hull_tib, smooth_tib)

    return(out_tib)
  }))

APfun::APtimer(tm)
beepr::beep(4)

#' Compute minimum bounding box
#'
#' @param xy coordinates
#' @param H convex hull (produced by chull function)

minbox <- function(xy, H) {


  hull <- xy[H, ]                      # hull vertices

  n <- length(H)                       # number of hull vertices

  ## unit basis vectors for all subspaces spanned by the hull edges
  hDir  <- diff(rbind(hull, hull[1,])) # account for circular hull vertices
  hLens <- sqrt(rowSums(hDir^2))       # length of basis vectors
  huDir <- diag(1/hLens) %*% hDir      # scaled to unit length

  ## unit basis vectors for the orthogonal subspaces
  ## rotation by 90 deg -> y' = x, x' = -y
  ouDir <- cbind(-huDir[ , 2], huDir[ , 1])

  ## project hull vertices on the subspaces spanned by the hull edges, and on
  ## the subspaces spanned by their orthogonal complements - in subspace coords
  projMat <- rbind(huDir, ouDir) %*% t(hull)

  ## range of projections and corresponding width/height of bounding rectangle
  rangeH  <- matrix(numeric(n*2), ncol=2)   # hull edge
  rangeO  <- matrix(numeric(n*2), ncol=2)   # orth subspace
  widths  <- numeric(n)
  heights <- numeric(n)
  for(i in seq(along=H)) {
    rangeH[i, ] <- range(projMat[  i, ])
    rangeO[i, ] <- range(projMat[n+i, ])  # orth subspace is in 2nd half
    widths[i]   <- abs(diff(rangeH[i, ]))
    heights[i]  <- abs(diff(rangeO[i, ]))
  }

  ## extreme projections for min-area rect in subspace coordinates
  eMin  <- which.min(widths*heights)   # hull edge leading to minimum-area
  hProj <- rbind(   rangeH[eMin, ], 0)
  oProj <- rbind(0, rangeO[eMin, ])

  ## move projections to rectangle corners
  hPts <- sweep(hProj, 1, oProj[ , 1], "+")
  oPts <- sweep(hProj, 1, oProj[ , 2], "+")

  ## corners in standard coordinates, rows = x,y, columns = corners
  ## in combined (4x2)-matrix: reverse point order to be usable in polygon()
  basis <- cbind(huDir[eMin, ], ouDir[eMin, ])  # basis formed by hull edge and orth
  hCorn <- basis %*% hPts
  oCorn <- basis %*% oPts
  pts   <- t(cbind(hCorn, oCorn[ , c(2, 1)]))

  ## angle of longer edge pointing up
  dPts <- diff(pts)
  e    <- dPts[which.max(rowSums(dPts^2)), ]  # one of the longer edges
  eUp  <- e * sign(e[2])                  # rotate upwards 180 deg if necessary
  deg  <- atan2(eUp[2], eUp[1])*180 / pi  # angle in degrees

  ## box size
  bbWidth  <- widths[eMin]
  bbHeight <- heights[eMin]

  ## figure of merit and its diagonal
  FoM    <- (bbWidth + bbHeight) / 2
  bbDiag <- sqrt(bbWidth^2 + bbHeight^2)

  return(list(pts=pts, width=bbWidth, height=bbHeight,
              FoM=FoM, diag=bbDiag, angle=deg))
}
