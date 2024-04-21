# FUNCTIONS ----

# Function to build VTS
test_build_vts <- function(in_vts, density){

  # Get tile scheme
  ts <- .tilescheme()

  in_vts$connect()

  # Test worker
  worker <- function(tile_name){

    # Get tile
    tile <- ts[tile_name][["buffs"]]

    # Generate point data
    pts <- sf::st_make_grid(tile, n=density, what = "centers")
    new_geom = sf::st_as_sf(data.frame(tile_name =  tile_name, poly_id = 1:length(pts)), geom = pts)

    # Append to VTS
    in_vts$append_geom(new_geom, tile_name = tile_name)

    return("Success")
  }

  results <- .exe_tile_worker(
    tile_names   = ts[["tile_name"]],
    worker       = worker,
    clusters     = 1
  )

  in_vts$disconnect()

  return(results)
}

# ENVIRONMENT ----

test_crs <- 2955

# Test tileset
test_ts <- TileManager::tileScheme(terra::ext(0, 2, 0, 2), dim = c(1,1), crs = test_crs)

# Local environments
withr::local_options(
  misterRS.crs      = test_crs,
  misterRS.clusters = 1,
  misterRS.verbose  = FALSE,
  misterRS.ts       = test_ts
)

# TESTS ----

test_that("Merge VTS", {

  vts_dir <- withr::local_tempdir()

  # Test zones
  test_zones <- sf::st_sf(
    geometry  = sf::st_sfc(sf::st_polygon(list(matrix(c(0,0,0,2,2,2,0,0), ncol=2,byrow = TRUE))), crs = test_crs),
    zone_name = 'sparse'
  )

  # Build two VTS: one with dense points, and the other with sparse points
  vts_sparse    <- vts$new(id = "vts_sparse", name = "Sparse VTS",    dir = vts_dir, geom_type = "POINT", geom_layer = "layer")
  vts_dense     <- vts$new(id = "vts_dense",  name = "Dense VTS",     dir = vts_dir, geom_type = "POINT", geom_layer = "layer")
  vts_combined  <- vts$new(id = "vts_combo",  name = "Combined VTS",  dir = vts_dir, geom_type = "POINT", geom_layer = "layer")

  # Populate test VTS with points
  test_build_vts(vts_sparse, density = 10)
  test_build_vts(vts_dense,  density = 20)

  # Connect
  vts_sparse$connect()
  vts_dense$connect()

  # All geometry was created
  expect_equal(400,  DBI::dbGetQuery(vts_sparse$con, "SELECT COUNT(fid) FROM layer")[,1])
  expect_equal(1600, DBI::dbGetQuery(vts_dense$con,  "SELECT COUNT(fid) FROM layer")[,1])

  # Disconnect
  vts_sparse$disconnect()
  vts_dense$disconnect()

  # Merge
  merge_vts(
    in_vts_list = list(
      'sparse' = vts_sparse,
      '<none>' = vts_dense
    ),
    out_vts     = vts_combined,
    zones       = test_zones,
    zone_field  = "zone_name"
  )

  # Connect
  vts_combined$connect()

  # Merge created expected number of points
  expect_equal(990,  DBI::dbGetQuery(vts_combined$con, "SELECT COUNT(fid) FROM layer")[,1])

  # All IDs are unique (for each tile name)
  expect_false(any(duplicated(DBI::dbGetQuery(vts_combined$con, "SELECT poly_id, tile_name FROM layer"))))

  # Disconnect
  vts_combined$disconnect()
})
