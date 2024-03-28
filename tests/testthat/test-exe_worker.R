
if(basename(getwd()) != "testthat") setwd(file.path(getwd(), "tests", "testthat"))

test_that("Run .exe_tile_worker in parallel and serial", {

  # Test CRS
  test_crs <- 2955

  # Test tileset
  test_ts <- TileManager::tileScheme(terra::ext(0, 3, 0, 3), dim = c(1,1), crs = sp::CRS(paste0("epsg:", test_crs)))

  # Tile names
  tile_names <- test_ts[["tile_name"]]

  # Local environments
  withr::local_options(
    misterRS.crs        = test_crs,
    misterRS.verbosity  = FALSE,
    misterRS.ts         = test_ts
  )

  # Test function: append geometry
  test_append_geom <- function(in_vts, tile_names, clusters){

    rows = 100

    # Bounds in which to generate random points
    box = sf::st_sfc(sf::st_polygon(list(rbind(c(0,0),c(90,0),c(90,90),c(0,90),c(0,0)))), crs = sf::st_crs(test_crs))

    # Test worker
    worker <- function(tile_name){

      # Generate point data
      new_geom = sf::st_as_sf(data.frame(tile_name =  tile_name, poly_id = 1:rows), geom = sf::st_sample(box, rows))

      # Append to VTS
      in_vts$append_geom(new_geom, tile_name = tile_name)

      return("Success")
    }

    results <- .exe_tile_worker(
      tile_names   = tile_names,
      worker       = worker,
      clusters     = clusters,
      cluster_vts    = "in_vts"

    )

    return(results)
  }


  # Test function: append geometry
  test_update_data <- function(in_vts, tile_names, clusters){


    in_vts$add_attribute("attr")
    new_fields <- paste0("attr_", letters[1:10])

    for(field in new_fields) in_vts$add_field(field, "INTEGER")


    # Test worker
    worker <- function(tile_name){

      seg_data <- in_vts$read_tile(tile_name = tile_name)

      # Generate point data
      for(field in new_fields) seg_data[[field]] <- 10000

      seg_data <- sf::st_drop_geometry(seg_data)[, c(in_vts$id_field, new_fields)]

      # Append to VTS
      in_vts$update_data(seg_data, tile_name = tile_name, attribute = "attr")

      return("Success")
    }

    results <- .exe_tile_worker(
      tile_names   = tile_names,
      worker       = worker,
      clusters     = clusters,
      cluster_vts    = "in_vts"

    )

    return(results)
  }

  # Temporary directory
  vts_dir <- withr::local_tempdir()

  # SERIAL

  # Create VTS
  my_vts_ser <- vts$new(
    id         = "my_vts_ser",
    name       = "My Serial VTS",
    dir        = vts_dir,
    geom_type  = "POINT",
    geom_layer = "layer"
  )

  # Execute in serial
  results_ser <- test_append_geom(my_vts_ser, tile_names, clusters = 1)

  # All nodes were successful
  expect_true(all(results_ser == "Success"))

  # VTS should NOT be connected
  expect_error(my_vts_ser$con)

  my_vts_ser$connect()

  # All geometry was created
  expect_equal(900, DBI::dbGetQuery(my_vts_ser$con, "SELECT COUNT(fid) FROM layer")[,1])

  # All tiles were registered
  expect_true(all(DBI::dbGetQuery(my_vts_ser$con, "SELECT geom FROM tile_reg")[,1] == 1))

  # The output has a valid spatial extent
  expect_false(any(is.na(DBI::dbReadTable(my_vts_ser$con, "gpkg_contents")[,c("min_x", "min_y", "max_x", "max_y")])))


  # 2024-03-02 - What's going on with parallel processing?

  # Basically: it just doesn't seem like I can WRITE geometry in parallel
  #
  # I don't know if that means I can't UPDATE geometry in parallel:
  #   + The test above seems to succeed in using the SQL UPDATE function in parallel
  #   + In practice, though, I still get the 'database is locked' error
  #
  # What to do:
  #
  #   + Speed up UPDATE (try writing a temporary table and then doin a bulk update
  #           https://stackoverflow.com/questions/11563869/update-multiple-rows-with-different-values-in-a-single-sql-query
  #           https://stackoverflow.com/questions/224732/sql-update-from-one-table-to-another-based-on-a-id-match
  #   + Figure out if this is even possible in parallel?

  # PARALLEL

  # # Create VTS
  # my_vts_par <- vts$new(
  #   id         = "my_vts_par",
  #   name       = "My Parallel VTS",
  #   dir        = vts_dir,
  #   geom_type  = "POINT",
  #   geom_layer = "layer"
  # )
  #
  # # Execute in serial
  # results_par <- test_append_geom(my_vts_par, tile_names, clusters = 3)
  #
  # # All nodes were successful
  # expect_true(all(results_par == "Success"))
  #
  # # VTS should NOT be connected
  # expect_error(my_vts_par$con)
  #
  # my_vts_par$connect()
  #
  # # All geometry was created
  # expect_equal(100000, DBI::dbGetQuery(my_vts_par$con, "SELECT COUNT(fid) FROM layer")[,1])
  #
  # # All tiles were registered
  # expect_true(all(DBI::dbGetQuery(my_vts_par$con, "SELECT geom FROM tile_reg")[,1] == 1))
  #
  # # Execute in serial
  # updates_par <- test_update_data (my_vts_par, tile_names, clusters = 3)
  #
  # # All nodes were successful
  # expect_true(all(updates_par == "Success"))

})



