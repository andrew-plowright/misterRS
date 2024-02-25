
test_ts_path <- "data/test_rsds/tilescheme/tile_scheme.rds"
test_ts <- readRDS(test_ts_path)


test_that("Create VTS", {

  # Local environments
  withr::local_options(
    misterRS.crs        = 2955,
    misterRS.clusters   = 1,
    misterRS.verbosity  = FALSE,
    misterRS.ts = test_ts
  )

  # Temporary directory
  vts_dir <- withr::local_tempdir()

  # Create VTS
  my_vts <- vts$new(
    id         = "my_vts",
    name       = "My VTS",
    dir        = vts_dir,
    geom_type  = "POLYGON",
    geom_layer = "layer"
  )

  # Error: No connection
  expect_error(my_vts$con, "VTS 'my_vts' is not connected")

  # Connect
  my_vts$connect()

  # Connection made
  expect_s4_class(my_vts$con, "SQLiteConnection")

  # Tables created
  expect_in("tile_reg", DBI::dbListTables(my_vts$con))
  expect_in("layer",    DBI::dbListTables(my_vts$con))

  # Geographic layers created
  expect_equal("layer",    DBI::dbReadTable(my_vts$con, "gpkg_extensions")[1, "table_name"])
  expect_equal("POLYGON",  DBI::dbReadTable(my_vts$con, "gpkg_geometry_columns")[1, "geometry_type_name"])
  expect_equal(2955,       DBI::dbReadTable(my_vts$con, "gpkg_geometry_columns")[1, "srs_id"])

  # Tiles registered
  expect_equal( c("R1C1", "R1C2", "R2C1", "R2C2"), DBI::dbReadTable(my_vts$con, "tile_reg")[,"tile_name"])

  # Add attribute
  my_vts$add_attribute(attribute = "poly")

  # All new attributes are 0 by default
  expect_true(all(DBI::dbReadTable(my_vts$con, "tile_reg")[,"poly"] == 0))

  # Has tiles
  expect_true(all(my_vts$has_tiles(c("R1C1", "R1C2"), "poly") == FALSE))

  # Error: Non-existent attribute
  expect_error(my_vts$has_tiles(c("R1C1"), "non-existent"),  "Attribute 'non-existent' not found in VTS 'my_vts'")

  # Error: Non-existent tile
  expect_error(my_vts$has_tiles("R1C7", "poly"), "One or more 'tile_names' were missing from tile registry")

  rm(my_vts)
})

test_that("Write to VTS", {

  # Local environments
  withr::local_options(
    misterRS.crs        = 2955,
    misterRS.clusters   = 1,
    misterRS.verbosity  = FALSE,
    misterRS.ts = test_ts
  )

  # Temporary directory
  vts_dir <- withr::local_tempdir()

  # Create VTS
  my_vts <- vts$new(
    id         = "my_vts",
    name       = "My VTS",
    dir        = vts_dir,
    geom_type  = "POLYGON",
    geom_layer = "layer"
  )

  # Connect
  my_vts$connect()

  # Add attribute
  my_vts$add_attribute(attribute = "poly")

  # Add field
  my_vts$add_field("poly_id",   field_type = "MEDIUMINT")
  my_vts$add_field("poly_name", field_type = "TEXT")

  # Fields added
  expect_in("poly_id",   DBI::dbListFields(my_vts$con, "layer"))
  expect_in("poly_name", DBI::dbListFields(my_vts$con, "layer"))

  # Fields are correct type
  table_info <- DBI::dbGetQuery(my_vts$con, "PRAGMA table_info(layer)")
  expect_equal("MEDIUMINT", table_info[table_info$name == "poly_id",   "type"])
  expect_equal("TEXT",      table_info[table_info$name == "poly_name", "type"])

  # Create generic polygon
  poly_coords <- cbind(c(0,1,1,0,0), c(0,0,1,1,0))
  poly_geom <- sf::st_polygon(list(poly_coords))

  # Create new_data
  data <- sf::st_sf(

    geom = sf::st_as_sfc(
      list(poly_geom, poly_geom, poly_geom),
      crs = sf::st_crs(getOption("misterRS.crs"))
    ),
    tile_name = "R1C1",
    poly_id = 1:3,
    poly_name = LETTERS[1:3]
  )

  # Write
  my_vts$write_tile(data, "R1C1", "poly", overwrite = FALSE)

  # Correct number of rows
  expect_equal(3, DBI::dbGetQuery(my_vts$con, "SELECT COUNT(tile_name) FROM layer")[,1])

  # correct contents
  expect_true(all(DBI::dbGetQuery(my_vts$con, "SELECT tile_name FROM layer")[,"tile_name"] == "R1C1"))
  expect_equal(LETTERS[1:3], DBI::dbGetQuery(my_vts$con, "SELECT poly_name FROM layer")[,"poly_name"])


  # Attempt overwrite without setting 'overwrite' to TRUE
  expect_error(
    my_vts$write_tile(data, "R1C1", "poly", overwrite = FALSE)
  )


  # New data for overwriting
  overwrite_data <- data[1:2,]
  overwrite_data[["poly_name"]] <- LETTERS[5:6]

  # Overwrite
  my_vts$write_tile(overwrite_data, "R1C1", "poly", overwrite = TRUE)

  # Correct number of rows
  expect_equal(2, DBI::dbGetQuery(my_vts$con, "SELECT COUNT(tile_name) FROM layer")[,1])

  # correct contents
  expect_true(all(DBI::dbGetQuery(my_vts$con, "SELECT tile_name FROM layer")[,"tile_name"] == "R1C1"))
  expect_equal(LETTERS[5:6], DBI::dbGetQuery(my_vts$con, "SELECT poly_name FROM layer")[,"poly_name"])


  # Stack rows
  extra_data <- data
  extra_data[["poly_id"]] <- 4:6
  extra_data[["poly_name"]] <- LETTERS[7:9]
  extra_data[["tile_name"]] <- "R1C2"

  # Write new rows
  my_vts$write_tile(extra_data, "R1C2", "poly", overwrite = FALSE)

  # Correct number of rows
  expect_equal(5, DBI::dbGetQuery(my_vts$con, "SELECT COUNT(tile_name) FROM layer")[,1])

  # correct contents
  expect_true(all(c("R1C1", "R1C2") %in% DBI::dbGetQuery(my_vts$con, "SELECT tile_name FROM layer")[,"tile_name"]))
  expect_equal(LETTERS[5:9], DBI::dbGetQuery(my_vts$con, "SELECT poly_name FROM layer")[,"poly_name"])

  rm(my_vts)
})
