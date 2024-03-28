
if(basename(getwd()) != "testthat") setwd(file.path(getwd(), "tests", "testthat"))

# Test tileset
test_ts <- TileManager::tileScheme(terra::ext(0, 2, 0, 2), dim = c(1,1), crs = sp::CRS(paste0("epsg:2955")))

# Local environments
withr::local_options(
  misterRS.crs        = 2955,
  misterRS.clusters   = 1,
  misterRS.verbosity  = FALSE,
  misterRS.ts = test_ts
)


test_that("Create VTS", {

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

  # Fields created
  expect_equal(c("tile_name", "geom"), DBI::dbListFields(my_vts$con, "tile_reg"))
  expect_equal(c( "fid" , "geom", "tile_name", "poly_id"  ), DBI::dbListFields(my_vts$con, "layer"))

  # Geographic layers created
  expect_equal("layer",    DBI::dbReadTable(my_vts$con, "gpkg_extensions")[1, "table_name"])
  expect_equal("POLYGON",  DBI::dbReadTable(my_vts$con, "gpkg_geometry_columns")[1, "geometry_type_name"])
  expect_equal(2955,       DBI::dbReadTable(my_vts$con, "gpkg_geometry_columns")[1, "srs_id"])

  # Tiles registered
  expect_equal( c("R1C1", "R1C2", "R2C1", "R2C2"), DBI::dbReadTable(my_vts$con, "tile_reg")[,"tile_name"])

  # All new attributes are 0 by default
  expect_true(all(DBI::dbReadTable(my_vts$con, "tile_reg")[,"geom"] == 0))

  # Has tiles
  expect_true(all(my_vts$has_tiles(c("R1C1", "R1C2"), "geom") == FALSE))

  # Error: Non-existent attribute
  expect_error(my_vts$has_tiles(c("R1C1"), "non-existent"),  "Attribute 'non-existent' not found in VTS 'my_vts'")

  # Error: Non-existent tile
  expect_error(my_vts$has_tiles("R1C7", "geom"), "One or more 'tile_names' were missing from tile registry")

  rm(my_vts)
})

test_that("Append geometry to VTS", {

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

  # Add field
  my_vts$add_field("poly_name", field_type = "TEXT")

  # Create generic polygon
  poly_coords <- cbind(c(0,1,1,0,0), c(0,0,1,1,0))
  poly_geom <- sf::st_polygon(list(poly_coords))

  # Create new_data
  data <- sf::st_sf(

    geom = sf::st_as_sfc(
      list(poly_geom, poly_geom, poly_geom),
      crs = sf::st_crs(getOption("misterRS.crs"))
    ),
    poly_id = 1:3,
    poly_name = LETTERS[1:3]
  )

  # Append
  my_vts$append_geom(data, "R1C1")

  # Correct number of rows
  expect_equal(3, DBI::dbGetQuery(my_vts$con, "SELECT COUNT(tile_name) FROM layer")[,1])

  # correct contents
  expect_true(all(DBI::dbGetQuery(my_vts$con, "SELECT tile_name FROM layer")[,"tile_name"] == "R1C1"))
  expect_equal(LETTERS[1:3], DBI::dbGetQuery(my_vts$con, "SELECT poly_name FROM layer")[,"poly_name"])

  # Attempt to append existing
  expect_error(my_vts$append_geom(data, "R1C1"))

  # Stack rows
  extra_data <- data
  extra_data[["poly_id"]] <- 4:6
  extra_data[["poly_name"]] <- LETTERS[4:6]
  extra_data[["tile_name"]] <- "R1C2"

  # Write new rows
  my_vts$append_geom(extra_data, "R1C2")

  # Correct number of rows
  expect_equal(6, DBI::dbGetQuery(my_vts$con, "SELECT COUNT(tile_name) FROM layer")[,1])

  # correct contents
  expect_true(all(c("R1C1", "R1C2") %in% DBI::dbGetQuery(my_vts$con, "SELECT tile_name FROM layer")[,"tile_name"]))
  expect_equal(LETTERS[1:6], DBI::dbGetQuery(my_vts$con, "SELECT poly_name FROM layer")[,"poly_name"])

  rm(my_vts)
})

test_that("Update data for VTS", {

  # Temporary directory
  vts_dir <- withr::local_tempdir()

  # Create VTS
  my_vts <- vts$new(
    id         = "my_vts",
    name       = "My VTS",
    dir        = vts_dir,
    geom_type  = "POINT",
    geom_layer = "layer"
  )

  # Connect
  my_vts$connect()

  # Create generic point
  point_geom <- sf::st_point(c(1,1), dim ="XY")

  # Create new_data
  init_geom <- sf::st_sf(

    geom = sf::st_as_sfc(
      list(point_geom, point_geom, point_geom),
      crs = sf::st_crs(getOption("misterRS.crs"))
    ),
    poly_id = 1:3
  )

  # Append
  my_vts$append_geom(init_geom, "R1C1")
  my_vts$append_geom(init_geom, "R1C2")

  # Add attribute
  my_vts$add_attribute("myattr")

  # Attribute is added
  expect_in("myattr", DBI::dbListFields(my_vts$con, "tile_reg"))

  # All 0s
  expect_true(all(DBI::dbReadTable(my_vts$con, "tile_reg")[["myattr"]] == 0))

  # Add field
  my_vts$add_field("myattr_char", field_type = "TEXT")
  my_vts$add_field("myattr_num",  field_type = "MEDIUMINT")

  # Fields are correct type
  table_info <- DBI::dbGetQuery(my_vts$con, "PRAGMA table_info(layer)")
  expect_equal("TEXT",      table_info[table_info$name == "myattr_char", "type"])
  expect_equal("MEDIUMINT", table_info[table_info$name == "myattr_num",   "type"])

  # Current data
  tile_data <- my_vts$read_tile("R1C1")

  # No data yet
  expect_true(all(is.na(tile_data[["myattr_char"]])))
  expect_true(all(is.na(tile_data[["myattr_num"]])))


  # Updated data
  update_data <- data.frame(
    poly_id     = 1:3,
    myattr_char = letters[1:3],
    myattr_num  = 1:3 * 10
  )

  # Execute update
  my_vts$update_data(update_data, tile_name = "R1C1", attribute = "myattr")

  # Check results
  tile_data <- my_vts$read_tile("R1C1")
  expect_equal(c(10, 20, 30),  tile_data$myattr_num)
  expect_equal(c("a","b","c"), tile_data$myattr_char)

  # Make sure that only the targeted tile was updated
  untouched_tile_data <- my_vts$read_tile("R1C2")
  expect_true(all(is.na(untouched_tile_data[["myattr_char"]])))
  expect_true(all(is.na(untouched_tile_data[["myattr_num"]])))

  # Try updating without overwrite
  expect_error(my_vts$update_data(update_data, tile_name = "R1C1", attribute = "myattr"))

  # Mix up order of rows
  mixed_data <- update_data[c(3,1,2), c("poly_id", "myattr_num")]
  mixed_data[["myattr_num"]] <- 4:6 * 10

  # Execute update
  my_vts$update_data(mixed_data, tile_name = "R1C1", attribute = "myattr", overwrite = TRUE)

  # Check results
  tile_data <- my_vts$read_tile("R1C1")
  expect_equal(c(50, 60, 40),  tile_data$myattr_num)
  expect_equal(c("a","b","c"), tile_data$myattr_char)

  rm(my_vts)
})

