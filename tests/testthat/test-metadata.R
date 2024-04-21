# Copy test data
test_dir <- withr::local_tempdir()
file.copy("data/test_rsds/ndsm", test_dir, recursive = TRUE)

# Test tileset
test_ts <- TileManager::tileLoad("data/test_rsds/tilescheme/tilescheme.gpkg")

# Local environments
withr::local_options(
  misterRS.crs      = 26917,
  misterRS.clusters = 1,
  misterRS.verbose  = FALSE,
  misterRS.ts       = test_ts
)

test_that("Metadata: range", {

  # Create VTS
  my_rts <- rts$new(
    id         = "ndsm",
    name       = "My nDSM",
    dir        = file.path(test_dir, "ndsm")
  )

  # Path for metadata
  meta_data_path <-  file.path(test_dir, "ndsm", "ndsm_metadata.json")

  # JSON shouldn't exist yet
  expect_false(file.exists(meta_data_path))

  # Calculate range
  test_range <- my_rts$metadata("range")

  expect_equal(test_range[[1]][["min"]], 0.0001,  tolerance = 4)
  expect_equal(test_range[[1]][["max"]], 34.5569, tolerance = 4)

  # JSON file should now exist
  expect_true(file.exists(meta_data_path))

  # Read JSON
  test_json <- jsonlite::read_json(meta_data_path)

  # JSON contains correct values
  expect_equal(test_range[[1]][["min"]], test_json[["range"]][[1]][["min"]],  tolerance = 4)
  expect_equal(test_range[[1]][["max"]], test_json[["range"]][[1]][["max"]],  tolerance = 4)

})
