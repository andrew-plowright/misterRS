
test_that(".tile_neib", {

  test_crs <- 2955

  # Test tileset
  ts <- TileManager::tileScheme(terra::ext(0, 5, 0, 5), dim = c(1,1), crs = sp::CRS(paste0("epsg:", test_crs)))

  # Queen's case
  expect_equal(
    .tile_neibs("R3C3", ts, "queen"),
    c("R2C2", "R2C3", "R2C4" ,"R3C2", "R3C3", "R3C4","R4C2", "R4C3", "R4C4")
  )

  # Rook's case
  expect_equal(
    .tile_neibs("R3C3", ts, "rook"),
    c("R2C3", "R3C2", "R3C3", "R3C4", "R4C3")
  )

  # Corner
  expect_equal(
    .tile_neibs("R1C1", ts, "queen"),
    c("R1C1", "R1C2", "R2C1", "R2C2")
  )

  # Multiple
  expect_equal(
    .tile_neibs(c("R1C1", "R1C2", "R2C1"), ts, "queen"),
    c("R1C1", "R1C2", "R1C3", "R2C1", "R2C2", "R2C3", "R3C1", "R3C2")
  )

})


