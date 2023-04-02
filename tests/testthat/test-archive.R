if(basename(getwd()) != "testthat") setwd("tests/testthat")

test_that("Projects are archived correctly", {

  # Environment
  local_misterRS_env()

  # Temporary directory
  temp_dir <- withr::local_tempdir()

  # Set directories
  datasets <- c("ndsm", "ortho") %>% setNames(.,.)
  in_dirs <- lapply(datasets, function(dataset) file.path("test_rsds", dataset))
  for(dir in in_dirs) file.copy(dir, temp_dir, recursive = TRUE)
  out_dirs <- lapply(datasets, function(dataset) file.path(temp_dir, dataset))

  # Create RSDS
  rsds_list <- list(
    rsds(id =  "ndsm", name = "nDSM",  dir = out_dirs$ndsm,  ext = "tif", archive = FALSE),
    rsds(id = "ortho", name = "Ortho", dir = out_dirs$ortho, ext = "tif", archive = TRUE)
  )
  names(rsds_list) <- sapply(rsds_list, slot, "id")

  # All files exist (initially)
  expect_true(all(sapply(rsds_list, function(rs) all(file.exists(misterRS:::.get_rsds_tilepaths(rs))))))

  # Run archiving function
  archive_stack(rsds_list, prompt = FALSE)

  # Archived dataset tiles have been retained
  expect_true(all(file.exists(misterRS:::.get_rsds_tilepaths(rsds_list[["ortho"]]))))

  # Archived dataset mosaic has been removed
  expect_false(file.exists(misterRS:::.get_rsds_mosaicpath(rsds_list[["ortho"]])))

  # Non-archived dataset is completely gone
  expect_false(dir.exists(rsds_list[["ndsm"]]@dir))
})

