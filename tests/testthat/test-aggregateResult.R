

context("Function aggregateResult")

pathstd <- tempdir()

test_case_aggregate <- system.file("test_agg", package = "antaresRead")

studies <- list.files(test_case_aggregate, pattern = "\\.tar\\.gz$", full.names = TRUE)

test_case_aggregate <- untar(studies[1], exdir = pathstd)

opts <- antaresRead::setSimulationPath(file.path(pathstd,"test_case_aggregate"), -1)


test_that("test parallel aggregate", {
  
  mc_all_path <- file.path(opts$simDataPath, "mc-all", "grid", "digest.txt")
  
  mdate_original <- file.mtime(mc_all_path)
  
  parAggregateMCall(opts, mcYears = c(1,2), verbose = 0)
  
  expect_true(file.exists(mc_all_path))
  expect_false(file.mtime(mc_all_path) == mdate_original)
  expect_true(file.size(mc_all_path) > 0)
  
  mdate_original <- file.mtime(mc_all_path)
  
  parAggregateMCall(opts, nbcl = 1, mcYears = c(1,2), verbose = 0)
  
  expect_true(file.exists(mc_all_path))
  expect_false(file.mtime(mc_all_path) == mdate_original)
  expect_true(file.size(mc_all_path) > 0)
  
})
  

  

  