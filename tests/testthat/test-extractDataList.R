context("Function extractDataList")

source("setup_test_case.R")

opts <- setSimulationPath(studyPath)

areas <- readAntares("all", showProgress=FALSE)

test_that("extractDataList throws a warning if area does not exist", {
  expect_warning(extractDataList(areas, c("a", "missingArea")))
})

test_that("extractDataList throws an error if all areas do not exist", {
  expect_error(extractDataList(areas, c("missingArea")), "area")
})
