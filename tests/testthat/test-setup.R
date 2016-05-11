
context("Setup functions")

source("setup_test_case.R")

trueOpts <- list(
  studyName = "Test_packages_R",
  opath = "economy",
  name = "test",
  mode = "Economy",
  synthesis = TRUE,
  yearByYear = TRUE,
  scenarios = TRUE,
  mcYears = 2,
  antaresVersion = 500L,
  start = as.POSIXlt("2018-01-01", tz = "UTC"),
  firstWeekday = "Monday",
  areaList = c("a", "a_offshore", "b", "c", "hub", "psp in", "psp in-2", "psp out", 
               "psp out-2"),
  setList = "@ a and b together",
  linkList = c("a - a_offshore", "a - b", "a - psp in", "a - psp out", "b - c", 
               "b - psp in", "b - psp out", "c - hub", "hub - psp in-2", 
               "hub - psp out-2"),
  areasWithClusters = c("a", "b", "c", "psp in", "psp out")
)
class(trueOpts) <- "simOptions"

test_that("setSimulationPath reads correct values", {
  opts <- setSimulationPath(studyPath)
  opts$variables <- opts$path <- opts$parameters <- opts$inputPath <- opts$studyPath <- NULL
  expect_equal(opts, trueOpts)
})

test_that("R option 'antares' is set", {
  opts <- setSimulationPath(studyPath)
  expect_identical(opts, getOption("antares"))
})

test_that("setSimulationPath fails if path is not an antares Ouput directory", {
  expect_error(setSimulationPath(file.path(studyPath, "..")))
})
