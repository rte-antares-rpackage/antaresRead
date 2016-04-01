
context("Setup functions")

source("setup_test_case.R")

trueOpts <- list(
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
  nodeList = c("a", "b", "c", "psp in", "psp out"),
  setList = "@ a and b together",
  linkList = c("a - b", "b - c", "b - psp in", "b - psp out"),
  nodesWithClusters = c("a", "b", "c")
)

test_that("setSimulationPath reads correct values", {
  opts <- setSimulationPath(studyPath, trace = 0)
  opts$variables <- opts$path <- opts$parameters <- NULL
  expect_equal(opts, trueOpts)
})

test_that("R option 'antares' is set", {
  opts <- setSimulationPath(studyPath, trace = 0)
  expect_identical(opts, getOption("antares"))
})

test_that("setSimulationPath fails if path is not an antares Ouput directory", {
  expect_error(setSimulationPath(file.path(studyPath, ..), trace=0))
})
