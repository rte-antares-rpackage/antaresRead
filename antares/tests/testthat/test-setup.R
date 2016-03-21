context("Setup functions")

studyPath <- system.file("sampleStudy/output/20160317-1558eco-test-eco", package = "antares")

trueOpts <- list(
  path = studyPath,
  opath = "economy",
  name = "test-eco",
  mode = "Economy",
  synthesis = TRUE,
  yearByYear = TRUE,
  scenarios = TRUE,
  mcYears = 2,
  antaresVersion = 500L,
  nodeList = c("a", "b", "psp in", "psp out"),
  setList = "@ a and b together",
  linkList = c("a - b", "b - psp in", "b - psp out"),
  nodesWithClusters = c("a", "b")
)

test_that("setSimulationPath reads correct values", {
  opts <- setSimulationPath(studyPath, trace = 0)
  opts$variables <- opts$start <- NULL
  expect_equal(opts, trueOpts)
})

test_that("R option 'antares' is set", {
  opts <- setSimulationPath(studyPath, trace = 0)
  expect_identical(opts, getOption("antares"))
})

test_that("setSimulationPath fails if path is not an antares Ouput directory", {
  expect_error(setSimulationPath(file.path(studyPath, ..), trace=0))
})
