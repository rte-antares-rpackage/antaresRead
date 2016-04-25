context("Function extractDataList")

source("setup_test_case.R")

opts <- setSimulationPath(studyPath)

nodes <- readAntares("all", showProgress=FALSE)

test_that("extractDataList throws a warning if node does not exist", {
  expect_warning(extractDataList(nodes, c("a", "missingNode")))
})

test_that("extractDataList throws an error if all nodes do not exist", {
  expect_error(extractDataList(nodes, c("missingNode")), "node")
})
