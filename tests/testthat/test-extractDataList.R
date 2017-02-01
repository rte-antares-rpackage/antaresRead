#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function extractDataList")

opts <- setSimulationPath(studyPath)

areas <- readAntares("all", showProgress=FALSE)

test_that("extractDataList throws a warning if area does not exist", {
  expect_warning(extractDataList(areas, c("a", "missingArea")))
})

test_that("extractDataList throws an error if all areas do not exist", {
  expect_error(suppressWarnings(extractDataList(areas, c("missingArea")), "area"))
})
