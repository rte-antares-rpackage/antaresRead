#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function readAntares (districts)")
sapply(studyPathS, function(studyPath){
  
opts <- setSimulationPath(studyPath)

suppressWarnings({
  districts <- readAntares(districts = "all", misc=TRUE, hydroStorage = TRUE,
                           hydroStorageMaxPower = TRUE, reserve = TRUE, 
                           mustRun = TRUE,
                           showProgress = FALSE)
})

test_that("readAntares adds misc to districts", {
  expect_false(is.null(districts$Bio_mass))
})

test_that("readAntares adds hydro storage to districts", {
  expect_false(is.null(districts$hydroStorage))
})

test_that("readAntares adds hydro storage max power to districts", {
  expect_false(is.null(districts$hstorPMaxHigh))
})

test_that("readAntares adds reserve to districts", {
  expect_false(is.null(districts$primaryRes))
})

test_that("readAntares adds mustRun to districts", {
  expect_false(is.null(districts$mustRunTotal))
})
})
