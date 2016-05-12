context("Function readAntares (add inputs)")

source("setup_test_case.R")

opts <- setSimulationPath(studyPath)

test_that("Misc importation is ok", {
  suppressWarnings(misc <- readAntares(areas="all", misc = TRUE, showProgress = FALSE))
  expect_is(misc, "data.table")
  expect_false(is.null(misc$area))
  expect_equal(nrow(misc), 24 * 7 * 52 * length(opts$areaList))
})

test_that("Thermal availability importation is ok", {
  suppressWarnings(output <- readAntares(clusters = "all", thermalAvailabilities = TRUE, showProgress = FALSE))
  expect_is(output, "data.table")
  expect_false(is.null(output$area))
  expect_equal(nrow(output), 24 * 7 * 52 * nrow(readClusterDesc()))
})

test_that("Hydro storage importation is ok", {
  suppressWarnings(output <- readAntares(areas="all", hydroStorage = TRUE, showProgress = FALSE))
  expect_is(output, "data.table")
  expect_false(is.null(output$area))
  expect_equal(nrow(output), 24 * 7 * 52 * length(opts$areaList))
})

test_that("Hydro storage maximum power is ok", {
  suppressWarnings(output <- readAntares(areas="all", hydroStorageMaxPower = TRUE, showProgress = FALSE))
  expect_is(output, "data.table")
  expect_false(is.null(output$area))
  expect_equal(nrow(output), 24 * 7 * 52 * length(opts$areaList))
})

test_that("Reserve is ok", {
  suppressWarnings(output <- readAntares(areas="all", reserve = TRUE, showProgress = FALSE))
  expect_is(output, "data.table")
  expect_false(is.null(output$area))
  expect_equal(nrow(output), 24 * 7 * 52 * length(opts$areaList))
})

test_that("Link capacity is ok", {
  suppressWarnings(output <- readAntares(links="all", linkCapacity = TRUE, showProgress = FALSE))
  expect_is(output, "data.table")
  expect_false(is.null(output$link))
  expect_equal(nrow(output), 24 * 7 * 52 * length(opts$linkList))
})

for (timeStep in c("hourly", "daily", "weekly", "monthly", "annual")) {
  expected_rows = switch(timeStep,
                         hourly = 24 * 7 * 52,
                         daily = 7 * 52,
                         weekly = 52,
                         monthly = 12,
                         annual = 1)
  
  test_that(sprintf("one can import %s Misc input", timeStep), {
    suppressWarnings(misc <- readAntares(misc = TRUE, showProgress = FALSE, timeStep = timeStep))
    expect_equal(nrow(misc), expected_rows * length(opts$areaList))
  })
}
