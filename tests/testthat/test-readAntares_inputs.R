#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function readAntares (add inputs)")

sapply(studyPathS, function(studyPath){
  
opts <- setSimulationPath(studyPath)
  
test_that("Misc importation is ok", {
  suppressWarnings(misc <- readAntares(areas="all", misc = TRUE, showProgress = FALSE))
  expect_is(misc, "data.table")
  expect_false(is.null(misc$area))
  expect_equal(nrow(misc), 24 * 7 * nweeks * length(opts$areaList))
})

test_that("Thermal availability importation is ok", {
  suppressWarnings(output <- readAntares(clusters = "all", thermalAvailabilities = TRUE, showProgress = FALSE))
  expect_is(output, "data.table")
  expect_false(is.null(output$area))
  expect_equal(nrow(output), 24 * 7 * nweeks * nrow(readClusterDesc()))
})

test_that("Hydro storage importation is ok", {
  suppressWarnings(output <- readAntares(areas="all", hydroStorage = TRUE, showProgress = FALSE))
  expect_is(output, "data.table")
  expect_false(is.null(output$area))
  expect_equal(nrow(output), 24 * 7 * nweeks * length(opts$areaList))
})

test_that("Hydro storage maximum power is ok", {
  suppressWarnings(output <- readAntares(areas="all", hydroStorageMaxPower = TRUE, showProgress = FALSE))
  expect_is(output, "data.table")
  expect_false(is.null(output$area))
  expect_equal(nrow(output), 24 * 7 * nweeks * length(opts$areaList))
})

test_that("Reserve is ok", {
  suppressWarnings(output <- readAntares(areas="all", reserve = TRUE, showProgress = FALSE))
  expect_is(output, "data.table")
  expect_false(is.null(output$area))
  expect_equal(nrow(output), 24 * 7 * nweeks * length(opts$areaList))
})

test_that("Link capacity is ok", {
  suppressWarnings(output <- readAntares(links="all", linkCapacity = TRUE, showProgress = FALSE))
  expect_is(output, "data.table")
  expect_false(is.null(output$link))
  expect_equal(nrow(output), 24 * 7 * nweeks * length(opts$linkList))
})

for (timeStep in c("hourly", "daily", "weekly", "monthly", "annual")) {
  expected_rows = switch(timeStep,
                         hourly = 24 * 7 * nweeks,
                         daily = 7 * nweeks,
                         weekly = nweeks,
                         monthly = nmonths,
                         annual = 1)
  
  test_that(sprintf("one can import %s Misc input", timeStep), {
    suppressWarnings(misc <- readAntares(misc = TRUE, showProgress = FALSE, timeStep = timeStep))
    expect_equal(nrow(misc), expected_rows * length(opts$areaList))
  })
}
})
