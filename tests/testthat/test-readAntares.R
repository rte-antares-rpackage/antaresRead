context("Function readAntares")

source("setup_test_case.R")

opts <- setSimulationPath(studyPath)

test_that("Areas importation is ok", {
  areas <- readAntares(areas = opts$areaList, showProgress = FALSE)
  expect_is(areas, "data.table")
  expect_true(!is.null(areas$area))
  expect_equal(nrow(areas), 24 * 7 * 52 * length(opts$areaList))
})

test_that("Links importation is ok", {
  links <- readAntares(links = opts$linkList, showProgress = FALSE)
  expect_is(links, "data.table")
  expect_true(!is.null(links$link))
  expect_equal(nrow(links), 24 * 7 * 52 * length(opts$linkList))
})

test_that("Clusters importation is ok", {
  clusters <- readAntares(clusters = opts$areasWithClusters, showProgress = FALSE)
  expect_is(clusters, "data.table")
  expect_true(!is.null(clusters$cluster))
  expect_equal(nrow(clusters), 24 * 7 * 52 * nrow(readClusterDesc()))
})

test_that("Misc importation is ok", {
  misc <- readAntares(areas="all", misc = TRUE, showProgress = FALSE)
  expect_is(misc, "data.table")
  expect_false(is.null(misc$area))
  expect_equal(nrow(misc), 24 * 7 * 52 * length(opts$areaList))
})

test_that("Thermal availability importation is ok", {
  output <- readAntares(clusters = "all", thermalAvailabilities = TRUE, showProgress = FALSE)
  expect_is(output, "data.table")
  expect_false(is.null(output$area))
  expect_equal(nrow(output), 24 * 7 * 52 * nrow(readClusterDesc()))
})

test_that("Hydro storage importation is ok", {
  output <- readAntares(areas="all", hydroStorage = TRUE, showProgress = FALSE)
  expect_is(output, "data.table")
  expect_false(is.null(output$area))
  expect_equal(nrow(output), 24 * 7 * 52 * length(opts$areaList))
})

test_that("Hydro storage maximum power is ok", {
  output <- readAntares(areas="all", hydroStorageMaxPower = TRUE, showProgress = FALSE)
  expect_is(output, "data.table")
  expect_false(is.null(output$area))
  expect_equal(nrow(output), 24 * 7 * 52 * length(opts$areaList))
})

test_that("Reserve is ok", {
  output <- readAntares(areas="all", reserve = TRUE, showProgress = FALSE)
  expect_is(output, "data.table")
  expect_false(is.null(output$area))
  expect_equal(nrow(output), 24 * 7 * 52 * length(opts$areaList))
})

test_that("Link capacity is ok", {
  output <- readAntares(links="all", linkCapacity = TRUE, showProgress = FALSE)
  expect_is(output, "data.table")
  expect_false(is.null(output$link))
  expect_equal(nrow(output), 24 * 7 * 52 * length(opts$linkList))
})

test_that("mustRun and mustRunPartial are ok", {
  output <- readAntares(areas = "all", clusters = "all", mustRun = TRUE, showProgress = FALSE)
  areas <- readAntares(areas = "all", mustRun = TRUE, showProgress = FALSE)
  clusters <- readAntares(clusters = "all", mustRun = TRUE, showProgress = FALSE)
  
  # Check columns mustRun(..) have been added
  expect_false(is.null(output$areas$mustRun))
  expect_false(is.null(output$areas$mustRunPartial))
  expect_equal(output$areas$mustRun + output$areas$mustRunPartial, output$areas$mustRunTotal)
  expect_false(is.null(output$clusters$mustRun))
  expect_false(is.null(output$clusters$mustRunPartial))
  expect_equal(output$clusters$mustRun + output$clusters$mustRunPartial, output$clusters$mustRunTotal)
  
  expect_equal(nrow(output$areas), 24 * 7 * 52 * length(opts$areaList))
  
  expect_gt(sum(clusters$mustRun), 0)
  expect_gt(sum(clusters$mustRunPartial), 0)
  
  expect_true(clusters[cluster == "base_must_run", all(production == mustRun)])
  expect_true(clusters[cluster != "base_must_run", all(mustRun == 0)])
  
  expect_true(clusters[cluster == "peak_must_run_partial", all(production >= mustRunPartial)])
  expect_true(clusters[cluster != "peak_must_run_partial", all(mustRunPartial == 0)])
})

# Test that importation works for all time resolutions.
for (timeStep in c("hourly", "daily", "weekly", "monthly", "annual")) {
  expected_rows = switch(timeStep,
                         hourly = 24 * 7 * 52,
                         daily = 7 * 52,
                         weekly = 52,
                         monthly = 12,
                         annual = 1)

  test_that(sprintf("one can import %s output", timeStep), {
    areas <- readAntares(areas = opts$areaList, showProgress = FALSE, timeStep = timeStep)
    expect_equal(nrow(areas), expected_rows * length(opts$areaList))
  })

  test_that(sprintf("one can import %s Misc input", timeStep), {
    misc <- readAntares(misc = TRUE, showProgress = FALSE, timeStep = timeStep)
    expect_equal(nrow(misc), expected_rows * length(opts$areaList))
  })
}

test_that("importation of different objects works", {
  out <- readAntares(areas = opts$areaList, links=opts$linkList,
                    clusters=opts$areasWithClusters, showProgress= FALSE, timeStep = "annual")
  expect_is(out, "antaresData")
  expect_equal(names(out), c("areas", "links", "clusters"))
})

test_that("the \"all\" alias is understood", {
  out <- readAntares(areas = opts$areaList, links=opts$linkList,
                    clusters=opts$areasWithClusters, showProgress = FALSE, timeStep = "annual")
  out2 <- readAntares("all", "all", "all", showProgress = FALSE, timeStep = "annual")

  expect_identical(out, out2)
})

test_that("default behavior is fine", {
  areas <- readAntares(showProgress = FALSE)

  # check simplify
  expect_is(areas, "data.table")
  # Check area output
  expect_true(!is.null(areas$area))
  # Check synthetic output
  expect_true(is.null(areas$mcYear))
  # Check hourly output
  expect_equal(nrow(areas), 24 * 7 * 52 * length(getOption("antares")$areaList))
})

test_that("It is possible to select only some columns", {
  out <- readAntares("all", "all", select = c("OP. COST", "FLOW LIN."),
                    timeStep = "annual", showProgress = FALSE)
  expect_equal(names(out$areas), c("area", "timeId", "OP. COST"))
  expect_equal(names(out$links), c("link", "timeId", "FLOW LIN."))
})

test_that("Aliases for variables work", {
  out <- readAntares("all", select = c("economy"),
                    timeStep = "annual", showProgress = FALSE)
  expect_equal(names(out),
               c("area", "timeId", "OV. COST", "OP. COST", "MRG. PRICE",
                 "CO2 EMIS.", "BALANCE", "SPIL. ENRG"))
})

test_that("Aliases are case incensitive", {
  out <- readAntares("all", select = c("Economy"),
                    timeStep = "annual", showProgress = FALSE)
  expect_equal(names(out),
               c("area", "timeId", "OV. COST", "OP. COST", "MRG. PRICE",
                 "CO2 EMIS.", "BALANCE", "SPIL. ENRG"))
})
