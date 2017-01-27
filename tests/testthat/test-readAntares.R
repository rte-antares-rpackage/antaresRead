#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function readAntares")

opts <- setSimulationPath(studyPath)

test_that("Areas importation is ok", {
  areas <- readAntares(areas = opts$areaList, showProgress = FALSE)
  expect_is(areas, "data.table")
  expect_true(!is.null(areas$area))
  expect_equal(nrow(areas), 24 * 7 * nweeks * length(opts$areaList))
})

test_that("Links importation is ok", {
  links <- readAntares(links = opts$linkList, showProgress = FALSE)
  expect_is(links, "data.table")
  expect_true(!is.null(links$link))
  expect_equal(nrow(links), 24 * 7 * nweeks * length(opts$linkList))
})

test_that("Clusters importation is ok", {
  clusters <- readAntares(clusters = opts$areasWithClusters, showProgress = FALSE)
  expect_is(clusters, "data.table")
  expect_true(!is.null(clusters$cluster))
  expect_equal(nrow(clusters), 24 * 7 * nweeks * nrow(readClusterDesc()))
})

test_that("importation of different objects works", {
  out <- readAntares(areas = opts$areaList, links=opts$linkList,
                    clusters=opts$areasWithClusters, showProgress= FALSE, timeStep = "annual")
  expect_is(out, "antaresData")
  expect_equal(names(out), c("areas", "links", "clusters"))
})

# Test that importation works for all time resolutions.
for (timeStep in c("hourly", "daily", "weekly", "monthly", "annual")) {
  expected_rows = switch(timeStep,
                         hourly = 24 * 7 * nweeks,
                         daily = 7 * nweeks,
                         weekly = nweeks,
                         monthly = 1,
                         annual = 1)

  test_that(sprintf("one can import %s output", timeStep), {
    areas <- readAntares(areas = opts$areaList, showProgress = FALSE, timeStep = timeStep)
    expect_equal(nrow(areas), expected_rows * length(opts$areaList))
  })
}

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
  expect_equal(nrow(areas), 24 * 7 * nweeks * length(getOption("antares")$areaList))
})

test_that("It is possible to select only some columns", {
  out <- readAntares("all", "all", select = c("OP. COST", "FLOW LIN."),
                    timeStep = "annual", showProgress = FALSE)
  expect_equal(names(out$areas), c("area", "timeId", "time", "OP. COST"))
  expect_equal(names(out$links), c("link", "timeId", "time", "FLOW LIN."))
})

test_that("Aliases for variables work", {
  out <- readAntares("all", select = c("economy"),
                    timeStep = "annual", showProgress = FALSE)
  expect_equal(names(out),
               c("area", "timeId", "time", "OV. COST", "OP. COST", "MRG. PRICE",
                 "CO2 EMIS.", "BALANCE", "SPIL. ENRG"))
})

test_that("Aliases are case incensitive", {
  out <- readAntares("all", select = c("Economy"),
                    timeStep = "annual", showProgress = FALSE)
  expect_equal(names(out),
               c("area", "timeId", "time", "OV. COST", "OP. COST", "MRG. PRICE",
                 "CO2 EMIS.", "BALANCE", "SPIL. ENRG"))
})
