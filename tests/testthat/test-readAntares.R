#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function readAntares")

sapply(studyPathS, function(studyPath){
  

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

test_that("'select' can be used to import input columns", {
  for (v in c("misc", "thermalAvailabilities", "hydroStorage", 
              "hydroStorageMaxPower", "reserve", "linkCapacity", "mustRun",
              "thermalModulation")) {
    baseParams <- list(areas = "all", links = "all", clusters = "all",
                       timeStep = "annual", showProgress = FALSE)
    params <- baseParams
    params$select <- ""
    params[[v]] <- TRUE
    
    params2 <- baseParams
    params2$select = v
    
    suppressWarnings({
      mydata <- do.call(readAntares, params)
      mydata2 <- do.call(readAntares, params2)
    })
    expect_equal(mydata, mydata2)
  }
})

test_that("'select' can be used to import link, area, cluster, detailed data", {
  for (v in c("areas", "links", "clusters", "mcYears")) {
    params <- list(select = "", timeStep = "annual", showProgress = FALSE)
    params[[v]] <- "all"
    
    params2 <- list(select = v, timeStep = "annual", showProgress = FALSE)
    
    suppressWarnings({
      mydata <- do.call(readAntares, params)
      mydata2 <- do.call(readAntares, params2)
    })
    expect_equal(mydata, mydata2)
  }
})

test_that("import only data related to some areas with 'select'", {
  mydata <- readAntares(areas = "a", select = c("links", "clusters"), 
                        timeStep = "annual", showProgress = FALSE)
  mydata2 <- readAntares(areas = "a", links = getLinks("a"), clusters = "a",
                         timeStep = "annual", showProgress = FALSE, select = "")
  expect_equal(mydata, mydata2)
})

describe("readAntaresAreas", {
  it("returns data for a set of areas", {
    mydata <- readAntaresAreas("a", showProgress = FALSE)
    expect_equal(names(mydata), c("areas", "links", "clusters"))
    expect_true(all(mydata$areas$area == "a"))
    expect_true(all(mydata$links$link %in% getLinks("a")))
    expect_true(all(mydata$clusters$area == "a"))
  })
  
  it ("throws error if no area specified", {
    expect_error(readAntaresAreas(showProgress = FALSE), "area")
  })
})

})
