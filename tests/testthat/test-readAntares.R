context("Function readAntares")

source("setup_test_case.R")

opts <- setSimulationPath(studyPath, trace=0)

test_that("Nodes importation is ok", {
  nodes <- readAntares(nodes = opts$nodeList, showProgress = FALSE)
  expect_is(nodes, "data.table")
  expect_true(!is.null(nodes$node))
  expect_equal(nrow(nodes), 24 * 7 * 52 * length(opts$nodeList))
})

test_that("Links importation is ok", {
  links <- readAntares(links = opts$linkList, showProgress = FALSE)
  expect_is(links, "data.table")
  expect_true(!is.null(links$link))
  expect_equal(nrow(links), 24 * 7 * 52 * length(opts$linkList))
})

test_that("Clusters importation is ok", {
  clusters <- readAntares(clusters = opts$nodesWithClusters, showProgress = FALSE)
  expect_is(clusters, "data.table")
  expect_true(!is.null(clusters$cluster))
  expect_equal(nrow(clusters), 24 * 7 * 52 * nrow(readClusterDesc()))
})

test_that("Misc importation is ok", {
  misc <- readAntares(misc = opts$nodeList, showProgress = FALSE)
  expect_is(misc, "data.table")
  expect_false(is.null(misc$node))
  expect_equal(nrow(misc), 24 * 7 * 52 * length(opts$nodeList))
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
    nodes <- readAntares(nodes = opts$nodeList, showProgress = FALSE, timeStep = timeStep)
    expect_equal(nrow(nodes), expected_rows * length(opts$nodeList))
  })

  test_that(sprintf("one can import %s Misc input", timeStep), {
    misc <- readAntares(misc = opts$nodeList, showProgress = FALSE, timeStep = timeStep)
    expect_equal(nrow(misc), expected_rows * length(opts$nodeList))
  })
}

test_that("importation of different objects works", {
  out <- readAntares(nodes = opts$nodeList, links=opts$linkList,
                    clusters=opts$nodesWithClusters, showProgress= FALSE, timeStep = "annual")
  expect_is(out, "antaresOutput")
  expect_equal(names(out), c("nodes", "links", "clusters"))
})

test_that("the \"all\" alias is understood", {
  out <- readAntares(nodes = opts$nodeList, links=opts$linkList,
                    clusters=opts$nodesWithClusters, showProgress = FALSE, timeStep = "annual")
  out2 <- readAntares("all", "all", "all", showProgress = FALSE, timeStep = "annual")

  expect_identical(out, out2)
})

test_that("default behavior is fine", {
  nodes <- readAntares(showProgress = FALSE)

  # check simplify
  expect_is(nodes, "data.table")
  # Check node output
  expect_true(!is.null(nodes$node))
  # Check synthetic output
  expect_true(is.null(nodes$mcYear))
  # Check hourly output
  expect_equal(nrow(nodes), 24 * 7 * 52 * length(getOption("antares")$nodeList))
})

test_that("It is possible to select only some columns", {
  out <- readAntares("all", "all", select = c("OP. COST", "FLOW LIN."),
                    timeStep = "annual", showProgress = FALSE)
  expect_equal(names(out$nodes), c("node", "timeId", "OP. COST"))
  expect_equal(names(out$links), c("link", "timeId", "FLOW LIN."))
})

test_that("Aliases for variables work", {
  out <- readAntares("all", select = c("economy"),
                    timeStep = "annual", showProgress = FALSE)
  expect_equal(names(out),
               c("node", "timeId", "OV. COST", "OP. COST", "MRG. PRICE",
                 "CO2 EMIS.", "BALANCE", "SPIL. ENRG"))
})

test_that("Aliases are case incensitive", {
  out <- readAntares("all", select = c("Economy"),
                    timeStep = "annual", showProgress = FALSE)
  expect_equal(names(out),
               c("node", "timeId", "OV. COST", "OP. COST", "MRG. PRICE",
                 "CO2 EMIS.", "BALANCE", "SPIL. ENRG"))
})
