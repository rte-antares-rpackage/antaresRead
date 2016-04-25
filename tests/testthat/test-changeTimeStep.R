context("Function changeTimeStep")

source("setup_test_case.R")

opts <- setSimulationPath(studyPath)

nodes <- readAntares(select = "LOAD", showProgress = FALSE, synthesis = FALSE)
nodes$day <- nodes$month <- nodes$hour <- NULL

for (timeStep in c("daily", "weekly", "monthly", "annual")) {
  
  test_that(sprintf("changeTimeStep aggregates hourly data at %s timestep", timeStep), {
    
    calc <- changeTimeStep(nodes, timeStep, "hourly", opts = opts)
    read <- readAntares(select = "LOAD", timeStep = timeStep, showProgress = FALSE, synthesis = FALSE)
    
    tmp <- merge(calc, read[, .(node, timeId, mcYear, read = LOAD)], by = c("node", "timeId", "mcYear"))
    
    expect_true(tmp[, all(read - LOAD == 0)])
    
  })
  
}

test_that("changeTimeStep keeps attributes", {
  nodesD <- changeTimeStep(nodes, "hourly", opts = opts)
  expect_equal(attr(nodesD, "timeStep"), "hourly")
  expect_equal(attr(nodesD, "synthesis"), attr(nodes, "synthesis"))
  expect_equal(attr(nodesD, "type"), attr(nodes, "type"))
})

test_that("Aggregating desaggregated data should give the initial values.", {
  nodesD <- readAntares(select = "LOAD", showProgress = FALSE, synthesis = FALSE, timeStep = "daily")
  nodesH <- changeTimeStep(nodesD, "hourly")
  nodesD2 <- changeTimeStep(nodesH, "daily")
  expect_equal(nodesD$LOAD, nodesD2$LOAD)
})

test_that("changeTimeStep works on antaresData objects", {
  mydata <- readAntares("all", "all", synthesis = FALSE, showProgress = FALSE)
  mydataAgg <- changeTimeStep(mydata, "daily")
  expect_equal(attr(mydataAgg, "timeStep"), "daily")
  expect_equal(attr(mydataAgg$nodes, "timeStep"), "daily")
  expect_equal(attr(mydataAgg$links, "timeStep"), "daily")
})

