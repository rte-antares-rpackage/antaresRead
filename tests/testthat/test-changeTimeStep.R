#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function changeTimeStep")
sapply(studyPathS, function(studyPath){
  

opts <- setSimulationPath(studyPath)

areas <- readAntares(select = "LOAD", showProgress = FALSE, mcYears = "all")
areas$day <- areas$month <- areas$hour <- NULL

for (timeStep in c("daily", "weekly", "monthly", "annual")) {
  
  test_that(sprintf("changeTimeStep aggregates hourly data at %s timestep", timeStep), {
    
    calc <- suppressWarnings(changeTimeStep(areas, timeStep, "hourly", opts = opts))
    read <- readAntares(select = "LOAD", timeStep = timeStep, showProgress = FALSE, mcYears = "all")
    
    tmp <- merge(calc, read[, .(area, timeId, mcYear, read = LOAD)], by = c("area", "timeId", "mcYear"))
    
    expect_true(tmp[, all(read - LOAD == 0)])
    
  })
  
}

test_that("changeTimeStep keeps attributes", {
  areasD <- changeTimeStep(areas, "hourly", opts = opts)
  expect_equal(attr(areasD, "timeStep"), "hourly")
  expect_equal(attr(areasD, "synthesis"), attr(areas, "synthesis"))
  expect_equal(attr(areasD, "type"), attr(areas, "type"))
})

test_that("Aggregating desaggregated data should give the initial values.", {
  areasD <- readAntares(select = "LOAD", showProgress = FALSE, mcYears = "all", timeStep = "daily")
  areasH <- changeTimeStep(areasD, "hourly")
  areasD2 <- suppressWarnings(changeTimeStep(areasH, "daily"))
  setorder(areasD, timeId, area, mcYear)
  setorder(areasD2, timeId, area, mcYear)
  expect_equal(areasD, areasD2)
})

test_that("changeTimeStep works on antaresData objects", {
  mydata <- readAntares("all", "all", mcYears = "all", showProgress = FALSE)
  mydataAgg <- suppressWarnings(changeTimeStep(mydata, "daily"))
  expect_equal(attr(mydataAgg, "timeStep"), "daily")
  expect_equal(attr(mydataAgg$areas, "timeStep"), "daily")
  expect_equal(attr(mydataAgg$links, "timeStep"), "daily")
})

})
