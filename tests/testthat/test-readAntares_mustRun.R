#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function readAntares (mustRun)")
sapply(studyPathS, function(studyPath){
  
opts <- setSimulationPath(studyPath)

output <- readAntares(areas = "all", clusters = "all", mustRun = TRUE, 
                      showProgress = FALSE, select = "")
areas <- readAntares(areas = "all", mustRun = TRUE, showProgress = FALSE,
                     select = "")
clusters <- readAntares(clusters = "all", mustRun = TRUE, showProgress = FALSE)

test_that("Columns mustRun* have been added", {
  expect_false(is.null(output$areas$mustRun))
  expect_false(is.null(output$areas$mustRunPartial))
  expect_equal(output$areas$mustRun + output$areas$mustRunPartial, output$areas$mustRunTotal)
  expect_false(is.null(output$clusters$mustRun))
  expect_false(is.null(output$clusters$mustRunPartial))
  expect_equal(output$clusters$mustRun + output$clusters$mustRunPartial, output$clusters$mustRunTotal)
})

test_that("The result has the same number of rows", {
  expect_equal(nrow(output$areas), 24 * 7 * nweeks * length(opts$areaList))
})

test_that("mustRun is equal to 0 for clusters that are not in 'must run' mode", {
  expect_true(clusters[cluster != "peak_must_run_partial", all(mustRunPartial == 0)])
  expect_true(clusters[cluster != "base_must_run", all(mustRun == 0)])
})

test_that("mustRun is greater than 0 for clusters in 'must run' mode", {
  expect_gt(sum(clusters$mustRun), 0)
  expect_gt(sum(clusters$mustRunPartial), 0)
  expect_true(clusters[cluster == "base_must_run", all(production == mustRun)])
  expect_true(clusters[cluster == "peak_must_run_partial", all(production >= mustRunPartial)])
})

test_that("Time aggregation of mustRun works", {
  totalMustRun <- sum(areas$mustRunTotal)
  for (t in c("daily", "weekly", "monthly", "annual")) {
    tmp <- readAntares(select="", mustRun=TRUE, showProgress=FALSE, timeStep = t)
    expect_equal(sum(tmp$mustRunTotal), totalMustRun, info = t)
  }
})

test_that("mustRun also works when synthesis = FALSE", {
  totalMustRun <- sum(areas$mustRunTotal)
  byYear <- readAntares(clusters = "all", mustRun = TRUE, showProgress=FALSE, 
                        timeStep = "daily", mcYears = "all")
  
  # average mustRun should be very close to the one in the synthetic results
  expect_lt(abs(sum(byYear$mustRunTotal)/length(opts$mcYears) / totalMustRun - 1), 0.001)
  
  # mustRun should vary between Monte-Carlo scenarii
  expect_false(all(byYear[mcYear == 1]$mustRunTotal == byYear[mcYear == 2]$mustRunTotal))
})

test_that("table 'thermalModulation' is removed from the returned object (#51)", {
  output <- readAntares(areas = "all", clusters = "all", mustRun = TRUE, 
                        thermalModulation = TRUE,
                        showProgress = FALSE, select = "")
  expect_null(output$thermalModulation)
  
})

})
