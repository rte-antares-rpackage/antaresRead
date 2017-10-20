#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function readAntares (date-time columns)")
sapply(studyPathS, function(studyPath){
  
opts <- setSimulationPath(studyPath)

test_that("readAntares add date-time columns at hourly time step", {
  output <- readAntares("a", showProgress = FALSE)
  expect_true(all(c("timeId", "time", "day", "month", "hour") %in% names(output)))
})

test_that("readAntares add date-time columns at daily time step", {
  output <- readAntares("a", showProgress = FALSE, timeStep = "daily")
  expect_true(all(c("timeId", "time", "day", "month") %in% names(output)))
})

test_that("readAntares add date-time columns at weekly time step", {
  output <- readAntares("a", showProgress = FALSE, timeStep = "weekly")
  expect_true(all(c("timeId", "time") %in% names(output)))
})

test_that("readAntares add date-time columns at monthly time step", {
  output <- readAntares("a", showProgress = FALSE, timeStep = "monthly")
  expect_true(all(c("timeId", "time", "month") %in% names(output)))
})

test_that("readAntares add date-time columns at annual time step", {
  output <- readAntares("a", showProgress = FALSE, timeStep = "annual")
  expect_true(all(c("timeId", "time") %in% names(output)))
})
})
