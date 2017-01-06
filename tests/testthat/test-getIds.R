#Copyright © 2016 RTE Réseau de transport d’électricité

context("getIds")

source("setup_test_case.R")
opts <- setSimulationPath(studyPath)

test_that("input is an antareDataTable", {
  
  setSimulationPath(studyPath, -1)
  mydata<-readAntares(areas = "all", links = "all", showProgress = FALSE)
  
  expect_error(getIds("RER"))
  expect_error(getIds(1))
  expect_error(getIds(c(1,6)))
  expect_error(getIds(mydata))
  expect_error(getIds(mydata), "x has to be an 'antaresDataTable' object")
})

test_that("output is character", {
  
  setSimulationPath(studyPath, -1)
  mydata<-readAntares(areas = "all", links = "all", showProgress = FALSE)
  
  expect_is(getIds(mydata$areas), "character")
})
