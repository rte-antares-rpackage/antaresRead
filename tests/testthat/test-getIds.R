#Copyright © 2016 RTE Réseau de transport d’électricité

context("getIds")

opts <- setSimulationPath(studyPath)

test_that("It only accepts antaresDataTable input", {
  
  setSimulationPath(studyPath, -1)
  mydata<-readAntares(areas = "all", links = "all", showProgress = FALSE)
  
  expect_error(getIdCols("RER"))
  expect_error(getIdCols(1))
  expect_error(getIdCols(c(1,6)))
  expect_error(getIdCols(mydata))
  expect_error(getIdCols(mydata), "x has to be an 'antaresDataTable' object")
})

test_that("output is character", {
  
  setSimulationPath(studyPath, -1)
  mydata<-readAntares(areas = "all", links = "all", showProgress = FALSE)
  
  expect_is(getIdCols(mydata$areas), "character")
})
