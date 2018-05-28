#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function extractDataList")

sapply(studyPathS, function(studyPath){
  
  
  opts <- setSimulationPath(studyPath)
  
  areas <- readAntares("all", showProgress=FALSE)
  
  test_that("extractDataList throws a warning if area does not exist", {
    expect_warning(extractDataList(areas, c("a", "missingArea")))
  })
  
  test_that("extractDataList throws an error if all areas do not exist", {
    expect_error(suppressWarnings(extractDataList(areas, c("missingArea")), "area"))
  })
  
  links <- readAntares(links = "all", showProgress=FALSE)
  
  test_that("error no area",{
    expect_error(extractDataList(links), "'x' does not contain areas data." )
  })
  
  
  areasMc <- readAntares("all", mcYears = "all", showProgress=FALSE)
  keepArea <- areasMc$area[1]
  
  test_that("No mcYear after extractDataList",{
    expect_false("mcYear" %in% names(extractDataList(areasMc)[[keepArea]]))
  })
  
  test_that("extractDataList on areas, links clusters",{
    allData <- readAntares(areas = "all", links = "all", clusters = "all", showProgress=FALSE)
    extractDataList(allData)
    expect_true(is(allData, "antaresDataList"))
  })
})
