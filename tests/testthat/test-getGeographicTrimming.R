#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function getGeographicTrimming")

sapply(studyPathS, function(studyPath){
  
  opts <- setSimulationPath(studyPath)
  
  test_that("Areas filtering is ok", {
    filtering <- getGeographicTrimming(areas = "all", links = T, opts = opts)
    expect_identical(names(filtering), c("areas", "links"))
    expect_true(length(filtering$areas) == length(opts$areaList))
  })
  
})
