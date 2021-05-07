context("Function ponderateMcAggregation")


sapply(studyPathS, function(studyPath){


  opts <- setSimulationPath(studyPath)
  data <- readAntares(areas = 'all', mcYears = 'all', showProgress = FALSE)
  data2 <- ponderateMcAggregation(data, fun = weighted.mean, w = c(.1, .9))
  expect_true(weighted.mean(data[area == 'a' & timeId == timeId[1]]$`OV. COST`, c(.1, .9)) ==
                data2[area == 'a' & timeId == timeId[1]]$`OV. COST`)

})
