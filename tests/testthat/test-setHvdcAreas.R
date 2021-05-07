
context("setHvdcAreas")

#misc

sapply(studyPathS, function(studyPath){
  
  
  opts <- setSimulationPath(studyPath)
  
  dt <- readAntares(areas = "all", links = "all", showProgress = FALSE)
  
  dt <- setHvdcAreas(dt, as.character(unique(dt$areas$area)[1]))
  
  expect_true(attributes(dt$areas)$hvdcAreas ==  as.character(unique(dt$areas$area)[1]))
  
  dtar <- setHvdcAreas(dt$areas, as.character(unique(dt$areas$area)[1]))
  expect_true(attributes(dtar)$hvdcAreas ==  as.character(unique(dt$areas$area)[1]))
  
  expect_error(setHvdcAreas(dt, 'unexist'))
  expect_error(setHvdcAreas(dt$links, 'unexist'))
  
  
})
