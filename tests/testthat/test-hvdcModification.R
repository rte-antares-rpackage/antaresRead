
context("hvdcModification")

#misc

sapply(studyPathS, function(studyPath){
  
  
  opts <- setSimulationPath(studyPath)

  data <- readAntares(areas = 'all', links = 'all', showProgress = FALSE)
  data <- setHvdcAreas(data, "c")
  data2 <- hvdcModification(data, removeHvdcAreas = TRUE, reafectLinks = TRUE)
  
  expect_true(all(data2$links[link == "b - hub"]$`FLOW LIN.` == data$links[link == "b - c"]$`FLOW LIN.`))
  
  
  data <- readAntares(areas = 'all', links = 'all', showProgress = FALSE)
  data <- setHvdcAreas(data, "psp in")
  data2 <- hvdcModification(data, removeHvdcAreas = TRUE, reafectLinks = TRUE)
  
  
  expect_true(all(data2$links[link == "b - hub"]$`FLOW LIN.` == data$links[link == "b - c"]$`FLOW LIN.`))
  expect_true(!("psp in - a" %in%unique(data2$links$link) |  "a - psp in" %in%unique(data2$links$link)))
  expect_true(!("psp in" %in% unique(data2$areas$area)))
  
  data2 <- hvdcModification(data, removeHvdcAreas = FALSE, reafectLinks = TRUE)
  expect_true(("psp in" %in% unique(data2$areas$area)))
  
})

