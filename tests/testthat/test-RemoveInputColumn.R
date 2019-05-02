context("Remove input column(s)")

#misc

sapply(studyPathS, function(studyPath){
  
  
  opts <- setSimulationPath(studyPath)
  V <- suppressWarnings({readAntares(areas = "all", select = c("misc", "-GeoThermal"), showProgress = FALSE)})
  V2 <- suppressWarnings({readAntares(areas = "all", select = c("misc"), showProgress = FALSE)})
  V2[,GeoThermal:=NULL] 
  
  expect_true(identical(V, V2))
  
  
  V <-  suppressWarnings({readAntares(areas = "all", select = c("misc", "-GeoThermal", "-Bio_gas"), showProgress = FALSE)})
  V2 <- suppressWarnings({readAntares(areas = "all", select = c("misc"), showProgress = FALSE)})
  V2[,GeoThermal:=NULL] 
  V2[,Bio_gas:=NULL] 
  
  expect_true(identical(V, V2))
  
  #reserve
  V <-  suppressWarnings({readAntares(areas = "all",  select = c("reserve", "-DSM"), showProgress = FALSE)})
  V2 <- suppressWarnings({readAntares(areas = "all", select = c("reserve"), showProgress = FALSE)})
  V2[,DSM:=NULL] 
  
  expect_true(identical(V, V2))
  
  
  #linkCapacity
  V <-  suppressWarnings({readAntares(links = "all",  select = c("linkCapacity", "-transCapacityIndirect"), showProgress = FALSE)})
  V2 <- suppressWarnings({readAntares(links = "all", select = c("linkCapacity"), showProgress = FALSE)})
  V2[,transCapacityIndirect:=NULL] 
  
  expect_true(identical(V, V2))
  
  V <-  suppressWarnings({readAntares(areas = "all", select = c("mustRun", "-mustRunPartial"), showProgress = FALSE)})
  V2 <- suppressWarnings({readAntares(areas = "all", select = c("mustRun"), showProgress = FALSE)})
  V2[,mustRunPartial:=NULL] 
  expect_true(identical(V, V2))
  
})

