context("Remove input column(s)")

#misc
opts <- setSimulationPath(studyPath)
V <- readAntares(areas = "all", select = c("misc", "-GeoThermal"))
V2 <- readAntares(areas = "all", select = c("misc"))
V2[,GeoThermal:=NULL] 

expect_true(identical(V, V2))


V <- readAntares(areas = "all", select = c("misc", "-GeoThermal", "-Bio_gas"))
V2 <- readAntares(areas = "all", select = c("misc"))
V2[,GeoThermal:=NULL] 
V2[,Bio_gas:=NULL] 

expect_true(identical(V, V2))




#reserve
V <- readAntares(areas = "all",  select = c("reserve", "-DSM"))
V2 <- readAntares(areas = "all", select = c("reserve"))
V2[,DSM:=NULL] 

expect_true(identical(V, V2))


#hydroStorageMaxPower
V <- readAntares(areas = "all",  select = c("hydroStorageMaxPower", "-hstorPMaxHigh"))
V2 <- readAntares(areas = "all", select = c("hydroStorageMaxPower"))
V2[,hstorPMaxHigh:=NULL] 

expect_true(identical(V, V2))
