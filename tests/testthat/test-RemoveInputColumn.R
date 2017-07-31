context("Remove input column(s)")

#From misc
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
