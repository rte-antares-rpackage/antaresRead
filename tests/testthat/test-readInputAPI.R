# context("readInputAPI")
# 
# describe("readInputAPI", {
#   
#   
#   # opts <- setSimulationPathAPI(path, "input")
#   opts <- setSimulationPathAPI(pathAPI, 1)
#   opts2 <- setSimulationPath("C:/Users/TitouanRobert/Desktop/antaresStd", 1)
#   
#   
#   expect_true(data.table::fsetequal(readInputTS(load = "at", opts = opts), readInputTS(load = "at", opts = opts2)))
#   expect_true(data.table::fsetequal(readInputTS(ror = "at", opts = opts), readInputTS(ror = "at", opts = opts2)))
#   
#   expect_true(data.table::fsetequal(readInputTS(wind = "at", opts = opts), readInputTS(wind = "at", opts = opts2)))
#   expect_true(data.table::fsetequal(readInputTS(solar = "at", opts = opts), readInputTS(solar = "at", opts = opts2)))
#   
#   expect_true(data.table::fsetequal(readInputTS(misc = "at", opts = opts), readInputTS(misc = "at", opts = opts2)))
# 
# })
