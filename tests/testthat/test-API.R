# context("testAPI")
# 
# describe("testAPI", {
#
#   # opts <- setSimulationPathAPI(path, "input")
#   opts <- setSimulationPathAPI(pathAPI, 1)
#   opts2 <- setSimulationPath("C:/Users/TitouanRobert/Desktop/antaresStd", 1)
#   
#   DT2 <- readLayout(opts = opts2)
#   DT <- readLayout(opts = opts)
#   
#   expect_true(data.table::fsetequal(DT2$areas, DT$areas))
#   expect_true(data.table::fsetequal(DT2$districts, DT$districts))
#   expect_true(data.table::fsetequal(DT2$links, DT$links))
#   expect_true(data.table::fsetequal(DT2$districtLinks, DT$districtLinks))
#   
#   
#   
#   re1 <- readClusterDesc(opts)
#   re2 <- readClusterDesc(opts2)
#   expect_true(data.table::fsetequal(re1, re2[, .SD, .SDcols = names(re1)]))
#   
# })
