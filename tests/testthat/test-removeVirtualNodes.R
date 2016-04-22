context("removeVirtualNodes function")

source("setup_test_case.R")

opts <- setSimulationPath(studyPath, trace=0)

data <- readAntares("all", "all", showProgress = FALSE)

vnodes <- c("psp in", "psp out")

dataCorrected <- removeVirtualNodes(data, storageFlexibility = vnodes)

test_that("removeVirtualNodes effectively removes virtual nodes", {
  expect_false(any(dataCorrected$nodes$node %in% vnodes))
  expect_false(any(dataCorrected$nodes$link %in% getLinks(vnodes)))
})

test_that("Balance is corrected for not b, but not for the other nodes", {
  expect_equal(data$nodes[node %in% c("a", "c")]$BALANCE, 
               dataCorrected$nodes[node %in% c("a", "c")]$BALANCE)
  
  correction <- - data$links[link %in% getLinks(vnodes), 
                             sum(`FLOW LIN.`), keyby = timeId]$V1
  
  expect_equal(dataCorrected$nodes[node=="b"]$BALANCE - data$nodes[node=="b"]$BALANCE,
               correction)
  
})

test_that("A column has been created for each storage/flexibility node", {
  expect_true(all(vnodes %in% names(dataCorrected$nodes)))
  expect_equal(data$links[link=="b - psp in"]$`FLOW LIN.`, 
               - dataCorrected$nodes[node == "b"]$`psp in`)
})

test_that("RemoveVirtualNodes corrects column 'node' in the table 'clusters'", {
  data <- readAntares("all", "all", "all", showProgress = FALSE, synthesis = FALSE)
  dataCorrected <- removeVirtualNodes(data, storageFlexibility = vnodes, production = "c")
  
  expect_false(any(dataCorrected$clusters$node == "c"))
})

test_that("RemoveVirtualNodes removes production nodes", {
  dataCorrected <- removeVirtualNodes(data, production = "c", reassignCosts = TRUE)
  
  expect_equal(dataCorrected$nodes[node == "b", `OP. COST`], 
               data$nodes[node == "b", `OP. COST`] + data$nodes[node == "c", `OP. COST`])
  
  expect_false(is.null(dataCorrected$nodes$OIL_virtual))
  expect_null(dataCorrected$nodes$GAS_virtual)
})

test_that("Hub management works", {
  dataCorrected <- removeVirtualNodes(data, storageFlexibility = c("b", vnodes))
  
  dataCorrected2 <- removeVirtualNodes(data, storageFlexibility = c(vnodes))
  dataCorrected2 <- removeVirtualNodes(dataCorrected2, storageFlexibility = "b")
  
  expect_equal(dataCorrected$nodes$BALANCE, dataCorrected2$nodes$BALANCE)
  expect_equal(dataCorrected$nodes$`OP. COST`, dataCorrected2$nodes$`OP. COST`)
  
  expect_null(dataCorrected$nodes$`psp in`)
})

test_that("RemoveVirtualNodes also work on non-synthesis results", {
  data <- readAntares("all", "all", showProgress = FALSE, synthesis = FALSE)
  dataCorrected <- removeVirtualNodes(data, storageFlexibility = vnodes, production = "c")
  
  correction <- - data$links[link %in% getLinks(c(vnodes, "c")), 
                             sum(`FLOW LIN.`), keyby = .(mcYear, timeId)]$V1
  
  setkey(data$nodes, mcYear, timeId)
  setkey(dataCorrected$nodes, mcYear, timeId)
  expect_equal(dataCorrected$nodes[node=="b"]$BALANCE - data$nodes[node=="b"]$BALANCE,
               correction)
})


