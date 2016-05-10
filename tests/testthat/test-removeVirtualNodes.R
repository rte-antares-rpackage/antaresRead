context("removeVirtualNodes function")

source("setup_test_case.R")

opts <- setSimulationPath(studyPath)

data <- readAntares("all", "all", showProgress = FALSE)

vnodes <- c("psp in-2", "psp out-2")

dataCorrected <- removeVirtualNodes(data, storageFlexibility = vnodes)

test_that("removeVirtualNodes effectively removes virtual nodes", {
  expect_false(any(dataCorrected$nodes$node %in% vnodes))
  expect_false(any(dataCorrected$nodes$link %in% getLinks(vnodes)))
})

test_that("Balance is corrected for 'Hub', but not for the other nodes", {
  expect_equal(data$nodes[! node %in% c("hub", vnodes)]$BALANCE, 
               dataCorrected$nodes[! node %in% c("hub", vnodes)]$BALANCE)
  
  correction <- - data$links[link %in% getLinks(vnodes, regexpSelect = FALSE), 
                             sum(`FLOW LIN.`), keyby = timeId]$V1
  
  expect_equal(dataCorrected$nodes[node=="hub"]$BALANCE - data$nodes[node=="hub"]$BALANCE,
               correction)
  
})

test_that("A column has been created for each storage/flexibility node", {
  expect_true(all(vnodes %in% names(dataCorrected$nodes)))
  expect_equal(data$links[link=="hub - psp in-2"]$`FLOW LIN.`, 
               - dataCorrected$nodes[node == "hub"]$`psp in-2`)
})

test_that("RemoveVirtualNodes corrects column 'node' in the table 'clusters'", {
  data <- readAntares("all", "all", "all", showProgress = FALSE, synthesis = FALSE)
  dataCorrected <- removeVirtualNodes(data, storageFlexibility = vnodes, production = "c")
  
  expect_false(any(dataCorrected$clusters$node == "c"))
})

test_that("RemoveVirtualNodes removes production nodes", {
  dataCorrected <- removeVirtualNodes(data, production = "a_offshore", reassignCosts = TRUE)
  
  expect_equal(dataCorrected$nodes[node == "a", `OP. COST`], 
               data$nodes[node == "a", `OP. COST`] + data$nodes[node == "a_offshore", `OP. COST`])
  
  expect_false(is.null(dataCorrected$nodes$WIND_virtual))
  expect_null(dataCorrected$nodes$GAS_virtual)
})

test_that("Hub management works", {
  dataCorrected <- removeVirtualNodes(data, storageFlexibility = c("hub", vnodes))
  
  dataCorrected2 <- removeVirtualNodes(data, storageFlexibility = c(vnodes))
  dataCorrected2 <- removeVirtualNodes(dataCorrected2, storageFlexibility = "hub")
  
  expect_equal(dataCorrected$nodes$BALANCE, dataCorrected2$nodes$BALANCE)
  expect_equal(dataCorrected$nodes$`OP. COST`, dataCorrected2$nodes$`OP. COST`)
  
  expect_null(dataCorrected$nodes$`psp in`)
})

test_that("RemoveVirtualNodes also works on non-synthesis results", {
  data <- readAntares("all", "all", showProgress = FALSE, synthesis = FALSE)
  dataCorrected <- removeVirtualNodes(data, storageFlexibility = vnodes)
  
  correction <- - data$links[link %in% getLinks(vnodes, regexpSelect = FALSE), 
                             sum(`FLOW LIN.`), keyby = .(mcYear, timeId)]$V1
  
  setkey(data$nodes, mcYear, timeId)
  setkey(dataCorrected$nodes, mcYear, timeId)
  expect_equal(dataCorrected$nodes[node=="hub"]$BALANCE - data$nodes[node=="hub"]$BALANCE,
               correction)
})


