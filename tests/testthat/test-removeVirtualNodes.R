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

