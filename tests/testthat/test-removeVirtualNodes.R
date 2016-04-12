context("removeVirtualNodes function")

source("setup_test_case.R")

opts <- setSimulationPath(studyPath, trace=0)

data <- readAntares("all", "all", showProgress = FALSE)

vnodes <- c("psp in", "psp out")

spec <- list(
  list(
    type = "pumped storage",
    trueNodes = "b",
    virtualNodes = vnodes
  )
)

dataCorrected <- removeVirtualNodes(data, spec)

test_that("removeVirtualNodes effectively removes virtual nodes", {
  expect_false(any(dataCorrected$nodes$node %in% vnodes))
  expect_false(any(dataCorrected$nodes$link %in% getLinks(vnodes)))
})

test_that("Balance is corrected for not b, but not for the other nodes", {
  expect_equal(data$nodes[node %in% c("a", "c")]$BALANCE, 
               dataCorrected$nodes[node %in% c("a", "c")]$BALANCE)
  
  correction <- - data$links[link %in% c("b - psp in", "b - psp out"), 
                             sum(`FLOW LIN.`), keyby = timeId]$V1
  
  expect_equal(dataCorrected$nodes[node=="b"]$BALANCE - data$nodes[node=="b"]$BALANCE,
               correction)
  
})
