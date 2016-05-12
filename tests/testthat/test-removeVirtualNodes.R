context("removeVirtualAreas function")

source("setup_test_case.R")

opts <- setSimulationPath(studyPath)

data <- readAntares("all", "all", showProgress = FALSE)

vareas <- c("psp in-2", "psp out-2")

dataCorrected <- removeVirtualAreas(data, storageFlexibility = vareas)

test_that("removeVirtualAreas effectively removes virtual areas", {
  expect_false(any(dataCorrected$areas$area %in% vareas))
  expect_false(any(dataCorrected$areas$link %in% getLinks(vareas)))
})

test_that("Balance is corrected for nodes connected to virtual nodes but not the others", {
  expect_equal(data$areas[! area %in% c("hub", vareas)]$BALANCE, 
               dataCorrected$areas[! area %in% c("hub", vareas)]$BALANCE)
  
  correction <- - data$links[link %in% getLinks(vareas, regexpSelect = FALSE), 
                             sum(`FLOW LIN.`), keyby = timeId]$V1
  
  expect_equal(dataCorrected$areas[area=="hub"]$BALANCE - data$areas[area=="hub"]$BALANCE,
               correction)
  
})

test_that("A column has been created for each storage/flexibility area", {
  expect_true(all(vareas %in% names(dataCorrected$areas)))
  expect_equal(data$links[link=="hub - psp in-2"]$`FLOW LIN.`, 
               - dataCorrected$areas[area == "hub"]$`psp in-2`)
})

test_that("RemoveVirtualAreas corrects column 'area' in the table 'clusters'", {
  data <- readAntares("all", "all", "all", showProgress = FALSE, synthesis = FALSE)
  dataCorrected <- removeVirtualAreas(data, storageFlexibility = vareas, production = "c")
  
  expect_false(any(dataCorrected$clusters$area == "c"))
})

test_that("RemoveVirtualAreas removes production areas", {
  dataCorrected <- removeVirtualAreas(data, production = "a_offshore", reassignCosts = TRUE)
  
  expect_equal(dataCorrected$areas[area == "a", `OP. COST`], 
               data$areas[area == "a", `OP. COST`] + data$areas[area == "a_offshore", `OP. COST`])
  
  expect_false(is.null(dataCorrected$areas$WIND_virtual))
  expect_null(dataCorrected$areas$GAS_virtual)
})

test_that("Hub management works", {
  dataCorrected <- removeVirtualAreas(data, storageFlexibility = c("hub", vareas))
  
  dataCorrected2 <- removeVirtualAreas(data, storageFlexibility = c(vareas))
  dataCorrected2 <- removeVirtualAreas(dataCorrected2, storageFlexibility = "hub")
  
  expect_equal(dataCorrected$areas$BALANCE, dataCorrected2$areas$BALANCE)
  expect_equal(dataCorrected$areas$`OP. COST`, dataCorrected2$areas$`OP. COST`)
  
  expect_null(dataCorrected$areas$`psp in`)
})

test_that("RemoveVirtualAreas also works on non-synthesis results", {
  data <- readAntares("all", "all", showProgress = FALSE, synthesis = FALSE)
  dataCorrected <- removeVirtualAreas(data, storageFlexibility = vareas)
  
  correction <- - data$links[link %in% getLinks(vareas, regexpSelect = FALSE), 
                             sum(`FLOW LIN.`), keyby = .(mcYear, timeId)]$V1
  
  setkey(data$areas, mcYear, timeId)
  setkey(dataCorrected$areas, mcYear, timeId)
  expect_equal(dataCorrected$areas[area=="hub"]$BALANCE - data$areas[area=="hub"]$BALANCE,
               correction)
})
