#Copyright © 2016 RTE Réseau de transport d’électricité

context("removeVirtualAreas function")

opts <- setSimulationPath(studyPath)

data <- suppressWarnings(readAntares("all", "all", districts = "all" , showProgress = FALSE, linkCapacity = TRUE))

vareas <- c("psp in-2", "psp out-2")

dataCorrected <- removeVirtualAreas(data, storageFlexibility = vareas)

test_that("removeVirtualAreas effectively removes virtual areas", {
  expect_false(any(dataCorrected$areas$area %in% vareas))
  expect_false(any(dataCorrected$areas$link %in% getLinks(vareas)))
})

test_that("Balance is corrected for nodes connected to virtual nodes but not the others", {
  setkeyv(data$areas, .idCols(data$areas))
  expect_equal(data$areas[! area %in% c("hub", vareas)]$BALANCE, 
               dataCorrected$areas[! area %in% c("hub", vareas)]$BALANCE)
  
  correction <- - data$links[link %in% getLinks(vareas), 
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
  data <- suppressWarnings(readAntares("all", "all", "all", showProgress = FALSE, mcYears = "all", linkCapacity = TRUE))
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

test_that("RemoveVirtualAreas() corrects production columns if newCols = FALSE", {
  dataCorrected <- removeVirtualAreas(data, production = "a_offshore")
  wind1 <- dataCorrected$areas[, WIND + WIND_virtual]
  dataCorrected2 <- removeVirtualAreas(data, production = "a_offshore", newCols = FALSE)
  wind2 <- dataCorrected2$areas$WIND
  expect_true(is.null(dataCorrected2$areas$WIND_virtual))
  expect_equal(wind1, wind2)
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
  data <- suppressWarnings(readAntares("all", "all", showProgress = FALSE, mcYears = "all", linkCapacity = TRUE))
  dataCorrected <- removeVirtualAreas(data, storageFlexibility = vareas)
  
  correction <- - data$links[link %in% getLinks(vareas), 
                             sum(`FLOW LIN.`), keyby = .(mcYear, timeId)]$V1
  
  setkey(data$areas, mcYear, timeId)
  setkey(dataCorrected$areas, mcYear, timeId)
  expect_equal(dataCorrected$areas[area=="hub"]$BALANCE - data$areas[area=="hub"]$BALANCE,
               correction)
})

test_that("reassignCosts works correctly", {
  dataCorrected <- removeVirtualAreas(data, storageFlexibility = c("psp out"),
                                      reassignCosts = TRUE)
  # NOTE: this test fails if flows at some timeId are equal to 0 but costs are 
  # greater than 0
  
  
  # Check that total cost is preserved
  oldCosts <- data$areas[, .(cost = sum(as.numeric(`OV. COST`))), by = area]
  newCosts <- dataCorrected$areas[, .(cost = sum(as.numeric(`OV. COST`))), by = area]
  expect_equal(oldCosts[area %in% c("a", "b", "psp out"), sum(cost)],
               newCosts[area %in% c("a", "b"), sum(cost)])
  
  # Check the repartition of the costs
  prop <- data$links[link == "b - psp out", abs(`FLOW LIN.`)] / 
    (data$links[link == "b - psp out", abs(`FLOW LIN.`)] + data$links[link == "a - psp out", abs(`FLOW LIN.`)])
  prop[is.na(prop)] <- 0
  
  expect_equal(
    data$areas[area == "a", `OV. COST`] + (1 - prop) * data$areas[area == "psp out", `OV. COST`],
    dataCorrected$areas[area == "a", `OV. COST`]
  )
})

test_that("removeVirtualAreas corrects variable PSP if newCols=FALSE", {
  psp1 <- dataCorrected$areas[, PSP + `psp in-2` + `psp out-2`]
  dataCorrected2 <- removeVirtualAreas(data, storageFlexibility = vareas, 
                                       newCols = FALSE)
  psp2 <- dataCorrected2$areas$PSP
  
  expect_equal(psp1, psp2)
  expect_true(is.null(dataCorrected2$areas$`psp in-2`))
})

test_that("removeVirtualAreas removes virtual links, but keeps the data needed to compute margins", {
  expect_true(nrow(dataCorrected$links[link == "a - psp out-2"]) == 0)

  if(!is.null(dataCorrected$areas)){
    expect_false(is.null(dataCorrected$areas$storageCapacity))
    expect_false(is.null(dataCorrected$areas$pumpingCapacity))
    expect_gt(min(dataCorrected$areas$storageCapacity), -1)
    expect_gt(min(dataCorrected$areas$pumpingCapacity), -1)
  }
  
  if(!is.null(dataCorrected$districts)){
    expect_false(is.null(dataCorrected$districts$storageCapacity))
    expect_false(is.null(dataCorrected$districts$pumpingCapacity))
    expect_gt(min(dataCorrected$districts$storageCapacity), -1)
    expect_gt(min(dataCorrected$districts$pumpingCapacity), -1)
  }

})

test_that("removeVirtualAreas compute storage and pumping capacities", {
  dataCorrectedStep <- removeVirtualAreas(data, storageFlexibility = getAreas("psp"))
  
  expect_equal(dataCorrectedStep$areas[area=="a" & timeId==200,]$pumpingCapacity, 2516)
  expect_equal(dataCorrectedStep$areas[area=="a" & timeId==203,]$pumpingCapacity, 4562)
  
  expect_equal(dataCorrectedStep$areas[area=="a" & timeId==202,]$storageCapacity, 2758)
  expect_equal(dataCorrectedStep$areas[area=="a" & timeId==208,]$storageCapacity, 4613)
  
  
})
