#Copyright © 2016 RTE Réseau de transport d’électricité

context("removeVirtualAreas function")

sapply(studyPathS, function(studyPath){
  
opts <- setSimulationPath(studyPath)

data <- suppressWarnings(readAntares(areas = "all", links = "all", districts = "all" , showProgress = FALSE,
                                     linkCapacity = TRUE, select = "nostat"))

vareas <- c("psp in-2", "psp out-2")

dataCorrected <- suppressWarnings(removeVirtualAreas(data, 
                                                     storageFlexibility = vareas))

test_that("removeVirtualAreas effectively removes virtual areas", {
  expect_false(any(dataCorrected$areas$area %in% vareas))
  expect_false(any(dataCorrected$links$link %in% getLinks(vareas)))
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
  data <- suppressWarnings(readAntares("all", "all", "all", showProgress = FALSE, mcYears = "all", linkCapacity = TRUE, select = "nostat"))
  dataCorrected <- removeVirtualAreas(data, storageFlexibility = vareas, production = "psp in-2")
  
  expect_false(any(dataCorrected$clusters$area == "psp in-2"))
})

test_that("RemoveVirtualAreas removes production areas", {
  dataCorrected <- suppressWarnings(removeVirtualAreas(x = data, 
                                                       production = "a_offshore", 
                                                       reassignCosts = TRUE))
  
  expect_equal(dataCorrected$areas[area == "a", `OP. COST`], 
               data$areas[area == "a", `OP. COST`] + data$areas[area == "a_offshore", `OP. COST`])
  
  expect_false(is.null(dataCorrected$areas$WIND_virtual))
  expect_null(dataCorrected$areas$GAS_virtual)
})

test_that("RemoveVirtualAreas() corrects production columns if newCols = FALSE", {
  dataCorrected <- suppressWarnings(removeVirtualAreas(data, 
                                      production = "a_offshore"))
  wind1 <- dataCorrected$areas[, WIND + WIND_virtual]
  dataCorrected2 <- suppressWarnings(removeVirtualAreas(data, 
                                                        production = "a_offshore", 
                                                        newCols = FALSE))
  wind2 <- dataCorrected2$areas$WIND
  expect_true(is.null(dataCorrected2$areas$WIND_virtual))
  expect_equal(wind1, wind2)
})

test_that("Hub management works", {
  dataCorrected <- suppressWarnings(removeVirtualAreas(x = data, 
                                                       storageFlexibility = c("hub", vareas)))
  
  dataCorrected2 <- suppressWarnings(removeVirtualAreas(data, 
                                                        storageFlexibility = c(vareas)))
  dataCorrected2 <- suppressWarnings(removeVirtualAreas(x = dataCorrected2, 
                                                        storageFlexibility = "hub"))
  
  expect_equal(dataCorrected$areas$BALANCE, dataCorrected2$areas$BALANCE)
  expect_equal(dataCorrected$areas$`OP. COST`, dataCorrected2$areas$`OP. COST`)
  
  expect_null(dataCorrected$areas$`psp in`)
})

test_that("RemoveVirtualAreas also works on non-synthesis results", {
  data <- suppressWarnings(readAntares("all", "all", showProgress = FALSE, mcYears = "all", linkCapacity = TRUE, select = "nostat"))
  dataCorrected <- removeVirtualAreas(data, storageFlexibility = vareas)
  
  correction <- - data$links[link %in% getLinks(vareas), 
                             sum(`FLOW LIN.`), keyby = .(mcYear, timeId)]$V1
  
  setkey(data$areas, mcYear, timeId)
  setkey(dataCorrected$areas, mcYear, timeId)
  expect_equal(dataCorrected$areas[area=="hub"]$BALANCE - data$areas[area=="hub"]$BALANCE,
               correction)
})

test_that("reassignCosts works correctly", {
  dataCorrected <- suppressWarnings(removeVirtualAreas(x = data, 
                                                       storageFlexibility = c("psp out"),
                                      reassignCosts = TRUE))
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
  dataCorrected2 <- suppressWarnings(removeVirtualAreas(x = data, 
                                                        storageFlexibility = vareas, 
                                       newCols = FALSE))
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
  dataCorrectedStep <- suppressWarnings(removeVirtualAreas(x = data, 
                                          storageFlexibility = getAreas("psp")))
  
  expect_equal(unique(dataCorrectedStep$areas[area=="a", pumpingCapacity]), 3000)
  expect_equal(unique(dataCorrectedStep$areas[area=="a", storageCapacity]), 3000)
  
  expect_equal(unique(dataCorrectedStep$areas[area=="c", pumpingCapacity]), 0)
  expect_equal(unique(dataCorrectedStep$areas[area=="c", storageCapacity]), 0)
  
})

test_that("removeVirtualAreas move the cluster from virtual areas to real areas", {
  data <- suppressWarnings(readAntares(areas="all", links = "all", clusters = "all", districts = "all", showProgress = FALSE, mcYears = "all", linkCapacity = TRUE, select = "nostat"))
  
  #for this example we use psp virtual areas like virtual production areas
  dataCorrected <- removeVirtualAreas(data, production = getAreas(select = c("psp in-2", "psp out-2")))
  
  expect_equal(dim(dataCorrected$clusters[area %in% vareas,])[1], 0)
  
  expect_gt(dim(data$clusters[area %in% vareas,])[1], dim(dataCorrected$clusters[area %in% vareas,])[1])
  
  #cluster was in virtal areas
  clusterVirual<-unique(data$clusters[area %in% vareas, cluster])
  
  rarea<-unique(dataCorrected$clusters[cluster %in% clusterVirual, area])
  
  expect_equal(as.character(rarea), "hub")
})

test_that("bug #119, removeVirtualAreas correct district data", {
  
  for(i in c(1, 2, NULL)){
    data <- suppressWarnings(readAntares(areas = "all", 
                                         links = "all", 
                                         districts = "all" , 
                                         showProgress = FALSE,
                                         linkCapacity = TRUE, 
                                         select = "nostat",
                                         mcYears = i))
    
    vareas <- getAreas(select = c("psp", "hub"))
    dataCorrected <- removeVirtualAreas(data, 
                                        storageFlexibility = vareas,
                                        production = getAreas("off"))
    
    ### BEFORE : COMPARE BALANCE DISTRICT(AREAS) WITH LINC B-C
    oldDistrict <- c("a", "b", "a_offshore", "psp in", "psp out")
    data$areas[area %in% oldDistrict, SumDistrictBefore := sum(BALANCE), by = c("timeId")]
    
    resSup <- data$areas[area %in% oldDistrict[1], SumDistrictBefore] > data$links[link=="b - c", `FLOW LIN.`]+1
    resInf <- data$areas[area %in% oldDistrict[1], SumDistrictBefore] < data$links[link=="b - c", `FLOW LIN.`]-1
    
    expect_false(TRUE %in% c(resSup, resInf))
    
    ### AFTER : COMPARE BALANCE DISTRICT(AREAS) WITH LINC B-C
    newAreas <- c("a", "b")
    dataCorrected$areas[area %in% newAreas, SumDistrictAfter:= sum(BALANCE), by = c("timeId")]
    
    resSup <- dataCorrected$areas[area %in% newAreas[1], SumDistrictAfter] > data$links[link=="b - c", `FLOW LIN.`]+1
    resInf <- dataCorrected$areas[area %in% newAreas[1], SumDistrictAfter] < data$links[link=="b - c", `FLOW LIN.`]-1
    
    expect_false(TRUE %in% c(resSup, resInf))
    
    ## BEFORE AND AFTER : COMPARE BALANCE DISTRICT(AREAS)
    resSup <- dataCorrected$areas[area %in% newAreas[1], SumDistrictAfter] > data$areas[area %in% oldDistrict[1], SumDistrictBefore]+1
    resInf <- dataCorrected$areas[area %in% newAreas[1], SumDistrictAfter] < data$areas[area %in% oldDistrict[1], SumDistrictBefore]-1
    
    expect_false(TRUE %in% c(resSup, resInf))
    
    ## BEFORE : COMPARE BALANCE DISTRICT(DISTRICT) AND AREAS
    data$areas[area %in% newAreas, SumDistrictBefore:= sum(BALANCE), by = c("timeId")]
    resSup <- data$areas[area %in% newAreas, SumDistrictBefore] > data$districts[, BALANCE]+1
    resInf <- data$areas[area %in% newAreas, SumDistrictBefore] < data$districts[, BALANCE]-1
    expect_false(TRUE %in% c(resSup, resInf))    
    
    ## AFTER :  COMPARE BALANCE DISTRICT(DISTRICT) AND AREAS
    resSup <- dataCorrected$areas[area %in% newAreas[1], SumDistrictAfter]  > dataCorrected$districts[, BALANCE]+1
    resInf <- dataCorrected$areas[area %in% newAreas[1], SumDistrictAfter]  < dataCorrected$districts[, BALANCE]-1
    expect_false(TRUE %in% c(resSup, resInf))  
    
    ## BALANCE OF DISTRICT MUST CHANGE
    expect_error(expect_equal(dataCorrected$districts[, BALANCE],
                 data$districts[, BALANCE]))
  
  }
  colCostToCorrect <-  c("OV. COST", "OP. COST", "CO2 EMIS.", "NP COST")
  colMustChange <- c("PSP", "WIND", "BALANCE", colCostToCorrect)
  
  data <- suppressWarnings(readAntares(areas = "all", 
                                       links = "all", 
                                       districts = "all" , 
                                       showProgress = FALSE,
                                       linkCapacity = TRUE, 
                                       mcYears = 2))
  
  
  vareas <- getAreas(select = c("psp", "hub"))
  dataCorrected <- removeVirtualAreas(data, 
                                      storageFlexibility = vareas,
                                      production = getAreas("off"))
  
  for(i in colMustChange){
    varSumCal <- paste0("sum", i)
    dataCorrected$areas[area %in% newAreas, c(varSumCal):= sum(get(i)), by = c("timeId")]
    resSup <- dataCorrected$areas[area %in% newAreas[1], get(varSumCal)]  > dataCorrected$districts[, get(i)]+1
    resInf <- dataCorrected$areas[area %in% newAreas[1], get(varSumCal)]  < dataCorrected$districts[, get(i)]-1
    expect_false(TRUE %in% c(resSup, resInf),
                 paste0("pb with : ", i))  
  }
})

test_that("RemoveVirtualAreas corrects column 'BALANCE' if rowBal is TRUE", {
  data <- suppressWarnings(readAntares(areas = "all", 
                                       links = "all", 
                                       districts = "all", 
                                       showProgress = FALSE, 
                                       mcYears = "all", 
                                       linkCapacity = TRUE, 
                                       select = "nostat"))
  byArea <- c("area", "mcYear", "timeId")
  data$areas[, `ROW BAL.`:= as.integer(rnorm(1,mean = 2000, sd = 5)), 
             by = byArea]
  dataCorrected <- removeVirtualAreas(data, 
                                      storageFlexibility = c("psp in-2"),
                                      rowBal = TRUE)
  data$areas[, BALANCE := BALANCE -`ROW BAL.`, by = byArea]

  realAreas <- c("a", "b", "c")
  for(realA in realAreas){
    expect_true(all.equal(dataCorrected$areas[area %in% realA,
                                                BALANCE , 
                                                by = byArea],
                          data$areas[area %in% realA, 
                                       BALANCE, 
                                       by = byArea],
                          check.attributes = FALSE))
    
    expect_equal(unique(dataCorrected$areas[area %in% realA, 
                                            `ROW BAL.`]),
                 0)
  }
  
  expect_false("BALANCE.x" %in% names(dataCorrected$areas))
  expect_false("BALANCE.x" %in% names(dataCorrected$districts))
  expect_false("ROW BAL..x" %in% names(dataCorrected$areas))
  expect_false("ROW BAL..x" %in% names(dataCorrected$areas))
})

test_that("add pumpingCapacity and storageCapacity to district if storageFlex is not NULL", {
  mydata <- suppressWarnings({readAntares(areas = "all",
                                          districts ="all",
                                          links = "all",
                                          showProgress = FALSE,
                                          hydroStorageMaxPower = TRUE,
                                          linkCapacity = TRUE,
                                          mcYears = 1)})
  mydataCorrected <- removeVirtualAreas(mydata,
                                        storageFlexibility = c(getAreas("psp"),
                                                               getAreas("hub")))
  
  expect_false(is.null(mydataCorrected$areas$pumpingCapacity))
  expect_false(is.null(mydataCorrected$areas$storageCapacity))
  expect_false(is.null(mydataCorrected$districts$pumpingCapacity))
  expect_false(is.null(mydataCorrected$districts$storageCapacity))
})

})
