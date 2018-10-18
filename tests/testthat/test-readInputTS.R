#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function readInputTS")
sapply(studyPathS, function(studyPath){
  
opts <- setSimulationPath(studyPath)


if(!isH5Opts(opts)){

test_that("Load importation works", {
  input <- readInputTS(load = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("Thermal availabilities importation works", {
  input <- readInputTS(thermalAvailabilities = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("Run of river importation works", {
  input <- readInputTS(ror = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("Hydro storage importation works", {
  input <- readInputTS(hydroStorage = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("Hydro storage maximum power importation works", {
  input <- readInputTS(hydroStorageMaxPower = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("Wind importation works", {
  input <- readInputTS(wind = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("Solar importation works", {
  input <- readInputTS(solar = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("Misc importation works", {
  input <- readInputTS(misc = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("Reserve importation works", {
  input <- readInputTS(reserve = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("Link capacity importation works", {
  input <- readInputTS(linkCapacity = "all", showProgress = FALSE)
  expect_is(input, "antaresDataTable")
  expect_gt(nrow(input), 0)
  expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
})

test_that("readInputTs must work if we change opts$timeIdMin and opts$timeIdMax", {
  input <- readInputTS(ror = "all", 
                       showProgress = FALSE, 
                       timeStep = "hourly",
                       opts = opts)
  expect_is(input, "antaresDataTable")
  
  sumRorA <- input[ , .(sumProdRor = sum(ror)), by= .(area, tsId)]
  sumRorA[, ror := sumProdRor]
  sumRorA <- sumRorA[, ror, by= .(area, tsId)]
  
  inputA <- suppressWarnings(readInputTS(ror = "all", 
                       showProgress = FALSE, 
                       timeStep = "annual",
                       opts = opts))
  
  inputA[, .(ror) , by= .(area, tsId)]
  
  expect_equal(sumRorA, inputA[, .(ror) , by= .(area, tsId)])

  optsNew <- opts
  optsNew$timeIdMin <- 3500
  optsNew$timeIdMax <- 4000
  
  inputAN <- suppressWarnings(readInputTS(ror = "all",
                        showProgress = FALSE,
                        timeStep = "annual",
                        opts = optsNew))
  
  inputH <- readInputTS(ror = "all", 
                       showProgress = FALSE, 
                       timeStep = "hourly",
                       opts = optsNew)
  
  sumRorANew <- inputH[ , .(sumProdRor = sum(ror)), by= .(area, tsId)]
  sumRorANew[, ror := sumProdRor]
  sumRorANew <- sumRorANew[, ror, by= .(area, tsId)]
  
  expect_equal(inputAN[, .( ror) , by= .(area, tsId)], sumRorANew)
  
})

}
})
