#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function readInputThermal")
sapply(studyPathS, function(studyPath){
  
  opts <- setSimulationPath(studyPath)
  
  
  if(!isH5Opts(opts)){
    
    test_that("Thermal availabilities importation works", {
      # read /series files (default)
      input <- readInputThermal(clusters = "peak_must_run_partial", showProgress = FALSE)
      expect_is(input, "antaresDataTable")
      expect_gt(nrow(input), 0)
      expect_equal(nrow(input) %% (24 * 7 * nweeks), 0)
    })
    
    test_that("Thermal modulation importation works", {
      input <- readInputThermal(clusters = "peak_must_run_partial", thermalModulation = TRUE, showProgress = FALSE)
      expect_is(input, "antaresDataList")
      expect_is(input$thermalModulation, "antaresDataTable")
      expect_gt(nrow(input$thermalModulation), 0)
      expect_equal(nrow(input$thermalModulation) %% (24 * 7 * nweeks), 0)
    })
    
    test_that("Thermal data importation works", {
      input <- readInputThermal(clusters = "peak_must_run_partial", thermalData = TRUE, showProgress = FALSE, timeStep = 'daily')
      expect_is(input, "antaresDataList")
      expect_is(input$thermalModulation, "antaresDataTable")
      expect_gt(nrow(input$thermalModulation), 0)
      expect_equal(nrow(input$thermalModulation) %% (24 * 7 * nweeks), 0)
    })
    
    test_that("Wrong area", {
      expect_error(readInputThermal(areas = "BAD_AREA", clusters = "peak_must_run_partial"),
                   regexp = "areas are not available")
    })
    
    test_that("Wrong cluster", {
      expect_error(readInputThermal(areas = "all", clusters = "BAD_CLUSTER"),
                   regexp = "clusters are not available")
    })
    
    test_that("No thermal data selected", {
      expect_error(readInputThermal(clusters = "peak_must_run_partial", thermalAvailabilities = FALSE),
                   regexp = "one type of data should be selected")
    })
    
  }
})

# >= v870 ----
## RES ----
test_that("test reading TS RES", {
 
  # read latest version study
  path_study_test <- grep(pattern = "87", x = studyPathSV8, value = TRUE)
  setSimulationPath(path_study_test, simulation = "input")
  
 toto=  readInputRES(clusters = "at_res_1")
 
  toto = readInputThermal(clusters = "at_gas")
  
})
