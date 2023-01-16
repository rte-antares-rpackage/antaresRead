#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function readInputThermal")
sapply(studyPathS, function(studyPath){
  
  opts <- setSimulationPath(studyPath)
  
  
  if(!isH5Opts(opts)){
    
    test_that("Thermal availabilities importation works", {
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
    
  }
})
