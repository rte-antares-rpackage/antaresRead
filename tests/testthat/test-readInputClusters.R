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
    
    test_that("Thermal data importation works", {
      input <- readInputThermal(clusters = "peak_must_run_partial", thermalModulation = TRUE, showProgress = FALSE)
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

# RES ----

## build mok

mockStudyMetaData <- function(force = FALSE, antares_version = "8.2.0", ...) {
  if (!is.null(getOption("antares")) & !isTRUE(force)) {
    stop("A study is already registered, use force = TRUE to overwrite.", 
         call. = FALSE)
  }
  
  antares_vers <- paste(unlist(as.numeric_version(antares_version)), 
                        collapse = "")
  
  opts <- list(
    typeLoad = "txt",
    antaresVersion = antares_vers,
    ...
  )
  
  class(opts) <- c("list", "simOptions")
  options(antares = opts)
  return(invisible(opts))
}

# dir in package
moc_dir <- system.file("moc_study", 
                       package = "antaresRead")

# add necessary info/params
mockStudyMetaData(force = TRUE, 
                  inputPath = moc_dir, 
                  mode = "Input", 
                  studyName = "mocStudy",
                  areasWithResClusters = c("at", "fr", "it"),
                  parameters = list(`other preferences` = list(
                    `renewable-generation-modelling` = "clusters")))

readInputRES(clusters = "at_res_1")















