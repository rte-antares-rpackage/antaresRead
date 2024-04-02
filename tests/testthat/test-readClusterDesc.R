
# v860 ----

path_study_test <- grep(pattern = "86", x = studyPathSV8, value = TRUE)
opts_study_test <- setSimulationPath(path_study_test, simulation = "input")

#minimal columns
mandatory_cols <- c("area","cluster")

## Thermal ----
test_that("test read cluster", {
  
  # function setSimulationPath() provide areas names with st-storage clusters
  areas <- opts_study_test$areasWithClusters
  
  # read clusters informations
  input <- readClusterDesc()
  
  # tests
  testthat::expect_true("data.table" %in% class(input))
  testthat::expect_true(all(areas %in% unique(readClusterDesc()$area)))
  testthat::expect_true(all(mandatory_cols %in% colnames(input)))
  testthat::expect_true(nrow(input) == length(input$cluster))
  
})

## Renewables ----
test_that("test read cluster renewables", {
  
  # function setSimulationPath() provide areas names with st-storage clusters
  areas_res <- opts_study_test$areasWithRESClusters
  
  #Study not renewables (need activateRES() from antaresEditObject)
  expect_error(readClusterResDesc(),
               regexp = "readClusterDesc is available only on studies with 'renewable-generation-modelling'")
  
})

## st-storage ----
test_that("test read cluster st-storage v860", {
  
  # function setSimulationPath() provide areas names with st-storage clusters
  areas_st <- opts_study_test$areasWithSTClusters
  
  # read clusters st-storage informations
  input_st <- readClusterSTDesc()
  
  # tests
  testthat::expect_true("data.table" %in% class(input_st))
  testthat::expect_true(all(areas_st %in% unique(readClusterSTDesc()$area)))
  testthat::expect_true(all(mandatory_cols %in% colnames(input_st)))
  testthat::expect_true(nrow(input_st) == length(input_st$cluster))
  
})
