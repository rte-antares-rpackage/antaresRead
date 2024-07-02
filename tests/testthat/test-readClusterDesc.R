# v710----

## Thermal ----
test_that("test read cluster", {
  path_study_test <- studyPathS
  opts_study_test <- setSimulationPath(path_study_test, simulation = "input")
  
  #minimal columns
  mandatory_cols <- c("area","cluster")
  
  # function setSimulationPath() provide areas names with st-storage clusters
  areas <- opts_study_test$areasWithClusters
  
  # read clusters informations
  input <- readClusterDesc()
  
  # tests
  testthat::expect_true("data.table" %in% class(input))
  testthat::expect_true(all(areas %in% unique(input$area)))
  testthat::expect_true(all(mandatory_cols %in% colnames(input)))
  testthat::expect_true(nrow(input) == length(input$cluster))
})

## Renewables ----
test_that("test read cluster renewables", {
  path_study_test <- grep(pattern = "87", x = studyPathSV8, value = TRUE)
  opts_study_test <- setSimulationPath(path_study_test, simulation = "input")
  
  #minimal columns
  mandatory_cols <- c("area","cluster")
  
  # function setSimulationPath() provide areas names with st-storage clusters
  areas_res <- opts_study_test$areasWithResClusters
  
  #read
  input <- readClusterResDesc()
  
  # tests
  testthat::expect_true("data.table" %in% class(input))
  testthat::expect_true(all(areas_res %in% unique(input$area)))
  testthat::expect_true(all(mandatory_cols %in% colnames(input)))
  testthat::expect_true(nrow(input) == length(input$cluster))
})

# v860 ----
## st-storage ----
test_that("test read cluster st-storage v860", {
  path_study_test <- grep(pattern = "87", x = studyPathSV8, value = TRUE)
  opts_study_test <- setSimulationPath(path_study_test, simulation = "input")
  
  #minimal columns
  mandatory_cols <- c("area","cluster")
  
  # function setSimulationPath() provide areas names with st-storage clusters
  areas_st <- opts_study_test$areasWithSTClusters
  
  # read clusters st-storage informations
  input_st <- readClusterSTDesc()
  
  # tests
  testthat::expect_true("data.table" %in% class(input_st))
  testthat::expect_true(all(
    areas_st %in% unique(readClusterSTDesc()$area)))
  testthat::expect_true(all(areas_st %in% unique(readClusterSTDesc()$area)))
  testthat::expect_true(all(mandatory_cols %in% colnames(input_st)))
  testthat::expect_true(nrow(input_st) == length(input_st$cluster))
})

# read empty study ----
test_that("test when study has no cluster (empty)", {
  path_empty_study <- setup_study_empty(sourcedir_empty_study)
  opts_study_test <- setSimulationPath(path_empty_study, simulation = "input")
  
  readClusterDesc()
})
