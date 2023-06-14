
## v860 ----

path_study_test <- grep(pattern = "86", x = studyPathSV8, value = TRUE)
opts_study_test <- setSimulationPath(path_study_test, simulation = "input")

test_that("test read cluster st-storage v860", {
  
  # function setSimulationPath() provide areas names with st-storage clusters
  areas_st <- opts_study_test$areasWithSTClusters
  
  # read clusters st-storage informations
  input_st <- readClusterSTDesc()
  
  # tests
  testthat::expect_true("data.table" %in% class(input_st))
  testthat::expect_true(areas_st %in% unique(readClusterSTDesc()$area))
})
