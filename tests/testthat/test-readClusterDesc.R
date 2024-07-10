# v710----

## Thermal ----
test_that("test read cluster", {
  path_study_test <- studyPathS
  opts_study_test <- setSimulationPath(path_study_test, simulation = "input")
  
  # function setSimulationPath() provide areas names with st-storage clusters
  areas <- opts_study_test$areasWithClusters
  
  # read clusters informations
  input <- readClusterDesc()
  
  # tests
  testthat::expect_true("data.table" %in% class(input))
  testthat::expect_true(all(areas %in% unique(input$area)))
  
  # tests if all colnames are returned according to ref
  ref_thermal <- antaresRead:::pkgEnv$inputProperties[
    Category%in%"thermal"]
  ref_thermal <- ref_thermal[`Version Antares` <= 
                               opts_study_test$antaresVersion | 
                               `Version Antares` %in% NA]
  
    # "name" in INI file corresponding "cluster" in output
  testthat::expect_true(all(
    setdiff(ref_thermal$`INI Name`, "name")%in%
      setdiff(colnames(input), c("area", "cluster"))))
  
  # object returned is one line per area/cluster
  testthat::expect_equal(nrow(input), nrow(unique(input)))
})

## Renewables ----
test_that("test read cluster renewables", {
  path_study_test <- grep(pattern = "test_case_study_v870", 
                          x = studyPathSV8, value = TRUE)
  opts_study_test <- setSimulationPath(path_study_test, simulation = "input")
  
  # function setSimulationPath() provide areas names with st-storage clusters
  areas_res <- opts_study_test$areasWithResClusters
  
  #read
  input <- readClusterResDesc()
  
  # tests
  testthat::expect_true("data.table" %in% class(input))
  testthat::expect_true(all(areas_res %in% unique(input$area)))
  
  # tests if all colnames are returned according to ref
  ref_res <- antaresRead:::pkgEnv$inputProperties[
    Category%in%"renewable"]
  ref_res <- ref_res[`Version Antares` <= 
                       opts_study_test$antaresVersion | 
                       `Version Antares` %in% NA]
  
  # "name" in INI file corresponding "cluster" in output
  testthat::expect_true(all(
    setdiff(ref_res$`INI Name`, "name")%in%
      setdiff(colnames(input), c("area", "cluster"))))
  
  # object returned is one line per area/cluster
  testthat::expect_equal(nrow(input), nrow(unique(input)))
})

# v860 ----
## st-storage ----
test_that("test read cluster st-storage v860", {
  path_study_test <- grep(pattern = "test_case_study_v870", 
                          x = studyPathSV8, value = TRUE)
  opts_study_test <- setSimulationPath(path_study_test, simulation = "input")
  
  # function setSimulationPath() provide areas names with st-storage clusters
  areas_st <- opts_study_test$areasWithSTClusters
  
  # read clusters st-storage informations
  input_st <- readClusterSTDesc()
  
  # tests
  testthat::expect_true("data.table" %in% class(input_st))
  testthat::expect_true(all(
    areas_st %in% unique(readClusterSTDesc()$area)))
  
  # tests if all colnames are returned according to ref
  ref_st <- antaresRead:::pkgEnv$inputProperties[
    Category%in%"storage"]
  ref_st <- ref_st[`Version Antares` <= 
                       opts_study_test$antaresVersion | 
                       `Version Antares` %in% NA]
  
  # "name" in INI file corresponding "cluster" in output
  testthat::expect_true(all(
    setdiff(ref_st$`INI Name`, "name")%in%
      setdiff(colnames(input_st), c("area", "cluster"))))
  
  # object returned is one line per area/cluster
  testthat::expect_equal(nrow(input_st), nrow(unique(input_st)))
})

# read empty study ----
test_that("test when study has no cluster (empty)", {
  path_empty_study <- setup_study_empty(sourcedir_empty_study)
  opts_study_test <- setSimulationPath(path_empty_study, simulation = "input")
  
  testthat::expect_equal(readClusterDesc(), 
                         data.table::data.table()) 
})
