test_that("readBindingConstraint", {
  
  # test for versions v710 v800 v850
  all_path_studies <- c(studyPathS, studyPathSV8)
  sapply(all_path_studies, function(study) {
    opts <- antaresRead::setSimulationPath(study, "input")
    
    bc <- antaresRead::readBindingConstraints(opts = opts)
    # test class object return
    testthat::expect_equal(class(bc), "bindingConstraints")
    
    names_bc_test <- paste0(names(bc), ".txt")
    path_test_bc <- paste0(file.path(opts$inputPath, "bindingconstraints", names_bc_test))
    # test if values files exists
    testthat::expect_true(all(unlist(lapply(path_test_bc, file.exists))))
  })
})
