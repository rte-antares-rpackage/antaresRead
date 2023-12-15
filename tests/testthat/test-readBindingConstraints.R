context("bindingConstraints")

# >= v710 ----
opts <- setSimulationPath(studyPathS, 0)

test_that("returns an object of class 'bindingConstraints'", {
  constraints <- readBindingConstraints(opts)
  expect_is(constraints, "bindingConstraints")
})
  
test_that("summary.bindingConstraints",{
  constraints <- readBindingConstraints(opts)
  sumConstraints <- summary(constraints)
  expect_is(sumConstraints, "data.frame")
  expect_true(all(c("enabled", "timeStep", "equation") %in% 
                    names(sumConstraints)))
})
  
  
test_that("Both operator",{
  constraints <- readBindingConstraints(opts)
  constraints[[1]]$operator <- "both"
  sumConstraints <- summary(constraints)
  expect_is(sumConstraints, "data.frame")
  expect_true(all(c("enabled", "timeStep", "equation") %in% 
                    names(sumConstraints)))
})  

##delete study ----
#unlink(studyPathS, recursive = TRUE)  

# >= v800 ----
#  
opts <- antaresRead::setSimulationPath(studyPathSV8[1], "input")

test_that("test if exist data value file", {
  bc <- antaresRead::readBindingConstraints(opts = opts)
  
  # test class object return
  testthat::expect_equal(class(bc), "bindingConstraints")

  names_bc_test <- paste0(names(bc), ".txt")
  path_test_bc <- paste0(file.path(opts$inputPath, "bindingconstraints", names_bc_test))
  
  # test if values files exists
  testthat::expect_true(all(unlist(lapply(path_test_bc, file.exists))))
})


# >= v870 ----
# 

# read latest version study
path_study_test <- grep(pattern = "87", x = studyPathSV8, value = TRUE)
opts_study_test <- setSimulationPath(path_study_test, simulation = "input")

test_that("Read scenarised BC", {
  
  bc <- readBindingConstraints(opts = opts_study_test)
  
  # test class object return
  testthat::expect_equal(class(bc), "bindingConstraints")
  
})
