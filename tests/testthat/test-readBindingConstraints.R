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
  constraints[[1]]$properties$operator <- "both"
  sumConstraints <- summary(constraints)
  expect_is(sumConstraints, "data.frame")
  expect_true(all(c("enabled", "timeStep", "equation") %in% 
                    names(sumConstraints)))
})  


# >= v800 ----

opts <- antaresRead::setSimulationPath(studyPathSV8[1], "input")

test_that("test if exist data value file", {
  bc <- antaresRead::readBindingConstraints(opts = opts)
  
  # test class object return
  testthat::expect_equal(class(bc), "bindingConstraints")

  names_bc_test <- paste0(names(bc), ".txt")
  path_test_bc <- paste0(file.path(opts$inputPath, "bindingconstraints", 
                                   names_bc_test))
  
  # test if values files exists
  testthat::expect_true(all(unlist(lapply(path_test_bc, file.exists))))
})


# >= v870 ----

# read latest version of empty study
study_empty_latest_version <- setup_study_empty(sourcedir_empty_study)

opts_test_empty <- antaresRead::setSimulationPath(study_empty_latest_version,
                                            "input")

# read latest version study
path_study_test <- grep(pattern = "test_case_study_v870", x = studyPathSV8, value = TRUE)
opts_study_test <- setSimulationPath(path_study_test, simulation = "input")

## empty study test ----
test_that("Read scenarised BC with empty study", {
  
  bc <- readBindingConstraints(opts = opts_test_empty)
  
  # test exception
  testthat::expect_equal(bc, NULL)
  
})

## test object structure ----
test_that("test object structure", {

  bc <- readBindingConstraints(opts = opts_study_test)

  # test class object 
  testthat::expect_equal(class(bc), "bindingConstraints")
  
  # test structure object
    # every BC must have 3 named elements
  nb_items <- unlist(lapply(bc, length))
  
  testthat::expect_true(all(nb_items %in% 3))
  
    # every BC must be named 
  names_items <- unlist(lapply(bc, names))
  
  testthat::expect_true(all(names_items 
                            %in% c("properties", "coefs", "values")))
})

# test values / operator ----
test_that("data verification based on the 'operator' property", {  
  bc <- readBindingConstraints(opts = opts_study_test)
  
  # test values
    # one or two txt files are associated with one BC
    # only "both" values have two .txt files
  bc_prop <- lapply(bc, `[[`, "properties")
  bc_id <- lapply(bc_prop, `[[`, "id")
  bc_operator <- lapply(bc_prop, `[[`, "operator")
  
    # list all values 
  path_bc_values <- file.path(opts_study_test$inputPath, 
                             "bindingconstraints")
  list_values_bc <- list.files(path = path_bc_values, pattern = ".txt")
  
    # test "less" (one file must be present)
  index <- bc_operator %in% "less"
  id_bc <- names(bc_operator[index])
  
  nb_file_present <- length(grep(pattern = id_bc, 
                                x = list_values_bc))
  
  testthat::expect_true(nb_file_present %in% 1)
  
    # test "greater" (one file must be present)
  index <- bc_operator %in% "greater"
  id_bc <- names(bc_operator[index])
  
  nb_file_present <- length(grep(pattern = id_bc, 
                                 x = list_values_bc))
  
  testthat::expect_true(nb_file_present %in% 1)
  
    # test "equal" (one file must be present)
  index <- bc_operator %in% "equal"
  id_bc <- names(bc_operator[index])
  
  nb_file_present <- length(grep(pattern = id_bc, 
                                 x = list_values_bc))
  
  testthat::expect_true(nb_file_present %in% 1)
  
    # test "both" (two file must be present)
  index <- bc_operator %in% "both"
  id_bc <- names(bc_operator[index])
  
  nb_file_present <- length(grep(pattern = id_bc, 
                                 x = list_values_bc))
  
  testthat::expect_true(nb_file_present %in% 2)
  
})

## test default values ----
test_that("test if default values are well returned", {  
  bc <- readBindingConstraints(opts = opts_study_test)
  
  # if txt values files are empty, default value is returned 
  
  # check empty values files
    # list all values 
  path_bc_values <- file.path(opts_study_test$inputPath,
                              "bindingconstraints")
  
  list_path_values_bc <- list.files(path = path_bc_values, 
                                    pattern = ".txt", 
                                    full.names = TRUE)
  list_values_bc <- list.files(path = path_bc_values, pattern = ".txt")
  
  # check empty size
  is_value_null <- file.size(list_path_values_bc) %in% 0
  
  list_values_bc_null <- list_values_bc[is_value_null]
  
  # check bindings concerned
  list_null_values <- lapply(names(bc), 
                             grepl, 
                             x= list_values_bc_null)
  
  is_true_null <- unlist(lapply(list_null_values, 
                                all))
  
  bc_default_values <- bc[is_true_null]
  
  # two data frames are returned for "both" case
  less_values_default <- bc_default_values[[names(bc_default_values)]]$values$less
  testthat::expect_true(sum(less_values_default) %in% 0)
  
  greater_values_default <- bc_default_values[[names(bc_default_values)]]$values$greater
  testthat::expect_true(sum(greater_values_default) %in% 0)
  
  # test if length of default values are ok with timestep
    # daily => 364 
  testthat::expect_true(dim(bc_default_values[[names(bc_default_values)]]$values$greater)[1] 
                        %in% 364)
  })


test_that("test if with_time_series and constraint_names arguments have the expected behaviour", { 
  
  opts <- list(
  "inputPath" = tempdir(),
  "typeLoad"= "not_api",
  "areaList" = c("fr", "de"),
  "antaresVersion" = 880
  )

  ini_binding <- c(
    "[0]",
    "name = batteries_fr",
    "id = batteries_fr",
    "enabled = true",
    "type = daily",
    "operator = equal",
    "filter-year-by-year = hourly, daily, weekly, monthly, annual",
    "filter-synthesis = hourly, daily, weekly, monthly, annual",
    "group = default",
    "fr.fr_cl_1 = 1.000000",
    "fr.fr_cl_2 = -1.000000",
    "fr.fr_cl_3 = -2.000000",
    "",
    "[1]",
    "name = batteries_de",
    "id = batteries_de",
    "enabled = true",
    "type = daily",
    "operator = equal",
    "filter-year-by-year = hourly, daily, weekly, monthly, annual",
    "filter-synthesis = hourly, daily, weekly, monthly, annual",
    "group = default",
    "de.de_cl_1 = 11.000000",
    "de.de_cl_2 = -11.000000",
    "de.de_cl_3 = -22.000000",
    "",
    "[2]",
    "name = mix_fr_de",
    "id = mix_fr_de",
    "enabled = true",
    "type = daily",
    "operator = less",
    "filter-year-by-year = hourly, daily, weekly, monthly, annual",
    "filter-synthesis = hourly, daily, weekly, monthly, annual",
    "group = default",
    "fr.fr_cl_1 = 23.000000",
    "de.de_cl_1 = -45.000000",
    ""
  )

  bindingconstraints_path <- file.path(tempdir(),"bindingconstraints")
  dir.create(bindingconstraints_path, recursive = TRUE, showWarnings = FALSE)
  writeLines(ini_binding, file.path(bindingconstraints_path,"bindingconstraints.ini"))
  write.table(matrix(rep(1,366 * 2), ncol = 2), file = file.path(bindingconstraints_path,"batteries_fr_eq.txt"), row.names = FALSE, col.names = FALSE)
  write.table(matrix(rep(2,366 * 2), ncol = 2), file = file.path(bindingconstraints_path,"batteries_de_eq.txt"), row.names = FALSE, col.names = FALSE)
  write.table(matrix(rep(3,366 * 2), ncol = 2), file = file.path(bindingconstraints_path,"mix_fr_de_lt.txt"), row.names = FALSE, col.names = FALSE)
  
  all_bc <- readBindingConstraints(opts = opts, with_time_series = FALSE)
  expect_true(inherits(all_bc, what = "bindingConstraints"))
  has_values <- lapply(all_bc, FUN = function(bc) {"values" %in% names(bc)})
  expect_true(all(has_values == FALSE))
  
  all_bc <- readBindingConstraints(opts = opts, with_time_series = TRUE)
  expect_true(inherits(all_bc, what = "bindingConstraints"))
  has_values <- lapply(all_bc, FUN = function(bc) {"values" %in% names(bc)})
  expect_true(all(has_values == TRUE))
  
  expect_warning(readBindingConstraints(opts = opts, constraint_names = "not_a_binding_constraint"),
                 regexp = "No binding constraints with one of the names you provide as argument."
                 )

  my_constraints <- c("batteries_fr", "batteries_de")
  bcs <- readBindingConstraints(opts = opts, constraint_names = my_constraints)
  expect_true(length(bcs) == 2)
  expect_equal(sort(names(bcs)), sort(my_constraints))

  my_constraints <- c("batteries_fr", "batteries_de", "not_a_binding_constraint")
  bcs <- readBindingConstraints(opts = opts, constraint_names = my_constraints)
  expect_true(length(bcs) == 2)
  
  my_constraints <- c("baTTeries_FR", "baTTeries_DE")
  bcs <- readBindingConstraints(opts = opts, constraint_names = my_constraints)
  expect_true(length(bcs) == 2)
  expect_equal(sort(tolower(names(bcs))), sort(tolower(my_constraints)))
  
  bcs <- readBindingConstraints(opts = opts, constraint_names = NULL)
  expect_true(length(bcs) == 3)
  expect_equal(sort(names(bcs)), c("batteries_de", "batteries_fr", "mix_fr_de"))
})


test_that("test the content of the return object", { 
  
  opts <- list(
  "inputPath" = tempdir(),
  "typeLoad"= "not_api",
  "areaList" = c("fr", "de"),
  "antaresVersion" = 880
  )

  ini_binding <- c(
    "[0]",
    "name = batteries_fr",
    "id = batteries_fr",
    "enabled = true",
    "type = daily",
    "operator = equal",
    "filter-year-by-year = hourly, daily, weekly, monthly, annual",
    "filter-synthesis = hourly, daily, weekly, monthly, annual",
    "group = default",
    "fr.fr_cl_1 = 1.000000",
    "fr.fr_cl_2 = -1.000000",
    "fr.fr_cl_3 = -2.000000",
    "",
    "[1]",
    "name = batteries_de",
    "id = batteries_de",
    "enabled = true",
    "type = daily",
    "operator = equal",
    "comments = ",
    "filter-year-by-year = hourly, daily, weekly, monthly, annual",
    "filter-synthesis = hourly, daily, weekly, monthly, annual",
    "group = default",
    "de.de_cl_1 = 1.000000",
    "de.de_cl_2 = -1.000000",
    "de.de_cl_3 = -2.000000",
    "",
    "[2]",
    "name = mix_fr_de",
    "id = mix_fr_de",
    "enabled = true",
    "type = daily",
    "operator = less",
    "comments = CC de niveau 1",
    "filter-year-by-year = hourly, daily, weekly, monthly, annual",
    "filter-synthesis = hourly, daily, weekly, monthly, annual",
    "group = default",
    "fr.fr_cl_1 = 1.000000",
    "de.de_cl_1 = -1.000000",
    ""
  )

  bindingconstraints_path <- file.path(tempdir(),"bindingconstraints")
  dir.create(bindingconstraints_path, recursive = TRUE, showWarnings = FALSE)
  writeLines(ini_binding, file.path(bindingconstraints_path,"bindingconstraints.ini"))
  write.table(matrix(rep(1,366 * 2), ncol = 2), file = file.path(bindingconstraints_path,"batteries_fr_eq.txt"), row.names = FALSE, col.names = FALSE)
  write.table(matrix(rep(1,366 * 2), ncol = 2), file = file.path(bindingconstraints_path,"batteries_de_eq.txt"), row.names = FALSE, col.names = FALSE)
  write.table(matrix(rep(2,366 * 2), ncol = 2), file = file.path(bindingconstraints_path,"mix_fr_de_lt.txt"), row.names = FALSE, col.names = FALSE)
  
  # with second members
  in_properties <- c("name", "id", "enabled", "timeStep", "operator", "comments", "filter-year-by-year", "filter-synthesis", "group")
  
  all_bc <- readBindingConstraints(opts = opts)
  expect_true(inherits(all_bc, what = "bindingConstraints"))
  expected_names <- c("properties","coefs","values")
  has_expected_length <- lapply(all_bc, FUN = function(bc) {length(names(bc)) == length(expected_names)})
  expect_true(all(has_expected_length == TRUE))
  has_expected_names <- lapply(all_bc, FUN = function(bc) {all(expected_names %in% names(bc))})
  expect_true(all(has_expected_names == TRUE))
  has_expected_properties <- lapply(all_bc, FUN = function(bc) {
    all(in_properties %in% names(bc[["properties"]]))
    }
  )
  expect_true(all(has_expected_properties == TRUE))
  
  # without second members
  all_bc <- readBindingConstraints(opts = opts, with_time_series = FALSE)
  expect_true(inherits(all_bc, what = "bindingConstraints"))
  expected_names <- c("properties","coefs")
  has_expected_length <- lapply(all_bc, FUN = function(bc) {length(names(bc)) == length(expected_names)})
  expect_true(all(has_expected_length == TRUE))
  has_expected_names <- lapply(all_bc, FUN = function(bc) {all(expected_names %in% names(bc))})
  expect_true(all(has_expected_names == TRUE))
  has_expected_properties <- lapply(all_bc, FUN = function(bc) {
    all(in_properties %in% names(bc[["properties"]]))
    }
  )
  expect_true(all(has_expected_properties == TRUE))
})
