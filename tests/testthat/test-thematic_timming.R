# read study  ----
path_study_test <- grep(pattern = "86", x = studyPathSV8, value = TRUE)
opts_study_test <- setSimulationPath(path_study_test, simulation = "input")

# Empty section "selection variables" ----
test_that("Empty selection variables", {
  # template study 860 as no section
  
  # read general data [variables selection] (thematic trimming)
  testthat::expect_warning(
    getThematicTrimming(opts = opts_study_test), 
    regexp = "`variables selection` section in file 'generaldata.ini' does not exist"
  )
  })

# ADD VARIABLES ----
test_that("read selection variables (+)", {
    # use generaldata test file 
    # copy and paste it in study
    # rename and paste it 
  path_file <- system.file("variables_selection/generaldata_add_var.ini", 
                           package = "antaresRead")
  
  # copy
  path_file_study <- file.path(opts_study_test$studyPath, "settings")
  file.copy(from = path_file, to = path_file_study)
  
  # remove
  file.remove(file.path(path_file_study, "generaldata.ini"))
  
  # rename
  file.rename(from = file.path(path_file_study, "generaldata_add_var.ini"), 
              to = file.path(path_file_study, "generaldata.ini"))
  
  # read general data [variables selection] (thematic trimming)
  read_thematic <- getThematicTrimming(opts = opts_study_test)
  
  # tests
  testthat::expect_true(!is.null(read_thematic))
  testthat::expect_true(all(names(read_thematic)%in%
                          c("variables", "status_selection")))
    # check variables names according to antares version
  antares_version <- opts_study_test$antaresVersion
  filter_vars_version <- pkgEnv$thematic[version<=antares_version,]
  
  testthat::expect_true(all(filter_vars_version$variable%in%
                          read_thematic$variables))
    # check status values
  pattern_add <- "select_var +"
  list_var_to_add <- readIni("settings/generaldata")
  list_var_to_add <- list_var_to_add$`variables selection`
  
  check_pattern_add <- grepl(pattern = pattern_add, 
                             x = names(list_var_to_add))
  var_selection <- unlist(list_var_to_add[check_pattern_add], 
                          use.names = FALSE)
  
  check_index <- read_thematic$variables %in% var_selection
  check_values <- read_thematic$variables[check_index]
  
  testthat::expect_equal(check_values, var_selection)
  testthat::expect_true(all(read_thematic[check_index,]$status_selection%in%
                          "active"))
  
})


# REMOVE VARIABLES ----
test_that("read selection variables (-)", {
  # use generaldata test file 
  # copy and paste it in study
  # rename and paste it 
  path_file <- system.file("variables_selection/generaldata_remove_var.ini", 
                           package = "antaresRead")
  
  # copy
  path_file_study <- file.path(opts_study_test$studyPath, "settings")
  file.copy(from = path_file, to = path_file_study)
  
  # remove
  file.remove(file.path(path_file_study, "generaldata.ini"))
  
  # rename
  file.rename(from = file.path(path_file_study, "generaldata_remove_var.ini"), 
              to = file.path(path_file_study, "generaldata.ini"))
  
  # read general data [variables selection] (thematic trimming)
  read_thematic <- getThematicTrimming(opts = opts_study_test)
  
  # tests
  testthat::expect_true(!is.null(read_thematic))
  testthat::expect_true(all(names(read_thematic)%in%
                              c("variables", "status_selection")))
  # check variables names according to antares version
  antares_version <- opts_study_test$antaresVersion
  filter_vars_version <- pkgEnv$thematic[version<=antares_version,]
  
  testthat::expect_true(all(filter_vars_version$variable%in%
                              read_thematic$variables))
  # check status values
  pattern_remove <- "select_var -"
  list_var_to_remove <- readIni("settings/generaldata")
  list_var_to_remove <- list_var_to_remove$`variables selection`
  
  check_pattern_remove <- grepl(pattern = pattern_remove, 
                                x = names(list_var_to_remove), 
                                fixed = TRUE)
  var_selection <- unlist(list_var_to_remove[check_pattern_remove], 
                          use.names = FALSE)
  
  check_index <- read_thematic$variables %in% var_selection
  check_values <- read_thematic$variables[check_index]
  
  testthat::expect_equal(check_values, var_selection)
  testthat::expect_true(all(read_thematic[check_index,]$status_selection%in%
                          "skip"))
})

# delete study ----
unlink(opts_study_test$studyPath, recursive = TRUE)
