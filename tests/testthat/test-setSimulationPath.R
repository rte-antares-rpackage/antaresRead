#Copyright © 2016 RTE Réseau de transport d’électricité

context("Setup functions")

# v710----
sapply(studyPathS, function(studyPath){
  
# suppressPackageStartupMessages(require(lubridate))
# suppressWarnings(suppressPackageStartupMessages(require(data.table)))

# Reading of study options #####################################################

trueOpts <- list(
  studyName = "Test_packages_R",
  name = "test",
  mode = "Economy",
  synthesis = TRUE,
  yearByYear = TRUE,
  scenarios = TRUE,
  mcYears = c(1,2),
  # antaresVersion = if (identical(basename(dirname(studyPath)), "v6")) {
  #   600L
  # } else {
  #   710L
  # },
  timeIdMin = (firstDay-1) * 24 + 1,
  timeIdMax = lastDay*24,
  start = as.POSIXlt("2018-01-01", tz = "UTC"),
  firstWeekday = "Monday",
  areaList = c("a", "a_offshore", "b", "c", "hub", "psp in", "psp in-2", "psp out", 
               "psp out-2"),
  districtList = "a and b",
  linkList = c("a - a_offshore", "a - b", "a - psp in", "a - psp out", "b - c", 
               "b - psp in", "b - psp out", "c - hub", "hub - psp in-2", 
               "hub - psp out-2"),
  areasWithClusters = c("a", "b", "c", "psp in", "psp in-2", "psp out", "psp out-2"),
  districtsDef = data.table(district = as.factor("a and b"), area = as.factor(c("a", "b"))),
  verbose = FALSE
)

test_that("setSimulationPath reads correct values", {
  opts <- setSimulationPath(studyPath)
  expect_equal(opts[names(trueOpts)], trueOpts)
})

test_that("R option 'antares' is set", {
  opts <- setSimulationPath(studyPath)
  expect_identical(opts, getOption("antares"))
})

test_that("setSimulationPath fails if path is not an antares Ouput directory", {
  expect_error(setSimulationPath(file.path(studyPath, "../..")))
})

opts <- setSimulationPath(studyPath)

test_that("setSimulationPath can read info in input", {
  opts <- setSimulationPath(studyPath, "input")
  for (v in c("studyName", "areaList", "districtList", "linkList",
              "areasWithClusters", "timeIdMin", "timeIdMax", "start",
              "firstWeekday")) {
    expect_equal(opts[[v]], trueOpts[[v]])
  }
})

test_that("setSimulationPath works if synthesis and some MC years are not saved (#31)", {
  opts <- setSimulationPath(studyPath)

  file.rename(file.path(opts$simDataPath, "mc-all"),
              file.path(opts$simDataPath, "mc-all_back"))
  file.rename(file.path(opts$simDataPath, "mc-ind/00001"),
              file.path(opts$simDataPath, "mc-ind/00001_back"))

  opts <- setSimulationPath(studyPath)
  trueOpts$synthesis <- FALSE
  trueOpts$mcYears <- 2
  expect_equal(opts[names(trueOpts)], trueOpts)

  file.rename(file.path(opts$simDataPath, "mc-all_back"),
              file.path(opts$simDataPath, "mc-all"))
  file.rename(file.path(opts$simDataPath, "mc-ind/00001_back"),
              file.path(opts$simDataPath, "mc-ind/00001"))
})

# Simulation selection #########################################################

# test_that("Interactive mode if no study path provided", {
#   skip_if_not(exists("choose.dir", getNamespace("utils")))
#   with_mock(
#     `utils::choose.dir` = function(...) {studyPath},
#     {
#       opts <- setSimulationPath()
#       expect_equal(opts[names(trueOpts)], trueOpts)
#     }
#   )
# })

# Create a fake simulation (just an empty directory)
dir.create(file.path(studyPath, "output/30000101-0000fake"))

test_that("Select simulation with path", {
  f <- list.files(file.path(studyPath, "output"), full.names = TRUE)
  opts <- setSimulationPath(f[1])
  expect_equal(opts[names(trueOpts)], trueOpts)
})

test_that("Select simulation by name", {
  opts <- setSimulationPath(studyPath, "eco-test")
  expect_equal(opts[names(trueOpts)], trueOpts)
})

test_that("Select simulation by index", {
  opts <- setSimulationPath(studyPath, 1)
  expect_equal(opts[names(trueOpts)], trueOpts)
})

test_that("select simulation with negative index", {
  opts <- setSimulationPath(studyPath, -2)
  expect_equal(opts[names(trueOpts)], trueOpts)
})
#
# test_that("select simulation interactively (number)", {
#   with_mock(
#     `base::scan` = function(...) {"1"},
#     {
#       expect_output(opts <- setSimulationPath(studyPath), "choose a simulation")
#       expect_equal(opts[names(trueOpts)], trueOpts)
#     }
#   )
# })
#
# test_that("select simulation interactively (name)", {
#   with_mock(
#     scan = function(...) {"eco-test"},
#     {
#       expect_output(opts <- setSimulationPath(studyPath), "choose a simulation")
#       expect_equal(opts[names(trueOpts)], trueOpts)
#     }
#   )
# })

test_that("Bad multiple path", {
  expect_error(setSimulationPath(studyPathSV8, simulation = "input"), 
               regexp = "Only one path is required")
})

# Remove fake study
unlink(file.path(studyPath, "output/30000101-0000fake"), TRUE, TRUE)


# Study with no simulation #####################################################

# We rename the folder "output" to simulate a study without any simulation
# results
file.rename(file.path(studyPath, "output"), file.path(studyPath, "outputBack"))

describe("No simulation", {
  it("Error if the user tries to read simulation results", {
    expect_error(setSimulationPath(studyPath, 1))
  })

  it("User can read input data", {
    expect_silent(opts <- setSimulationPath(studyPath, 0))
    expect_equal(opts$mode, "Input")
  })

  it("Read input data by default", {
    expect_silent(opts <- setSimulationPath(studyPath))
    expect_equal(opts$mode, "Input")
  })
})

file.rename(file.path(studyPath, "outputBack"), file.path(studyPath, "output"))

# Antares v6 ###################################################################

test_that("Folder 'maps' is not interpreted as a study (#49)", {
  expect_silent(opts <- setSimulationPath(studyPath, -1))
})

test_that("No meta info areas with a ST cluster < 860", {
  opts <- setSimulationPath(studyPath, "input")
  expect_true(length(opts$areasWithSTClusters)==0)
})

test_that("No meta info binding study < 870", {
  opts <- setSimulationPath(studyPath, "input")
  expect_null(opts$binding)
})

})

# v860----
test_that("New meta data for areas with a ST cluster", {
  # read latest version study
  path_study_test <- grep(pattern = "test_case_study_v870", x = studyPathSV8, value = TRUE)
  opts_study_test <- setSimulationPath(path_study_test, simulation = "20240105-0934eco")
  
  expect_false(is.null(opts_study_test$areasWithSTClusters))
})


# v870----
test_that("New meta data for group dimension of binding constraints", {
  # read latest version study
  path_study_test <- grep(pattern = "test_case_study_v870", x = studyPathSV8, value = TRUE)
  opts_study_test <- setSimulationPath(path_study_test, simulation = "input")
  
  expect_is(opts_study_test$binding, "data.table")
})

# v900----
## test private ----
test_that("read new format version from .antares file", {
  test_path_files <- system.file("test_files", package = "antaresRead")
  
  # "readIniFile" conversion problem (9.0 => 9)
  antares_file <- file.path(test_path_files, 
                               "antares_version_files", 
                               "antares_version_float.antares")
  
  file_to_read <- readIniFile(file = antares_file, stringsAsFactors = TRUE)
  
  version_value <- file_to_read$antares$version
  
  # test right conversion for package
  expect_equal(.transform_antares_version(version_value), 900)
  
  # exception max digit minor 
  expect_error(.transform_antares_version(9.111), 
               regexp = "Invalid antares_version format")
  
  # read right format file (9.99)
  antares_file <- file.path(test_path_files, 
                               "antares_version_files", 
                               "antares_version_float_2digit.antares")
  
  file_to_read <- readIniFile(file = antares_file, stringsAsFactors = TRUE)
  
  version_value <- file_to_read$antares$version
  
  # test right conversion for package
  expect_equal(.transform_antares_version(version_value), 999)
})

## study ----
test_that("read new format version from study", {
  path <- setup_study_empty(dir_path = sourcedir_empty_study)
  opts_study_test <- setSimulationPath(path)
  
  # "hack" study and paste test file with version "9.0"
  test_path_files <- system.file("test_files", package = "antaresRead")
  antares_file <- file.path(test_path_files, 
                            "antares_version_files", 
                            "antares_version_float.antares")
  
  # delete "study.antares"
  file_to_remove <- file.path(opts_study_test$studyPath, "study.antares") 
  file.remove(file_to_remove)
  file.copy(from = antares_file, to = file_to_remove)
  
  # read study 
  study <- setSimulationPath(path)
  
  # test right conversion for package
  expect_equal(study$antaresVersion, 900)
})

# v920----
test_that("Check version pb (9.2*100)<920 TRUE ?", {
  # check `?double` (details section Double-precision values)
  # 9.2*100)<920 return TRUE localy 
  # (9.2*100)-920 return -1.136868e-13 
  # check version on 9.2 don't work 
  
  # context
  expect_error(
    expect_false((9.2*100)<920)
  )
  
  expect_error(
    expect_identical((9.2*100)-920, 0)
  )
  
  expect_identical(round(9.2*100)-920, 0)
  
  # just test private function
  expect_false(
    .transform_antares_version("9.2")<920
  )
})
