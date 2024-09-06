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
  districtsDef = data.table(district = as.factor("a and b"), area = as.factor(c("a", "b")))
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

library(stringr)
test_that("valid versions are transformed correctly", {
  expect_equal(transform_antares_version("9.0")$r, 900)
  expect_equal(transform_antares_version("9.45")$r, 945)
  expect_equal(transform_antares_version("10.10")$r, 1010)
  expect_equal(transform_antares_version("10.45")$r, 1045)
  expect_equal(transform_antares_version("12.12")$r, 1212)
})

test_that("invalid minor versions with more than 2 digits raise an error", {
  expect_error(transform_antares_version("10.113400000"), "Minor version exceeds 2 digits limit.")
  expect_error(transform_antares_version("9.1234"), "Minor version exceeds 2 digits limit.")
})

test_that("invalid major versions less than 9 raise an error", {
  expect_error(transform_antares_version("8.99"), "Major version must be 9 or higher.")
  expect_error(transform_antares_version("7.10"), "Major version must be 9 or higher.")
})

test_that("single numeric versions work correctly", {
  expect_equal(transform_antares_version("860")$r, 860)
  expect_equal(transform_antares_version("890")$r, 890)
})

test_that("correct output for to_write field", {
  expect_equal(transform_antares_version("9.0")$w, "9.0")
  expect_equal(transform_antares_version("9.45")$w, "9.45")
  expect_equal(transform_antares_version("10.10")$w, "10.10")
  expect_equal(transform_antares_version("12.12")$w, "12.12")
})
