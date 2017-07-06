#Copyright © 2016 RTE Réseau de transport d’électricité

context("Setup functions")
suppressPackageStartupMessages(require(lubridate))
suppressPackageStartupMessages(require(data.table))

# Reading of study options #####################################################

trueOpts <- list(
  studyName = "Test_packages_R",
  name = "test",
  mode = "Economy",
  synthesis = TRUE,
  yearByYear = TRUE,
  scenarios = TRUE,
  mcYears = c(1,2),
  antaresVersion = 500L,
  timeIdMin = 7 * 24 + 1,
  timeIdMax = 24 * 7 * 3,
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
  expect_error(setSimulationPath(file.path(studyPath, "..")))
})

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

test_that("Interactive mode if no study path provided", {
  skip_if_not(exists("choose.dir", getNamespace("utils")))
  with_mock(
    `utils::choose.dir` = function(...) {studyPath},
    {
      opts <- setSimulationPath()
      expect_equal(opts[names(trueOpts)], trueOpts)
    }
  )
})

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

test_that("select simulation interactively (number)", {
  with_mock(
    `base::scan` = function(...) {"1"},
    {
      expect_output(opts <- setSimulationPath(studyPath), "choose a simulation")
      expect_equal(opts[names(trueOpts)], trueOpts)
    }
  )
})

test_that("select simulation interactively (name)", {
  with_mock(
    scan = function(...) {"eco-test"},
    {
      expect_output(opts <- setSimulationPath(studyPath), "choose a simulation")
      expect_equal(opts[names(trueOpts)], trueOpts)
    }
  )
})

# Remove fake study
unlink(file.path(studyPath, "output/30000101-0000fake"), TRUE, TRUE)


# Study with no simulation #####################################################

# We rename the folder "output" to simulate a study without any simulation
# results
file.rename(file.path(studyPath, "output"), file.path(studyPath, "outputBack"))

test_that("No simulation", {
  test_that("Error if the user tries to read simulation results", {
    expect_error(setSimulationPath(studyPath, 1))
  })
  
  test_that("User can read input data", {
    expect_silent(opts <- setSimulationPath(studyPath, 0))
    expect_equal(opts$mode, "Input")
  })
  
  test_that("Read input data by default", {
    expect_silent(opts <- setSimulationPath(studyPath))
    expect_equal(opts$mode, "Input")
  })
})

file.rename(file.path(studyPath, "outputBack"), file.path(studyPath, "output"))

# Antares v6 ###################################################################

test_that("Folder 'maps' is not interpreted as a study (#49)", {
  dir.create(file.path(studyPath, "output", "maps"))
  expect_silent(opts <- setSimulationPath(studyPath, -1))
})
unlink(file.path(studyPath, "output", "maps"))

# Correction of start date #####################################################

describe(".getStartDate", {
  mNames <- c("january", "february", "march", "april", "may", "june", "july",
              "september", "october", "november", "december")
  dNames <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", 
              "Saturday")
  
  for (month in mNames) {
    for (day in dNames) {
      for (leapyear in c(FALSE, TRUE)) {
        params <- list(general = list(
          horizon = 2018,
          `first-month-in-year` = month,
          leapyear = leapyear,
          january.1st = day
        ))
        
        it(sprintf("corrects the year of the study (%s, %s, %s)", month, day, leapyear), {
          start <- suppressWarnings(.getStartDate(params))
          # start compatible with january.1st?
          if (month == "january") {
            expect_equal(wday(start), which(dNames == day))
          } else {
            start2 <- start
            year(start2) <- year(start) + 1
            month(start2) <- 1
            expect_equal(wday(start2), which(dNames == day))
          }
          # Start compatible with leapyear?horizon = 2018, 
          if (month %in% c("january", "february")) {
            expect_equal(leap_year(year(start)), leapyear)
          } else {
            expect_equal(leap_year(year(start) + 1), leapyear)
          }
        })
      }
    }
  }
})
