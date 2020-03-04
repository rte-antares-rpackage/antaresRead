#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function .getStartDate")

sapply(studyPathS, function(studyPath){

# Correction of start date #####################################################

describe(".getStartDate", {
  library(lubridate)
  mNames <- c("january", "february", "march", "april", "may", "june", "july","august",
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
        
        describe(sprintf("corrects the year of the study (%s, %s, %s)", month, day, leapyear), {
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
            expect_equal(lubridate::leap_year(year(start)), leapyear)
          } else {
            expect_equal(lubridate::leap_year(year(start) + 1), leapyear)
          }
        })
      }
    }
  }
})
  
test_that(".getStartDate must work with default param", {
  params <- list(general = list(
    horizon = 2018,
    `first-month-in-year` = "july",
    leapyear = FALSE,
    january.1st = "Tuesday"
  ))
  
  start <- suppressWarnings(.getStartDate(params))
  expect_equal(start, as.POSIXct("2018-07-01", tz ="UTC"))
  
})

test_that(".getStartDate must work with leap year ", {
  params <- list(general = list(
    horizon = 2019,
    `first-month-in-year` = "july",
    leapyear = TRUE,
    january.1st = "Wednesday"
  ))
  
  start <- suppressWarnings(.getStartDate(params))
  expect_equal(start, as.POSIXct("2019-07-01", tz ="UTC"))
  
  params <- list(general = list(
    horizon = 2020,
    `first-month-in-year` = "january",
    leapyear = TRUE,
    january.1st = "Wednesday"
  ))
  
  start <- suppressWarnings(.getStartDate(params))
  expect_equal(start, as.POSIXct("2020-01-01", tz ="UTC"))
  
  params <- list(general = list(
    horizon = 2020,
    `first-month-in-year` = "july",
    leapyear = FALSE,
    january.1st = "Friday"
  ))
  
  start <- suppressWarnings(.getStartDate(params))
  expect_equal(start, as.POSIXct("2020-07-01", tz ="UTC"))
})

test_that(".getStartDate must work with a month different from July", {
  params <- list(general = list(
    horizon = 2018,
    `first-month-in-year` = "october",
    leapyear = FALSE,
    january.1st = "Tuesday"
  ))
  
  start <- suppressWarnings(.getStartDate(params))
  expect_equal(start, as.POSIXct("2018-10-01", tz ="UTC"))
  
  params <- list(general = list(
    horizon = 2018,
    `first-month-in-year` = "august",
    leapyear = FALSE,
    january.1st = "Tuesday"
  ))
  
  start <- suppressWarnings(.getStartDate(params))
  expect_equal(start, as.POSIXct("2018-08-01", tz ="UTC"))
  
  params <- list(general = list(
    horizon = 2019,
    `first-month-in-year` = "february",
    leapyear = FALSE,
    january.1st = "Wednesday"
  ))
  
  start <- suppressWarnings(.getStartDate(params))
  expect_equal(start, as.POSIXct("2019-02-01", tz ="UTC"))
  
  params <- list(general = list(
    horizon = 2020,
    `first-month-in-year` = "january",
    leapyear = TRUE,
    january.1st = "Wednesday"
  ))
  
  start <- suppressWarnings(.getStartDate(params))
  expect_equal(start, as.POSIXct("2020-01-01", tz ="UTC"))
  
})

})
