context(".timeIdToDate")
suppressWarnings(suppressPackageStartupMessages(require(lubridate)))

describe(".timeIdToDate", {
  
  opts <- list(
    start = as.POSIXlt("2017-01-01", tz = "UTC"), 
    firstWeekday = "Monday"
  )
  
  it ("works for hourly time step", {
    res <- .timeIdToDate(1:(365*24), "hourly", opts)
    expect_is(res, "POSIXt")
    expect_equal(as.Date(res[1]), as.Date("2017-01-01"))
    expect_equal(hour(res[1]), 0)
    expect_equal(minute(res[1]), 0)
    expect_equal(as.Date(res[365*24]), as.Date("2017-12-31"))
    expect_equal(hour(res[365*24]), 23)
    expect_equal(minute(res[365*24]), 0)
  })
  
  it("works for daily time step", {
    res <- .timeIdToDate(1:365, "daily", opts)
    expect_is(res, "Date")
    expect_equal(res[1], as.Date("2017-01-01"))
    expect_equal(res[365], as.Date("2017-12-31"))
  })
  
  it("works for weekly time step", {
    res <- .timeIdToDate(1:53, "weekly", opts)
    expect_is(res, "Date")
    expect_equal(res[1], as.Date("2016-12-26"))
    expect_equal(res[2], as.Date("2017-01-02"))
    expect_equal(res[53], as.Date("2017-12-25"))
    
    opts <- list(
      start = as.POSIXlt("2017-01-01", tz = "UTC"), 
      firstWeekday = "Tuesday"
    )
    res <- .timeIdToDate(1:53, "weekly", opts)
    expect_is(res, "Date")
    expect_equal(res[1], as.Date("2016-12-27"))
    expect_equal(res[2], as.Date("2017-01-03"))
    expect_equal(res[53], as.Date("2017-12-26"))
  })
  
  it("works for monthly time step", {
    res <- .timeIdToDate(1:12, "monthly", opts)
    expect_is(res, "Date")
    expect_equal(res[1], as.Date("2017-01-01"))
    expect_equal(res[12], as.Date("2017-12-01"))
  })
  
  it("works for annual", {
    res <- .timeIdToDate(1, "annual", opts)
    expect_is(res, "Date")
    expect_equal(res[1], as.Date("2017-01-01"))
  })
})
