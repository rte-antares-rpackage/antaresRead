#Copyright © 2016 RTE Réseau de transport d’électricité

addDateTimeColumns <- function(x) {
  if (!is(x, "antaresData")) stop("x has to be an 'antaresData' object created with readAntares or readInputTS.")
  
  timeStep <- attr(x, "timeStep")
  opts <- attr(x, "opts")
  synthesis <- attr(x, "synthesis")
  
  if (is(x, "antaresDataList")) {
    for (el in x) addDateTimeColumns(el)
    return(invisible(x))
  }
  
  # If table is empty, return it as is
  if (nrow(x) == 0) {
    return(x)
  }
  
  if (timeStep == "hourly") {
    
    timestamp <- as.POSIXct(opts$start)
    lubridate::hour(timestamp) <- lubridate::hour(timestamp) + 1:(24*365) - 1
    
    newCols <- data.table(time = timestamp,
                          day = lubridate::day(timestamp),
                          month = as.factor(toupper(lubridate::month(timestamp, TRUE, TRUE, locale = "C"))),
                          hour = as.factor(format(timestamp, format = "%H:%M")))
    
  } else if (timeStep == "daily") {
    
    date <- as.Date(opts$start)
    date <- date + 1:365 - 1
    
    newCols <- data.table(time = date,
                          day = lubridate::day(date),
                          month = as.factor(toupper(lubridate::month(date, TRUE, TRUE, locale = "C"))))
    
  } else if (timeStep == "weekly") {
    
    timestamp <- as.POSIXct(opts$start)
    lubridate::hour(timestamp) <- lubridate::hour(timestamp) + 1:(24*365) - 1
    
    weekId <- .getTimeId(1:(24*365), "weekly", opts)
    date <- as.Date(tapply(timestamp, weekId, function(x) as.Date(min(x))), origin = "1970-1-1")
    date[1] <- date[2] - 7
    
    week <- format(date, format = "%G-w%V")
    
    newCols <- data.table(time = as.factor(week))
    
  } else if (timeStep == "monthly") {
    
    timestamp <- as.POSIXct(opts$start)
    lubridate::hour(timestamp) <- lubridate::hour(timestamp) + 1:(24*365) - 1
    
    monthId <- .getTimeId(1:(24*365), "monthly", opts)
    month <- tapply(timestamp, monthId, function(x) format(min(x), format = "%Y-%m"))
    
    monthName <- tapply(timestamp, monthId, function(x) toupper(lubridate::month(min(x), TRUE, TRUE, locale = "C")))
    
    newCols <- data.table(time = month, 
                          month = as.factor(toupper(monthName)))
    
  } else {
    
    year <- lubridate::year(as.POSIXct(opts$start))
    
    newCols <- data.table(time = year)
    
  }
  
  colsToAdd <- setdiff(names(newCols), names(x))
  if (timeStep != "annual") {
    x[, c(colsToAdd) := newCols[x$timeId, colsToAdd, with = FALSE]]
  } else {
    x[, c(colsToAdd) := newCols[, colsToAdd, with = FALSE]]
  }
  
  .reorderCols(x)
}
