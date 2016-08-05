addDateTimeColumns <- function(x) {
  if (!is(x, "antaresData")) stop("x has to be an 'antaresData' object created with readAntares or readInputTS.")
  
  timeStep <- attr(x, "timeStep")
  opts <- attr(x, "opts")
  synthesis <- attr(x, "synthesis")
  
  if (is(x, "antaresDataTable")) {
    type <- attr(x, "type")
    x <- list(x)
    names(x) <- type
  }
  
  if (timeStep == "hourly") {
    
    timestamp <- as.POSIXct(opts$start)
    lubridate::hour(timestamp) <- lubridate::hour(timestamp) + 1:(24*7*52) - 1
    
    newCols <- data.table(time = timestamp,
                          day = lubridate::day(timestamp),
                          month = as.factor(toupper(lubridate::month(timestamp, TRUE, TRUE))),
                          hour = as.factor(format(timestamp, format = "%H:%M")))
    
  } else if (timeStep == "daily") {
    
    date <- as.Date(opts$start)
    date <- date + 1:(7*52) - 1
    
    newCols <- data.table(time = date,
                          day = lubridate::day(date),
                          month = as.factor(toupper(lubridate::month(date, TRUE, TRUE))))
    
  } else if (timeStep == "weekly") {
    
    timestamp <- as.POSIXct(opts$start)
    lubridate::hour(timestamp) <- lubridate::hour(timestamp) + 1:(24*7*52) - 1
    
    weekId <- .getTimeId(1:(24*7*52), "weekly", opts)
    date <- as.Date(tapply(timestamp, weekId, function(x) as.Date(min(x))), origin = "1970-1-1")
    date[1] <- date[2] - 7
    
    week <- format(date, format = "%G-w%V")
    
    newCols <- data.table(time = as.factor(week))
    
  } else if (timeStep == "monthly") {
    
    timestamp <- as.POSIXct(opts$start)
    lubridate::hour(timestamp) <- lubridate::hour(timestamp) + 1:(24*7*52) - 1
    
    monthId <- .getTimeId(1:(24*7*52), "monthly", opts)
    month <- tapply(timestamp, monthId, function(x) format(min(x), format = "%Y-%m"))
    
    monthName <- tapply(timestamp, monthId, function(x) toupper(lubridate::month(min(x), TRUE, TRUE)))
    
    newCols <- data.table(time = month, 
                          month = as.factor(toupper(monthName)))
    
  } else {
    
    year <- lubridate::year(as.POSIXct(opts$start))
    
    newCols <- data.table(time = year)
    
  }
  
  for (e in names(x)) {
    tmp <- copy(newCols)
    for (n in names(newCols)[-1]) if (n %in% names(x[[e]])) tmp[, c(n) := NULL]
    if (timeStep != "annual") x[[e]] <- cbind(x[[e]], tmp[x[[e]]$timeId])
    else x[[e]] <- cbind(x[[e]], tmp)
  }
  
  .addClassAndAttributes(x, synthesis = synthesis, timeStep = timeStep, 
                         opts = opts, simplify = TRUE)
}
