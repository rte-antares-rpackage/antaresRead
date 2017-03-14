#Copyright © 2016 RTE Réseau de transport d’électricité

#' format timeId in a readable format
#' 
#' This function converts a \code{timeId} variable in a readable format according
#' to time step. 
#' 
#' @return 
#' A vector with same length than \code{timeId}. The vector class is determined
#' by timeStep:
#' For hourly time step, the function returns a timestamp 
#' (\code{POSIXct}), for daily time step a date and for weekly and monthly time step
#' the first day of the month or the week in \code{Date} format
#' 
#' @noRd
#'   
.timeIdToDate <- function(timeId, timeStep=c("hourly", "daily", "weekly", "monthly", "annual"), 
                          opts=simOptions()) {
  timeStep <- match.arg(timeStep)
  
  if (timeStep == "hourly") {
    
    timestamp <- as.POSIXct(opts$start)
    lubridate::hour(timestamp) <- lubridate::hour(timestamp) + 1:(24*365) - 1
    
    return(timestamp[timeId])
    
  } else if (timeStep == "daily") {
    
    date <- as.Date(opts$start)
    date <- date + 1:365 - 1
    
    return(date[timeId])
    
  } else if (timeStep == "weekly") {
    
    timestamp <- as.POSIXct(opts$start)
    lubridate::hour(timestamp) <- lubridate::hour(timestamp) + 1:(24*365) - 1
    
    weekId <- .getTimeId(1:(24*365), "weekly", opts)
    date <- as.Date(tapply(timestamp, weekId, function(x) as.Date(min(x))), origin = "1970-1-1")
    date[1] <- date[2] - 7
    
    week <- date #format(date, format = "%G-w%V")
    
    return(unname(week[timeId]))
    
  } else if (timeStep == "monthly") {
    
    timestamp <- as.POSIXct(opts$start)
    lubridate::hour(timestamp) <- lubridate::hour(timestamp) + 1:(24*365) - 1
    
    monthId <- .getTimeId(1:(24*365), "monthly", opts)
    month <- as.Date(tapply(timestamp, monthId, function(x) as.Date(min(x))), origin = "1970-1-1")
    # month <- tapply(timestamp, monthId, function(x) format(min(x), format = "%Y-%m"))
    
    return(unname(month[timeId]))

  } else {
    
    date <- as.Date(opts$start)
    return(rep(date, length(timeId)))
    
  }
}
