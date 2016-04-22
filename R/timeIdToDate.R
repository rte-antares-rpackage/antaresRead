#' format timeId in a readable format
#' 
#' This function converts a \code{timeId} variable in a readable format according
#' to time step. 
#' 
#' @return 
#' A vector with same length than \code{timeId}. The vector class is determined
#' by timeStep:
#' For hourly time step, the function returns a timestamp 
#' (\code{POSIXct}), for daily time step a date and for weekly and monthly 
#' timestep a character string containing the year and the number of the week or 
#' the month.
#' 
#' @noRd
#'   
.timeIdToDate <- function(timeId, timeStep=c("hourly", "daily", "weekly", "monthly", "annual"), 
                          opts=simOptions()) {
  timeStep <- match.arg(timeStep)
  
  if (timeStep == "hourly") {
    
    timestamp <- as.POSIXct(opts$start)
    lubridate::hour(timestamp) <- lubridate::hour(timestamp) + 1:(24*7*52) - 1
    
    return(timestamp[timeId])
    
  } else if (timeStep == "daily") {
    
    date <- as.Date(opts$start)
    date <- date + 1:(7*52) - 1
    
    return(date[timeId])
    
  } else if (timeStep == "weekly") {
    
    timestamp <- as.POSIXct(opts$start)
    lubridate::hour(timestamp) <- lubridate::hour(timestamp) + 1:(24*7*52) - 1
    
    weekId <- .getTimeId(1:(24*7*52), "weekly", opts)
    date <- as.Date(tapply(timestamp, weekId, function(x) as.Date(min(x))), origin = "1970-1-1")
    date[1] <- date[2] - 7
    
    week <- format(date, format = "%G-w%V")
    
    return(week[timeId])
    
  } else if (timeStep == "monthly") {
    
    timestamp <- as.POSIXct(opts$start)
    lubridate::hour(timestamp) <- lubridate::hour(timestamp) + 1:(24*7*52) - 1
    
    monthId <- .getTimeId(1:(24*7*52), "monthly", opts)
    month <- tapply(timestamp, monthId, function(x) format(min(x), format = "%Y-%m"))
    
    return(month[timeId])

  } else {
    
    year <- lubridate::year(as.POSIXct(opts$start))
    return(rep(year, length(timeId)))
    
  }
}
