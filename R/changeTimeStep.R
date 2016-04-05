#' Change the timestep of an output
#' 
#' This function changes the timestep of a table or an antaresOutput object
#' and performs the required aggregation
#' 
#' @param x
#' data.table with a column "timeId" or an object of class "antaresOutput"
#' @param newTimeStep
#' Desired time step.
#' @param oldTimeStep
#' Current time step of the data. If the data has not been manipulated, this 
#' argument is optional since the time step is stored in an attribute of the
#' data.
#' @param fun
#' function to use for aggregation. Either "sum" or "mean"
#' 
#' @export
#' 
changeTimeStep <- function(x, newTimeStep, oldTimeStep, fun = c("sum", "mean"), opts=getOption("antares")) {
  fun <- match.arg(fun)
  
  if (!is.null(attr(x, "timeStep"))) oldTimeStep <- attr(x, "timeStep")
  
  if (newTimeStep == oldTimeStep) return(x)
  
  if (is(x, "antaresOutput")) {
    for (i in 1:length(x)) {
      x[[i]] <- changeTimeStep(x[[i]], newTimeStep, oldTimeStep, fun)
    }
    return(x)
  }
  
  # Keep a copy of attributes to put them back at the end of the function
  synthesis <- attr(x, "synthesis")
  type <- attr(x, "type")
  
  # Suppress time variables
  if (!is.null(x$hour)) x$hour <- NULL
  if (!is.null(x$day)) x$day <- NULL
  if (!is.null(x$month)) x$month <- NULL
  
  # Strategy: if newTimeStep is not hourly, first desagregate data at hourly
  # level. Then, in all cases aggregate hourly data at the desired level.
  refTime <- data.table(
    oldTimeId = .getTimeId(1:(24*7*52), oldTimeStep, opts),
    timeId = .getTimeId(1:(24*7*52), newTimeStep, opts)
  )
  
  x <- copy(x)
  setnames(x, "timeId", "oldTimeId")
  x <- merge(x, refTime, by = "oldTimeId", allow.cartesian=TRUE)
  
  # Desagregation
  if (oldTimeStep != "hourly" & fun == "sum") {
    idVars <- intersect(names(x), c(pkgEnv$idVars, "oldTimeId"))
    idVars  <- idVars[idVars != "timeId"]
    by <- parse(text = sprintf("list(%s)", paste(idVars, collapse = ", ")))
    
    setorderv(x, idVars)
    timeId <- x$timeId
    x$timeId <- NULL
    
    x <- x[, lapply(.SD, function(x) x / .N), by = eval(by)]
    x$timeId <- timeId
  }
  
  x$oldTimeId <- NULL
  
  # Aggregation
  if (newTimeStep != "hourly") {
    idVars <- intersect(names(x), pkgEnv$idVars)
    by <- parse(text = sprintf("list(%s)", paste(idVars, collapse = ", ")))
    
    if (fun == "sum") x <- x[, lapply(.SD, sum), keyby=eval(by)]
    else x <- x[, lapply(.SD, mean), keyby=eval(by)]
  }
  
  class(x) <- append("antaresTable", class(x))
  attr(x, "type") <- type
  attr(x, "timeStep") <- newTimeStep
  attr(x, "synthesis") <- synthesis
  
  x
}

.getTimeId <- function(hourId, timeStep, opts) {
  # Easy cases
  if (timeStep == "hourly") return(hourId)
  
  if (timeStep == "daily") {
    return( (hourId - 1) %/% 24 + 1 )
  }
  
  if (timeStep == "annual") {
    return(as.factor(rep("annual", length(hourId))))
  }
  
  # Hard cases
  # Create a correlation table between hourIds and actual dates and compute new 
  # timeIds based on the actual dates
  timeRef <- data.table(hourId = 1:(24*7*52))
  
  tmp <- as.POSIXct(opts$start)
  lubridate::hour(tmp) <- lubridate::hour(tmp) + timeRef$hourId - 1
  timeRef$hour <- tmp
  
  if (timeStep == "weekly") {
    timeRef$wday <- lubridate::wday(timeRef$hour)
    startWeek <- which(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday") == opts$firstWeekday)
    timeRef[, change := wday == startWeek & wday != shift(wday)]
    timeRef$change[1] <- TRUE
    timeRef[, timeId := cumsum(change)]
    
    return(timeRef$timeId[hourId])
  }
  
  if (timeStep == "monthly") {
    timeRef$month <- lubridate::month(timeRef$hour)
    timeRef[, change :=  month != shift(month)]
    timeRef$change[1] <- TRUE
    timeRef[, timeId := cumsum(change)]
    
    return(timeRef$timeId[hourId])
  }
  
}

#' Aggregates input time series at desired time resolution.
#'
#' Input time series are only stored at hourly resolution. This function aims
#' to aggregate the series at the desired resolution.
#'
#' Weekly and monthly aggregations are a bit complicated. The strategy is to
#' find the rows that corresponds to a change of week or month, create a
#' variable equal to 1 if week/month has changed and 0 otherwise. The cumulated
#' sum of this variable corresponds to the timeId :
#'
#' day change cumsum(change) timeId
#'   n      1              1      1
#'   n      0              1      1
#'   n      0              1      1
#' ...    ...            ...    ...
#' n+6      0              1      1
#' n+7      1              2      2
#' n+7      0              2      2
#'
#' @noRd
#' 
.aggregateByTimeStep <- function(x, timeStep = c("hourly", "daily", "weekly", "monthly", "annual"), opts) {
  if (timeStep == "hourly") return(x)
  
  x$timeId <- .getTimeId(x$timeId, timeStep, opts)
  
  # Identify id variables used for aggregation
  idVars <- intersect(names(x), pkgEnv$idVars)
  byt <- parse(text = sprintf("list(%s)", paste(idVars, collapse = ", ")))
  
  x[, lapply(.SD, sum), keyby=eval(byt)]
  
}
