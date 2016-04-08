#' Change the timestep of an output
#' 
#' This function changes the timestep of a table or an readAntares object
#' and performs the required aggregation or desaggregation.
#' 
#' @param x
#'   data.table with a column "timeId" or an object of class "readAntares"
#' @param newTimeStep
#'   Desired time step.The possible values are hourly, daily, weekly, monthly and annual.
#' @param oldTimeStep
#'   Current time step of the data. If the data has not been manipulated, this 
#'   argument is optional since the time step is stored in an attribute of the
#'   data.
#' @param fun
#'   function to use for aggregation. Either "sum" or "mean"
#' @param opts
#'   list of simulation parameters returned by the function 
#'   \code{\link{setSimulationPath}}
#'   
#' @return 
#' Either a data.table or an object of class "readAntares" depending on the 
#' class of \code{x}
#' 
#' @examples 
#' \dontrun{
#' nodesH <- readAntares(select = "LOAD", synthesis = FALSE, mcYears = 1)
#' nodesD <- readAntares(select = "LOAD", synthesis = FALSE, mcYears = 1, timeStep ="daily")
#' 
#' nodesDAgg <- changeTimeStep(nodesH, "daily")
#' 
#' all.equal(nodesDAgg$LOAD, nodesD$LOAD)
#' }
#' 
#' @export
#' 
changeTimeStep <- function(x, newTimeStep, oldTimeStep, fun = c("sum", "mean"), opts=getOption("antares")) {
  fun <- match.arg(fun)
  
  if (!is.null(attr(x, "timeStep"))) oldTimeStep <- attr(x, "timeStep")
  
  if (newTimeStep == oldTimeStep) return(x)
  
  if (is(x, "antaresOutput")) {
    for (i in 1:length(x)) {
      x[[i]] <- changeTimeStep(x[[i]], newTimeStep, oldTimeStep, fun, opts)
    }
    
    attr(x, "timeStep") <- newTimeStep
    
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
    
    # Code below is a bit hacky: we want to use a window function on all variables but one (timeId).
    # Instead of writing a loop on the columns, we separate the timeId column
    # from the table, use the syntax of data.table to perform the window function
    # and finally put back the timeId in the table.
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
