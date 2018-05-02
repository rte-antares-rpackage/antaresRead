#Copyright © 2016 RTE Réseau de transport d’électricité

#' Change the timestep of an output
#' 
#' This function changes the timestep of a table or an \code{antaresData} object
#' and performs the required aggregation or desaggregation. We can specify
#' (des)aggregate functions by columns, see the param `fun`.
#' 
#' @param x
#'   data.table with a column "timeId" or an object of class "antaresDataList"
#' @param newTimeStep
#'   Desired time step.The possible values are hourly, daily, weekly, 
#'   monthly and annual.
#' @param oldTimeStep
#'   Current time step of the data. This argument is optional for an object of
#'   class \code{antaresData} because the time step of the data is stored inside
#'   the object
#' @param fun
#'   Character vector with one element per column to (des)aggregate indicating 
#'   the function to use ("sum", "mean", "min" or "max") for this column. It can
#'   be a single element, in that case the same function is applied to every 
#'   columns.
#' @inheritParams readAntares
#'   
#' @return 
#' Either a data.table or an object of class "antaresDataList" depending on the 
#' class of \code{x}
#' 
#' @examples 
#' \dontrun{
#' setSimulationPath()
#' 
#' areasH <- readAntares(select = "LOAD", synthesis = FALSE, mcYears = 1)
#' areasD <- readAntares(select = "LOAD", synthesis = FALSE, mcYears = 1, timeStep ="daily")
#' 
#' areasDAgg <- changeTimeStep(areasH, "daily", "hourly")
#' 
#' all.equal(areasDAgg$LOAD, areasD$LOAD)
#' 
#' # Use different aggregation functions
#' mydata <- readAntares(select = c("LOAD", "MRG. PRICE"), timeStep = "monthly")
#' changeTimeStep(mydata, "annual", fun = c("sum", "mean"))
#' }
#' 
#' @export
#' 
changeTimeStep <- function(x, newTimeStep, oldTimeStep, fun = "sum", opts=simOptions()) {
  fun <- match.arg(fun, c("sum", "mean", "min", "max"), several.ok = TRUE)
  
  # Agregation function
  funs <- list(sum = function(x) sum(as.numeric(x)), mean = mean, min = min, max = max)
  #desagregation function
  ifuns <- list(sum = function(x, n) {x / n},
                mean = function(x, n) {x},
                min = function(x, n) {x},
                max = function(x, n) {x})
  
  if (is(x, "antaresData")) {
    opts <- simOptions(x)
    oldTimeStep <- attr(x, "timeStep")
  }
  
  if (newTimeStep == oldTimeStep) return(x)
  
  .checkIfWeWantToAggregate(newTimeStep, oldTimeStep)
  
  if (is(x, "antaresDataList")) {
    for (i in 1:length(x)) {
      x[[i]] <- changeTimeStep(x[[i]], newTimeStep, oldTimeStep, fun, opts)
    }
    
    attr(x, "timeStep") <- newTimeStep
    
    return(x)
  }
  
  # Keep a copy of attributes to put them back at the end of the function
  synthesis <- attr(x, "synthesis")
  type <- attr(x, "type")
  
  # Should we had date-time columns ?
  addDateTimeCol <- !is.null(x[["time"]])
  
  # Suppress time variables
  if (!is.null(x[["time"]])) x$time <- NULL
  if (!is.null(x$hour)) x$hour <- NULL
  if (!is.null(x$day)) x$day <- NULL
  if (!is.null(x$week)) x$week <- NULL
  if (!is.null(x$month)) x$month <- NULL
  
  # Strategy: if oldTimeStep is not hourly, first desagregate data at hourly
  # level. Then, in all cases aggregate hourly data at the desired level.
  refTime <- data.table(
    oldTimeId = .getTimeId(opts$timeIdMin:opts$timeIdMax, oldTimeStep, opts),
    timeId = .getTimeId(opts$timeIdMin:opts$timeIdMax, newTimeStep, opts)
  )
  
  x <- copy(x)
  setnames(x, "timeId", "oldTimeId")
  x <- merge(x, refTime, by = "oldTimeId", allow.cartesian=TRUE)
  
  # Desagregation
  if (oldTimeStep != "hourly") {
    idVars <- c(.idCols(x), "oldTimeId")
    idVars  <- idVars[idVars != "timeId"]
    by <- parse(text = sprintf("list(%s)", paste(idVars, collapse = ", ")))
    
    # Code below is a bit hacky: we want to use a window function on all variables but one (timeId).
    # Instead of writing a loop on the columns, we separate the timeId column
    # from the table, use the syntax of data.table to perform the window function
    # and finally put back the timeId in the table.
    setorderv(x, idVars)
    timeId <- x$timeId
    x$timeId <- NULL
    
    if (length(fun) == 1) fun <- rep(fun, ncol(x) - length(idVars))
    
    x <- x[, mapply(function(x, f) {f(x, .N)}, x = .SD, f = ifuns[fun], SIMPLIFY=FALSE), by = eval(by)]
    
    x$timeId <- timeId
    
    .reorderCols(x)
  }
  
  x$oldTimeId <- NULL
  
  # Aggregation
  if (newTimeStep != "hourly") {
    
    idVars <- .idCols(x)
    
    if (length(fun) == 1) fun <- rep(fun, ncol(x) - length(idVars))
    
    x <- x[, mapply(function(x, f) {f(x)}, x = .SD, f = funs[fun], SIMPLIFY=FALSE), keyby=idVars]
  }
  
  x <- .addClassAndAttributes(x, synthesis, newTimeStep, opts, type = type)
  
  if(addDateTimeCol) addDateTimeColumns(x)
  
  x
}

.getTimeId <- function(hourId, timeStep, opts) {
  # Easy cases
  if (timeStep == "hourly") return(hourId)
  
  if (timeStep == "daily") {
    return( (hourId - 1) %/% 24 + 1 )
  }
  
  if (timeStep == "annual") {
    return(rep(1L, length(hourId)))
  }
  
  # Hard cases
  # Create a correlation table between hourIds and actual dates and compute new 
  # timeIds based on the actual dates
  timeRef <- data.table(hourId = 1:(24*365))
  
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

.checkIfWeWantToAggregate<-function(newTimeStep, oldTimeStep){
  
  yesWeWantToAggregate<-FALSE
  if(oldTimeStep=="hourly" & oldTimeStep!=newTimeStep){
    yesWeWantToAggregate<-TRUE
  } else if (oldTimeStep=="daily" & (newTimeStep=="weekly" | newTimeStep=="monthly" | newTimeStep=="annual" )){
    yesWeWantToAggregate<-TRUE
  } else if (oldTimeStep=="weekly" & (newTimeStep=="monthly" | newTimeStep=="annual")){
    yesWeWantToAggregate<-TRUE
  } else if (oldTimeStep=="monthly" & (newTimeStep=="annual")){
    yesWeWantToAggregate<-TRUE
  } else{
    yesWeWantToAggregate<-FALSE
  }
  
  if(yesWeWantToAggregate){
    warning('Aggregation will be perform approximatively because optimization variables in ANTARES are doubles but ANTARES write only integers in TXT files, with this transformation we lose precision. If you want accurate data then you must import the corresponding data with `readAntares`')
  }

}
