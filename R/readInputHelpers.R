#' .importMisc
#'
#' Private function used to read misc input files.
#'
#' @return
#' a data.table
#' @noRd
#'
.importMisc <- function(nodes, timeStep, opts) {
  if (is.null(nodes)) return(NULL)
  
  res <- llply(nodes, function(n) {
    path <- file.path(opts$path, "../../input/misc-gen",
                      sprintf("miscgen-%s.txt", n))
    
    if(file.size(path) == 0) misc <- data.table(matrix(0L, 24*7*52,length(pkgEnv$miscNames)))
    else misc <- fread(path, sep="\t", header = FALSE, integer64 = "numeric")
    
    setnames(misc, names(misc), pkgEnv$miscNames)
    
    misc$node <- n
    misc$timeId <- 1:nrow(misc)
    
    misc[1:(24 * 7 * 52)]
  })
  
  res <- rbindlist(res)
  setcolorder(res, c("node", "timeId", pkgEnv$miscNames))
  
  .aggregateByTimeStep(res, timeStep, opts)
}


.importThermal <- function(nodes, timeStep, opts) {
  if (is.null(nodes)) return(NULL)
  
  pathTSNumbers <- file.path(opts$path, "ts-numbers/thermal")
  if (! file.exists(pathTSNumbers)) {
    message("Monte-Carlo scenarii not available for thermal capacity.")
    return(NULL)
  }
  
  # Keep only nodes with clusters to avoid difficulties in the rest of the function
  nodes <- nodes[nodes %in% opts$nodesWithClusters]
  
  # Read the Ids of the time series used in each Monte-Carlo Scenario.
  tsIds <- llply(nodes, function(n) {
    cls <- list.files(file.path(pathTSNumbers, n))
    nameCls <- gsub(".txt", "", cls)
    
    
    if (length(cls) == 0) return(NULL)
    
    res <- llply(cls, function(cl) {
      as.numeric(readLines(file.path(pathTSNumbers, n, cl))[-1])
    })
    
    names(res) <- nameCls
    
    res
  })
  
  names(tsIds) <- nodes
  
  # Read the input time series. 
  pathInput <- file.path(opts$path, "../../input/thermal/series")
  
  # Three nested loops: nodes, clusters, Monte Carlo simulations.
  series <- ldply(nodes, function(n) {
    cls <- list.files(file.path(pathInput, n))
    
    if (length(cls) == 0) return(NULL)
    
    ldply(cls, function(cl) {
      ts <- fread(file.path(pathInput, n, cl, "series.txt"))[1:(24*7*52),]
      ids <- tsIds[[n]][[cl]]
      
      ldply(1:length(ids), function(i) {
        data.frame(
          node = n, 
          cluster = cl, 
          mcYear = i,
          timeId = 1:nrow(ts),
          capacity = ts[[ ids[i] ]]
        )
      })
    })
    
  })
  
  series <- data.table(series)
  
  .aggregateByTimeStep(series, timeStep, opts)
  
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
  
  # Identify id variables used for aggregation
  idVars <- intersect(names(x), pkgEnv$idVars)
  by <- parse(text = sprintf("list(%s)", paste(setdiff(idVars, "timeId"), collapse = ", ")))
  byt <- parse(text = sprintf("list(%s)", paste(idVars, collapse = ", ")))
  
  if (timeStep == "daily") {
    
    x$timeId <- (x$timeId - 1) %/% 24 + 1
    
  } else if (timeStep == "weekly") {
    
    tmp <- opts$start
    hour(tmp) <- hour(tmp) + 1:(24*7*52) - 1
    x$wday <- wday(tmp)
    
    startWeek <- which(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday") == opts$firstWeekday)
    
    x[, change := wday == startWeek & wday != shift(wday), by = eval(by)]
    x[is.na(change), change := TRUE]
    x[, timeId := cumsum(change), by = eval(by)]
    x$wday <- x$change <- NULL
    
  } else if (timeStep == "monthly") {
    
    tmp <- opts$start
    hour(tmp) <- hour(tmp) + 1:(24*7*52) - 1
    x$month <- month(tmp)
    
    x[, change :=  month != shift(month), by = eval(by)]
    x[is.na(change), change := TRUE]
    x[, timeId := cumsum(change), by = eval(by)]
    x$month <- x$change <- NULL
    
  } else if (timeStep == "annual") {
    
    x$timeId <- rep("annual", nrow(x))
    
  }
  
  x[, lapply(.SD, sum), keyby=eval(byt)]
  
}
