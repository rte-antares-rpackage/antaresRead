#' .importMisc
#'
#' Private function used to read misc input files.
#'
#' @return
#' a data.table
#' @noRd
#'
#'
.importMisc <- function(node, timeStep, opts, ...) {
  path <- file.path(opts$path, "../../input/misc-gen",
                    sprintf("miscgen-%s.txt", node))
    
  if(file.size(path) == 0) misc <- data.table(matrix(0L, 24*7*52,length(pkgEnv$miscNames)))
  else misc <- fread(path, sep="\t", header = FALSE, integer64 = "numeric")
    
  setnames(misc, names(misc), pkgEnv$miscNames)
    
  misc$node <- node
  misc$timeId <- 1:nrow(misc)
    
  misc <- misc[1:(24 * 7 * 52)]
  
  setcolorder(misc, c("node", "timeId", pkgEnv$miscNames))
  
  .aggregateByTimeStep(misc, timeStep, opts)
}


.importThermal <- function(node, synthesis, timeStep, mcYears, opts, ...) {
  if (!node %in% opts$nodesWithClusters) return(NULL)
  
  pathTSNumbers <- file.path(opts$path, "ts-numbers/thermal")
  if (! file.exists(pathTSNumbers)) {
    message("Monte-Carlo scenarii not available for thermal capacity.")
    return(NULL)
  }
  
  # Read the Ids of the time series used in each Monte-Carlo Scenario.
  cls <- list.files(file.path(pathTSNumbers, node))
  if (length(cls) == 0) return(NULL)
  
  nameCls <- gsub(".txt", "", cls)
    
  tsIds <- llply(cls, function(cl) {
    as.numeric(readLines(file.path(pathTSNumbers, node, cl))[-1])
  })
  
  names(tsIds) <- nameCls
  
  # Read the input time series. 
  pathInput <- file.path(opts$path, "../../input/thermal/series")
  
  # Two nested loops: clusters, Monte Carlo simulations.
  cls <- list.files(file.path(pathInput, node))
  if (length(cls) == 0) return(NULL)
    
  series <- ldply(cls, function(cl) {
    ts <- fread(file.path(pathInput, node, cl, "series.txt"), integer64 = "numeric")[1:(24*7*52),]
    ids <- tsIds[[cl]]
    
    ldply(1:length(ids), function(i) {
      data.frame(
        node = node, 
        cluster = cl, 
        mcYear = i,
        timeId = 1:nrow(ts),
        capacity = ts[[ ids[i] ]]
      )
    })
  })
  
  series <- data.table(series)
  
  res <- .aggregateByTimeStep(series, timeStep, opts)
  
  if (synthesis) {
    res <- res[, .(capacity=mean(capacity)), keyby = .(node, cluster, timeId)]
  }
  
  res
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
    lubridate::hour(tmp) <- lubridate::hour(tmp) + 1:(24*7*52) - 1
    x$wday <- lubridate::wday(tmp)
    
    startWeek <- which(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday") == opts$firstWeekday)
    
    x[, change := wday == startWeek & wday != shift(wday), by = eval(by)]
    x[is.na(change), change := TRUE]
    x[, timeId := cumsum(change), by = eval(by)]
    x$wday <- x$change <- NULL
    
  } else if (timeStep == "monthly") {
    
    tmp <- opts$start
    lubridate::hour(tmp) <- lubridate::hour(tmp) + 1:(24*7*52) - 1
    x$month <- lubridate::month(tmp)
    
    x[, change :=  month != shift(month), by = eval(by)]
    x[is.na(change), change := TRUE]
    x[, timeId := cumsum(change), by = eval(by)]
    x$month <- x$change <- NULL
    
  } else if (timeStep == "annual") {
    
    x$timeId <- rep("annual", nrow(x))
    
  }
  
  x[, lapply(.SD, sum), keyby=eval(byt)]
  
}
