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
  
  res <- changeTimeStep(series, timeStep, "hourly", opts=opts)
  
  if (synthesis) {
    res <- res[, .(capacity=mean(capacity)), keyby = .(node, cluster, timeId)]
  }
  
  res
}

.importHydroStorage <- function(node, synthesis, timeStep, mcYears, opts, ...) {
  
  pathTSNumbers <- file.path(opts$path, "ts-numbers/hydro")
  if (! file.exists(pathTSNumbers)) {
    message("Monte-Carlo scenarii not available for hydro capacity.")
    return(NULL)
  }
  
  # Read the Ids of the time series used in each Monte-Carlo Scenario.
  tsIds <- as.numeric(readLines(file.path(pathTSNumbers, paste0(node, ".txt")))[-1])
  
  # Input time series
  pathInput <- file.path(opts$path, "../../input/hydro/series")
  f <- file.path(pathInput, node, "mod.txt")
  
  if (file.size(f) == 0) return(NULL)
  
  ts <- fread(f, integer64 = "numeric")
  
  N <- nrow(ts)
  
  series <- ldply(1:length(tsIds), function(i) {
    data.frame(
      node = node, 
      mcYear = i,
      timeId = 1:nrow(ts),
      hydroStorage = ts[[ tsIds[i] ]]
    )
  })
  
  series <- data.table(series)
  
  
  res <- changeTimeStep(series, timeStep, "monthly", opts=opts)
  
  if (synthesis) {
    res <- res[, .(hydroStorage=mean(hydroStorage)), keyby = .(node, timeId)]
  }
  
  res

}

#' .importSimpleInputTS
#' 
#' This private function is used to read input time series that do not vary 
#' between Monte-Carlo scenarios: misc production, reserve, hydro storage
#' max power.
#' 
#' @noRd
#' 
.importSimpleInputTS <- function(node, timeStep, opts, path, fileNamePattern, 
                                 colnames, inputTimeStep, fun = "sum", ...) {
  
  path <- file.path(opts$path, path, sprintf(fileNamePattern, node))
  
  expectedRows <- switch(inputTimeStep, hourly=24*7*52, daily=7*52, monthly=12)
  
  if (file.size(path) == 0) {
    inputTS <- data.table(matrix(0L, expectedRows,length(colnames)))
    setnames(inputTS, names(inputTS), colnames)
  } else {
    inputTS <- fread(path, integer64 = "numeric", header = FALSE, col.names = colnames)
    inputTS <- inputTS[1:expectedRows]
  }
  
  inputTS$node <- node
  inputTS$timeId <- 1:nrow(inputTS)
  
  setcolorder(inputTS, c("node", "timeId", colnames))
  
  changeTimeStep(inputTS, timeStep, inputTimeStep, fun = fun)
  
}

.importHydroStorageMaxPower <- function(node, timeStep, opts, ...) {
  
  .importSimpleInputTS(node, timeStep, opts, "../../input/hydro/common/capacity",
                       "maxpower_%s.txt", colnames=c("low", "avg", "high"),
                       inputTimeStep = "daily")
  
}

.importMisc <- function(node, timeStep, opts, ...) {
  
  .importSimpleInputTS(node, timeStep, opts, "../../input/misc-gen",
                       "miscgen-%s.txt", colnames=pkgEnv$miscNames,
                       inputTimeStep = "hourly")
  
}

.importReserves <- function(node, timeStep, opts, ...) {
  
  .importSimpleInputTS(node, timeStep, opts, "../../input/reserves",
                       "%s.txt", colnames=c("primaryRes", "strategicRes", "DSM", "dayAhead"),
                       inputTimeStep = "hourly")
  
}

.importLinkCapacity <- function(link, timeStep, opts, ...) {
  
  nodes <- strsplit(link, " - ")[[1]]
  
  colnames <- c("transCapacityDirect", "transCapacityIndirect",
                "impedances", "hurdlesCostDirect", "hurdlesCostIndirect")
  
  # A bit hacky, but it works !
  res <- .importSimpleInputTS(nodes[2], timeStep, opts, 
                              file.path("../../input/links", nodes[1]), "%s.txt", 
                              colnames=colnames,
                              inputTimeStep = "hourly")
  
  res$node <- NULL
  res$link <- link
  
  setcolorder(res, c("link", "timeId", colnames))
  
}
