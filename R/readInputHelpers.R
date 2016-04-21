.importThermal <- function(node, synthesis, timeStep, mcYears, opts, ...) {
  if (!node %in% opts$nodesWithClusters) return(NULL)
  if (synthesis) mcYears <- 1:opts$mcYears
  
  # Path to the files containing the IDs of the time series used for each
  # Monte-Carlo years.
  pathTSNumbers <- file.path(opts$path, "ts-numbers/thermal")
  
  # Path to the time series. Ideally, time series are stored in output. If it is
  # not the case, read the series in the input.
  pathInput <- file.path(opts$path, "ts-generator/thermal/mc-0")
  
  if (dir.exists(pathInput)) {
    filePattern <- sprintf("%s/%s/%%s.txt", pathInput, node)
  } else {
    pathInput <- file.path(opts$path, "../../input/thermal/series")
    filePattern <- sprintf("%s/%s/%%s/series.txt", pathInput, node)
  }
  
  # Read the Ids of the time series used in each Monte-Carlo Scenario.
  cls <- list.files(file.path(pathTSNumbers, node))
  if (length(cls) == 0) return(NULL)
  
  nameCls <- gsub(".txt", "", cls)
    
  tsIds <- llply(cls, function(cl) {
    as.numeric(readLines(file.path(pathTSNumbers, node, cl))[-1])
  })
  
  names(tsIds) <- nameCls
  
  # Two nested loops: clusters, Monte Carlo simulations.
  series <- ldply(nameCls, function(cl) {
    ids <- tsIds[[cl]][mcYears]
    colToRead <- sort(unique(ids)) # Columns to read in the ts file
    colIds <- sapply(ids, function(i) which(colToRead == i)) # correspondance between the initial ids and the columns in the generated table
    
    ts <- fread(sprintf(filePattern, cl), integer64 = "numeric", select = colToRead)[1:(24*7*52),]
    
    ldply(1:length(ids), function(i) {
      data.frame(
        node = node, 
        cluster = cl, 
        mcYear = mcYears[i],
        timeId = 1:nrow(ts),
        thermalAvailability = ts[[ colIds[i] ]]
      )
    })
  })
  
  series <- data.table(series)
  
  res <- changeTimeStep(series, timeStep, "hourly", opts=opts)
  
  if (synthesis) {
    res <- res[, .(thermalAvailability=mean(thermalAvailability)), keyby = .(node, cluster, timeId)]
  }
  
  res
}

.importMustRunModulation <- function(node, opts, ...) {
  if (!node %in% opts$nodesWithClusters) return(NULL)
  if (opts$antaresVersion < 500) return(NULL)
  
  path <- file.path(opts$path, "../../input/thermal/prepro", node)
  
  clusters <- list.files(path)
  
  res <- ldply(clusters, function(cl) {
    modulation <- fread(file.path(path, cl, "modulation.txt"), select = 4, colClasses = "numeric")
    
    setnames(modulation, names(modulation), "mustRunModulation")
    
    if (all(modulation$mustRunModulation == 0)) 
      return(data.table(mustRunModulation=numeric(), node = factor(), cluster = factor(), timeId=integer()))
    
    modulation$node <- node
    modulation$cluster <- cl
    modulation <- modulation[1:(24*12*52)]
    modulation$timeId <- 1:(24*12*52)
    
    modulation
  })
}

.importHydroStorage <- function(node, synthesis, timeStep, mcYears, opts, ...) {
  if (synthesis) mcYears <- 1:opts$mcYears
  
  pathTSNumbers <- file.path(opts$path, "ts-numbers/hydro")
  
  
  # Read the Ids of the time series used in each Monte-Carlo Scenario.
  tsIds <- as.numeric(readLines(file.path(pathTSNumbers, paste0(node, ".txt")))[-1])
  tsIds <- tsIds[mcYears]
  
  # Input time series
  pathInput <- file.path(opts$path, "ts-generator/hydro/mc-0")
  
  if (dir.exists(pathInput)) {
    f <- file.path(pathInput, node, "storage.txt")
  } else {
    pathInput <- file.path(opts$path, "../../input/hydro/series")
    f <- file.path(pathInput, node, "mod.txt")
  }
  
  if (file.size(f) == 0) {
    series <- ldply(1:length(tsIds), function(i) {
      data.frame(
        node = node, 
        mcYear = mcYears[i],
        timeId = 1:12,
        hydroStorage = rep(0L, 12)
      )
    })
  } else {
    colToRead <- sort(unique(tsIds)) # Columns to read in the ts file
    colIds <- sapply(tsIds, function(i) which(colToRead == i)) # link between the initial ids and the columns in the generated table
    
    
    ts <- fread(f, integer64 = "numeric", select = colToRead)
    
    N <- nrow(ts)
    
    series <- ldply(1:length(tsIds), function(i) {
      data.frame(
        node = node, 
        mcYear = mcYears[i],
        timeId = 1:nrow(ts),
        hydroStorage = ts[[ colIds[i] ]]
      )
    })
  }
  
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
