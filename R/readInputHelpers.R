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
  
  changeTimeStep(misc, timeStep, "hourly", opts=opts)
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
