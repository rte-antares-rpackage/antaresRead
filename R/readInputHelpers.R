#' .importInputTS
#' 
#' Private function that reads input time series for a given node
#' 
#' @param node
#'   single node name.
#' @param timeStep
#'   desired time step.
#' @param opts 
#'   object of class "simOptions"
#' @param fileNamePattern
#'   string representing the path where the time series are located. Must
#'   contain one and only one "%s". This sequence will be replaced by the node
#'   name when the function is executed.
#' @param colnames
#'   (optionnal) name of the columns of the file. Useful only for simple time
#'   series (ie time series that does not change between scenarii) because we
#'   know in advance the number of columns of the file.
#' @param inputTimeStep
#'   actual time step of the input series.
#' @param fun
#'   function to use when changing time step of the data.
#'   
#' @return
#' If colnames is missing or empty and the file to read is also missing or
#' empty, then the function returns NULL. In all other cases, it returns
#' a data.table with one line per timeId. The table contains at least
#' columns "node" and "timeId".
#' 
#' @noRd
#' 
.importInputTS <- function(node, timeStep, opts, fileNamePattern, 
                                 colnames, inputTimeStep, fun = "sum", ...) {
  
  path <- file.path(opts$inputPath, sprintf(fileNamePattern, node))
  
  # If file does not exists or is empty, but we know the columns, then we
  # create a table filled with 0. Else we return NULL
  expectedRows <- switch(inputTimeStep, hourly=24*7*52, daily=7*52, monthly=12)
  
  if (!file.exists(path) || file.size(path) == 0) {
    if (missing(colnames) || length(colnames) == 0) return(NULL)
    inputTS <- data.table(matrix(0L, expectedRows,length(colnames)))
  } else {
    inputTS <- fread(path, integer64 = "numeric", header = FALSE)
    inputTS <- inputTS[1:expectedRows]
  }
  
  # Colnames
  if (!missing(colnames) && length(colnames) > 0) {
    setnames(inputTS, names(inputTS)[1:length(colnames)], colnames)
  }
  colnames <- names(inputTS)
  
  # Add node and timeId columns and put it at the begining of the table
  inputTS$node <- node
  inputTS$timeId <- 1:nrow(inputTS)
  setcolorder(inputTS, c("node", "timeId", colnames))
  
  changeTimeStep(inputTS, timeStep, inputTimeStep, fun = fun)
  
}

.importLoad <- function(node, timeStep, opts, ...) {
  .importInputTS(node, timeStep, opts, "load/series/load_%s.txt", inputTimeStep = "hourly")
}

.importThermalAvailabilities <- function(node, timeStep, opts, ...) {
  if (!node %in% opts$nodesWithClusters) return(NULL)
  
  clusters <- list.files(file.path(opts$inputPath, "thermal/series", node))
  
  ldply(clusters, function(cl) {
    filePattern <- sprintf("%s/%s/%%s/series.txt", "thermal/series", node)
    res <- .importInputTS(cl, timeStep, opts, filePattern, inputTimeStep = "hourly")
    
    res$node <- node
    res$cluster <- cl
    
    setcolorder(res, c("node", "cluster", "timeId", setdiff(names(res), c("node", "cluster", "timeId"))))
  })
  
}

.importROR <- function(node, timeStep, opts, ...) {
  .importInputTS(node, timeStep, opts, "hydro/series/%s/ror.txt", inputTimeStep = "hourly")
}

.importHydroStorageInput <- function(node, timeStep, opts, ...) {
  .importInputTS(node, timeStep, opts, "hydro/series/%s/mod.txt", inputTimeStep = "monthly")
}
  
.importHydroStorageMaxPower <- function(node, timeStep, opts, ...) {
  
  .importInputTS(node, timeStep, opts, "hydro/common/capacity/maxpower_%s.txt", 
                 colnames=c("low", "avg", "high"),
                 inputTimeStep = "daily")
  
}

.importWind <- function(node, timeStep, opts, ...) {
  .importInputTS(node, timeStep, opts, "wind/series/wind_%s.txt", inputTimeStep = "hourly")
}

.importSolar <- function(node, timeStep, opts, ...) {
  .importInputTS(node, timeStep, opts, "solar/series/solar_%s.txt", inputTimeStep = "hourly")
}

.importMisc <- function(node, timeStep, opts, ...) {
  
  .importInputTS(node, timeStep, opts, "misc-gen/miscgen-%s.txt", 
                 colnames=pkgEnv$miscNames,
                 inputTimeStep = "hourly")
  
}

.importReserves <- function(node, timeStep, opts, ...) {
  
  .importInputTS(node, timeStep, opts, "reserves/%s.txt", 
                 colnames=c("primaryRes", "strategicRes", "DSM", "dayAhead"),
                 inputTimeStep = "hourly")
  
}

.importLinkCapacity <- function(link, timeStep, opts, ...) {
  
  nodes <- strsplit(link, " - ")[[1]]
  
  colnames <- c("transCapacityDirect", "transCapacityIndirect",
                "impedances", "hurdlesCostDirect", "hurdlesCostIndirect")
  
  # A bit hacky, but it works !
  res <- .importInputTS(nodes[2], timeStep, opts, 
                        sprintf("%s/%%s.txt", file.path("links", nodes[1])), 
                        colnames=colnames,
                        inputTimeStep = "hourly")
  
  res$node <- NULL
  res$link <- link
  
  setcolorder(res, c("link", "timeId", colnames))
  
}

.importMustRunModulation <- function(node, opts, ...) {
  if (!node %in% opts$nodesWithClusters) return(NULL)
  if (opts$antaresVersion < 500) return(NULL)
  
  path <- file.path(opts$inputPath, "thermal/prepro", node)
  
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
