#' .importInputTS
#' 
#' Private function that reads input time series for a given area
#' 
#' @param area
#'   single area name.
#' @param timeStep
#'   desired time step.
#' @param opts 
#'   object of class "simOptions"
#' @param fileNamePattern
#'   string representing the path where the time series are located. Must
#'   contain one and only one "%s". This sequence will be replaced by the area
#'   name when the function is executed.
#' @param colnames
#'   name of the columns of the file.
#' @param inputTimeStep
#'   actual time step of the input series.
#' @param fun
#'   function to use when changing time step of the data.
#' @param type
#'   "simple" or "matrix" if the data to import is a matrix of time series
#'   representing the same variable
#'   
#' @return
#' If colnames is missing or empty and the file to read is also missing or
#' empty, then the function returns NULL. In all other cases, it returns
#' a data.table with one line per timeId. The table contains at least
#' columns "area" and "timeId".
#' 
#' @noRd
#' 
.importInputTS <- function(area, timeStep, opts, fileNamePattern, colnames, 
                           inputTimeStep, fun = "sum", type = "simple", ...) {
  
  path <- file.path(opts$inputPath, sprintf(fileNamePattern, area))
  
  # If file does not exists or is empty, but we know the columns, then we
  # create a table filled with 0. Else we return NULL
  timeRange <- switch(inputTimeStep, 
                      hourly=c(opts$timeIdMin, opts$timeIdMax), 
                      daily=range(.getTimeId(opts$timeIdMin:opts$timeIdMax, "daily", opts)), 
                      monthly=range(.getTimeId(opts$timeIdMin:opts$timeIdMax, "monthly", opts)))
  
  if (!file.exists(path) || file.size(path) == 0) {
    if (type == "matrix") return(NULL)
    inputTS <- data.table(matrix(0L, timeRange[2] - timeRange[1] + 1,length(colnames)))
  } else {
    inputTS <- fread(path, integer64 = "numeric", header = FALSE)
    inputTS <- inputTS[timeRange[1]:timeRange[2]]
  }
  
  # Add area and timeId columns and put it at the begining of the table
  inputTS$area <- area
  inputTS$timeId <- timeRange[1]:timeRange[2]
  .reorderCols(inputTS)
  
  inputTS <- changeTimeStep(inputTS, timeStep, inputTimeStep, fun = fun)
  
  # If the data is a matrix of time series melt the data
  if (type == "matrix") {
    colnames <- c("tsId", colnames)
    inputTS <- melt(inputTS, id.vars = c("area", "timeId"))
    inputTS$variable <- as.integer(substring(inputTS$variable, 2))
  }
  
  setnames(inputTS, names(inputTS), c("area", "timeId", colnames))
  
  inputTS
}

.importLoad <- function(area, timeStep, opts, ...) {
  .importInputTS(area, timeStep, opts, "load/series/load_%s.txt", "load", 
                 inputTimeStep = "hourly", type = "matrix")
}

.importThermalAvailabilities <- function(area, timeStep, opts, ...) {
  if (!area %in% opts$areasWithClusters) return(NULL)
  
  clusters <- list.files(file.path(opts$inputPath, "thermal/series", area))
  
  ldply(clusters, function(cl) {
    filePattern <- sprintf("%s/%s/%%s/series.txt", "thermal/series", area)
    res <- .importInputTS(cl, timeStep, opts, filePattern, "ThermalAvailabilities",
                          inputTimeStep = "hourly", type = "matrix")
    
    res$area <- area
    res$cluster <- cl
    
    setcolorder(res, c("area", "cluster", "timeId", setdiff(names(res), c("area", "cluster", "timeId"))))
  })
  
}

.importROR <- function(area, timeStep, opts, ...) {
  .importInputTS(area, timeStep, opts, "hydro/series/%s/ror.txt", "ror", 
                 inputTimeStep = "hourly", type = "matrix")
}

.importHydroStorageInput <- function(area, timeStep, opts, ...) {
  .importInputTS(area, timeStep, opts, "hydro/series/%s/mod.txt", "hydroStorage", 
                 inputTimeStep = "monthly", type = "matrix")
}
  
.importHydroStorageMaxPower <- function(area, timeStep, opts, ...) {
  
  .importInputTS(area, timeStep, opts, "hydro/common/capacity/maxpower_%s.txt", 
                 colnames=c("hstorPMaxLow", "hstorPMaxAvg", "hstorPMaxHigh"),
                 inputTimeStep = "daily", fun = "mean")
  
}

.importWind <- function(area, timeStep, opts, ...) {
  .importInputTS(area, timeStep, opts, "wind/series/wind_%s.txt", "wind", 
                 inputTimeStep = "hourly", type = "matrix")
}

.importSolar <- function(area, timeStep, opts, ...) {
  .importInputTS(area, timeStep, opts, "solar/series/solar_%s.txt", "solar", 
                 inputTimeStep = "hourly", type = "matrix")
}

.importMisc <- function(area, timeStep, opts, ...) {
  
  .importInputTS(area, timeStep, opts, "misc-gen/miscgen-%s.txt", 
                 colnames=pkgEnv$miscNames,
                 inputTimeStep = "hourly")
  
}

.importReserves <- function(area, timeStep, opts, ...) {
  
  .importInputTS(area, timeStep, opts, "reserves/%s.txt", 
                 colnames=c("primaryRes", "strategicRes", "DSM", "dayAhead"),
                 inputTimeStep = "hourly")
  
}

.importLinkCapacity <- function(link, timeStep, opts, ...) {
  
  areas <- strsplit(link, " - ")[[1]]
  
  colnames <- c("transCapacityDirect", "transCapacityIndirect",
                "impedances", "hurdlesCostDirect", "hurdlesCostIndirect")
  
  # A bit hacky, but it works !
  res <- .importInputTS(areas[2], timeStep, opts, 
                        sprintf("%s/%%s.txt", file.path("links", areas[1])), 
                        colnames=colnames,
                        inputTimeStep = "hourly", 
                        fun = c("sum", "sum", "mean", "mean", "mean"))
  
  res$area <- NULL
  res$link <- link
  
  setcolorder(res, c("link", "timeId", colnames))
  
}

.importThermalModulation <- function(area, opts, timeStep, ...) {
  if (!area %in% opts$areasWithClusters) return(NULL)
  
  path <- file.path(opts$inputPath, "thermal/prepro", area)
  
  clusters <- list.files(path)
  
  res <- ldply(clusters, function(cl) {
    modulation <- fread(file.path(path, cl, "modulation.txt"), colClasses = "numeric")
    
    setnames(modulation, 
             names(modulation), 
             c("marginalCostModulation", "marketBidModulation", 
               "capacityModulation", "minGenModulation"))
    
    
    
    if (all(modulation$minGenModulation == 0)) 
      modulation[, minGenModulation := NA_real_]
    
    modulation$area <- area
    modulation$cluster <- cl
    modulation <- modulation[opts$timeIdMin:opts$timeIdMax]
    modulation$timeId <- opts$timeIdMin:opts$timeIdMax
    
    changeTimeStep(modulation, timeStep, "hourly", fun = "mean")
  })
}