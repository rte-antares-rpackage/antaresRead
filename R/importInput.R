#Copyright © 2016 RTE Réseau de transport d’électricité

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
                           inputTimeStep, fun = "sum", type = "simple", colSelect = NULL, ...) {
  
  path <- file.path(opts$inputPath, sprintf(fileNamePattern, area))
  
  # if(!is.null(colSelect)){
  #   colnames <- colnames[colSelect]
  # }
  
  # If file does not exists or is empty, but we know the columns, then we
  # create a table filled with 0. Else we return NULL
  timeRange <- switch(inputTimeStep, 
                      hourly=c(opts$timeIdMin, opts$timeIdMax), 
                      daily=range(.getTimeId(opts$timeIdMin:opts$timeIdMax, "daily", opts)), 
                      monthly=range(.getTimeId(opts$timeIdMin:opts$timeIdMax, "monthly", opts)))
  
  
  if (opts$typeLoad == 'api' || (file.exists(path) && !file.size(path) == 0)) {
    if(is.null(colSelect))
    {
      # inputTS <- fread(path, integer64 = "numeric", header = FALSE, showProgress = FALSE)
      inputTS <- fread_antares(opts = opts, file = path, integer64 = "numeric", header = FALSE, showProgress = FALSE)
    } else {
      # inputTS <- fread(path, integer64 = "numeric", header = FALSE, select = colSelect, showProgress = FALSE)
      inputTS <- fread_antares(opts = opts, file = path, integer64 = "numeric", header = FALSE, select = colSelect, showProgress = FALSE)
    }
    
    # browser()
    if (opts$antaresVersion < 650) {
      if(!is.null(inputTS)){
        inputTS <- .reorderInputTSHydroStorage(inputTS, path, opts)
      }
    }
    
    if(!is.null(inputTS)){
      inputTS <- inputTS[timeRange[1]:timeRange[2]]
    } else {
      if (type == "matrix") return(NULL)
      inputTS <- data.table(matrix(0L, timeRange[2] - timeRange[1] + 1,length(colnames)))
    }
  } else {
    if (type == "matrix") return(NULL)
    inputTS <- data.table(matrix(0L, timeRange[2] - timeRange[1] + 1,length(colnames)))
  }
  
  if(!is.null(inputTS)){
    # Add area and timeId columns and put it at the begining of the table
    inputTS$area <- area
    inputTS$timeId <- timeRange[1]:timeRange[2]
    .reorderCols(inputTS)
    
    inputTS <- changeTimeStep(inputTS, timeStep, inputTimeStep, fun = fun, opts = opts)
    
    # If the data is a matrix of time series melt the data
    if (type == "matrix") {
      colnames <- c("tsId", colnames)
      inputTS <- melt(inputTS, id.vars = c("area", "timeId"))
      inputTS$variable <- as.integer(substring(inputTS$variable, 2))
    }
    
    setnames(inputTS, names(inputTS), c("area", "timeId", colnames))
  }
  inputTS
}

.importLoad <- function(area, timeStep, opts, ...) {
  .importInputTS(area, timeStep, opts, "load/series/load_%s.txt", "load", 
                 inputTimeStep = "hourly", type = "matrix")
}

.importThermalAvailabilities <- function(area, timeStep, opts, ...) {
  if (!area %in% opts$areasWithClusters) return(NULL)
  
  if(!"api" %in% opts$typeLoad){
    clusters <- list.files(file.path(opts$inputPath, "thermal/series", area))
  } else {
    clusters <- names(read_secure_json(file.path(opts$inputPath, "thermal/series", area), 
                                       token = opts$token, timeout = opts$timeout, config = opts$httr_config))
  }
  
  ldply(clusters, function(cl) {
    filePattern <- sprintf("%s/%s/%%s/series.txt", "thermal/series", area)
    res <- .importInputTS(cl, timeStep, opts, filePattern, "ThermalAvailabilities",
                          inputTimeStep = "hourly", type = "matrix")
    
    if (is.null(res)) return(NULL)
    
    res$area <- area
    res$cluster <- cl
    
    setcolorder(res, c("area", "cluster", "timeId", setdiff(names(res), c("area", "cluster", "timeId"))))
  })
  
}

.importResProduction <- function(area, timeStep, opts, ...) {
  
  if (!area %in% opts$areasWithResClusters) return(NULL)
  
  if(!"api" %in% opts$typeLoad){
    clusters <- list.files(file.path(opts$inputPath, "renewables/series", area))
  } else {
    clusters <- names(read_secure_json(file.path(opts$inputPath, "renewables/series", area), 
                                       token = opts$token, timeout = opts$timeout, config = opts$httr_config))
  }
  
  ldply(clusters, function(cl) {
    filePattern <- sprintf("%s/%s/%%s/series.txt", "renewables/series", area)
    res <- .importInputTS(cl, timeStep, opts, filePattern, "production",
                          inputTimeStep = "hourly", type = "matrix")
    
    if (is.null(res)) return(NULL)
    
    res$area <- area
    res$cluster <- cl
    
    setcolorder(res, c("area", "cluster", "timeId", setdiff(names(res), c("area", "cluster", "timeId"))))
  })
}

.importROR <- function(area, timeStep, opts, ...) {
  .importInputTS(area, timeStep, opts, "hydro/series/%s/ror.txt", "ror", 
                 inputTimeStep = "hourly", type = "matrix")
}

# "mingen" (v860)
.importmingen <- function(area, timeStep, opts, ...){
  .importInputTS(area, timeStep, opts, "hydro/series/%s/mingen.txt", "mingen", 
                 inputTimeStep = "hourly", type = "matrix")
}

.importHydroStorageInput <- function(area, timeStep, opts, ...) {
  inputTimeStepV <- ifelse(opts$antaresVersion >= 650, yes = "daily", no = "monthly")
  .importInputTS(area, timeStep, opts, "hydro/series/%s/mod.txt", "hydroStorage", 
                 inputTimeStep = inputTimeStepV, type = "matrix")
}

.importHydroStorageMaxPower <- function(area, timeStep, opts, unselect = NULL, ...) {
  
  unselect = unselect$areas
  if (opts$antaresVersion >= 650) {
    beginName <- c("generatingMaxPower", "generatingMaxEnergy", 
                   "pumpingMaxPower", "pumpingMaxEnergy")
  } else {
    beginName <- c("hstorPMaxLow", "hstorPMaxAvg", "hstorPMaxHigh")
  }
  
  if(!is.null(unselect)){
    colSelect <- which(!beginName%in%unselect)
    names <- beginName[colSelect]
  }else{
    colSelect <- NULL
    names <- beginName
  }
  
  .importInputTS(area, timeStep, opts, "hydro/common/capacity/maxpower_%s.txt", 
                 colnames=names,
                 inputTimeStep = "daily", fun = "mean", colSelect = colSelect)
  
}

.importWind <- function(area, timeStep, opts, ...) {
  .importInputTS(area, timeStep, opts, "wind/series/wind_%s.txt", "wind", 
                 inputTimeStep = "hourly", type = "matrix")
}

.importSolar <- function(area, timeStep, opts, ...) {
  .importInputTS(area, timeStep, opts, "solar/series/solar_%s.txt", "solar", 
                 inputTimeStep = "hourly", type = "matrix")
}

.importMisc <- function(area, timeStep, opts, colSelect = NULL, names = NULL, unselect = NULL, ...) {
  
  unselect = unselect$areas
  if(!is.null(unselect)){
    colSelect <- which(!pkgEnv$miscNames%in%unselect)
    names <- pkgEnv$miscNames[colSelect]
  }else{
    colSelect <- NULL
    names <- pkgEnv$miscNames
  }
  
  
  if(is.null(names)){
    names=pkgEnv$miscNames
  }
  .importInputTS(area, timeStep, opts, "misc-gen/miscgen-%s.txt", 
                 colnames=names,
                 inputTimeStep = "hourly", colSelect = colSelect)
  
}

.importReserves <- function(area, timeStep, opts, colSelect = NULL, names = NULL, unselect =NULL, ...) {
  beginName <- c("primaryRes", "strategicRes", "DSM", "dayAhead")
  unselect = unselect$areas
  
  if(!is.null(unselect)){
    colSelect <- which(!beginName%in%unselect)
    names <- beginName[colSelect]
  }else{
    colSelect <- NULL
    names <- beginName
  }
  
  .importInputTS(area, timeStep, opts, "reserves/%s.txt", 
                 colnames=names,
                 inputTimeStep = "hourly", colSelect = colSelect)
  
}

.importLinkCapacity <- function(link, timeStep, opts, unselect = NULL, ...) {
  
  
  
  areas <- strsplit(link, " - ")[[1]]
  
  unselect <- unselect$links
  
  #TODO DEL after some antaresVersion, by example, del this check after Antares
  #version 8 and check in readAntares the version
  if (opts$antaresVersion >= 650) {
    
    if (opts$antaresVersion >= 820) {
      
      beginName <- c("hurdlesCostDirect", "hurdlesCostIndirect",
                     "impedances", "loopFlow", "p.ShiftMin", "p.ShiftMax")
      fun = c("mean", "mean", "mean", "sum", "sum", "sum")
      
    }else{
      
      beginName <- c("transCapacityDirect", "transCapacityIndirect",
                     "hurdlesCostDirect", "hurdlesCostIndirect",
                     "impedances", "loopFlow", "p.ShiftMin", "p.ShiftMax")
      fun = c("sum", "sum", "mean", "mean", "mean", "sum", "sum", "sum")
      
    }
    
  } else {
    beginName <- c("transCapacityDirect", "transCapacityIndirect",
                   "impedances", "hurdlesCostDirect", "hurdlesCostIndirect")
    fun = c("sum", "sum", "mean", "mean", "mean")
  }
  
  if(!is.null(unselect)){
    colSelect <- which(!beginName%in%unselect)
    names <- beginName[colSelect]
  }else{
    colSelect <- NULL
    names <- beginName
  }
  
  
  if(opts$antaresVersion >= 820){
    #For V>8.2 read  transCapacityDirect in separated file, include MC
    if(!link%in%opts$linkList)return(NULL)
    ###Read parameters file
      res <- .importInputTS(areas[2], timeStep, opts, 
                            sprintf("%s/%%s_parameters.txt", file.path("links", areas[1])), 
                            colnames = names,
                            inputTimeStep = "hourly", 
                            fun = fun, colSelect = colSelect)
      
      
      ###Read transCapacityDirect file
      transCapacityDirect <- .importInputTS(areas[2], timeStep, opts, 
                                            sprintf("%s/capacities/%%s_direct.txt", file.path("links", areas[1])), 
                                            colnames = "transCapacityDirect",
                                            inputTimeStep = "hourly", type = "matrix",
                                            fun = "sum", colSelect = colSelect)
      
      ###Read transCapacityIndirect file
      transCapacityIndirect <- .importInputTS(areas[2], timeStep, opts, 
                                              sprintf("%s/capacities/%%s_indirect.txt", file.path("links", areas[1])), 
                                              colnames = "transCapacityIndirect",
                                              inputTimeStep = "hourly", type = "matrix",
                                              fun = "sum", colSelect = colSelect)
      res <- merge(transCapacityIndirect, res,  by = c("area","timeId"))
      res <- merge(transCapacityDirect, res, by = c("area","timeId", "tsId"))
      res <- res[order(area, tsId, timeId)]
      names <- c("tsId", "transCapacityDirect", "transCapacityIndirect", names)

  }else{
    
    # A bit hacky, but it works !
    res <- .importInputTS(areas[2], timeStep, opts, 
                          sprintf("%s/%%s.txt", file.path("links", areas[1])), 
                          colnames = names,
                          inputTimeStep = "hourly", 
                          fun = fun, colSelect = colSelect)
  }
  
  res$area <- NULL
  res$link <- link
  
  setcolorder(res, c("link", "timeId", names))
  
}




.importThermalData <- function(area, opts, timeStep, unselect = NULL, ...) {
  if (!area %in% opts$areasWithClusters) return(NULL)
  unselect <- unselect$areas
  path <- file.path(opts$inputPath, "thermal/prepro", area)
  
  if(!"api" %in% opts$typeLoad){
    clusters <- list.files(path)
  } else {
    clusters <- names(read_secure_json(path, token = opts$token, timeout = opts$timeout, config = opts$httr_config))
  }
  
  beginName <- c("FODuration", "PODuration", "FORate", "PORate", "NPOMin", "NPOMax")
  if(!is.null(unselect)){
    colSelect <- which(!beginName%in%unselect)
    names <- beginName[colSelect]
  }else{
    colSelect <- NULL
    names <- beginName
  }
  
  
  res <- ldply(clusters, function(cl) {
    if(is.null(colSelect))
    {
      # data <- fread(file.path(path, cl, "data.txt"), colClasses = "numeric")
      data <- fread_antares(opts = opts, file = file.path(path, cl, "data.txt"), colClasses = "numeric")
    }else{
      # data <- fread(file.path(path, cl, "data.txt"), select = colSelect, colClasses = "numeric")
      data <- fread_antares(opts = opts, file = file.path(path, cl, "data.txt"), select = colSelect, colClasses = "numeric")
    }
    
    setnames(data, 
             names(data), names)
    
    data$area <- area
    data$cluster <- cl
    
    # index blocks
    a <- opts$parameters$general$simulation.start
    b <- opts$parameters$general$simulation.end
    
    data <- data[a:b]
    data$timeId <- a:b
    
    changeTimeStep(data, timeStep, "daily", fun = "mean")
  })
}

.importThermalModulation <- function(area, opts, timeStep, unselect = NULL, ...) {
  if (!area %in% opts$areasWithClusters) return(NULL)
  unselect <- unselect$areas
  path <- file.path(opts$inputPath, "thermal/prepro", area)
  
  if(!"api" %in% opts$typeLoad){
    clusters <- list.files(path)
  } else {
    clusters <- names(read_secure_json(path, token = opts$token, timeout = opts$timeout, config = opts$httr_config))
  }
  
  beginName <- c("marginalCostModulation", "marketBidModulation", 
                 "capacityModulation", "minGenModulation")
  if(!is.null(unselect)){
    colSelect <- which(!beginName%in%unselect)
    names <- beginName[colSelect]
  }else{
    colSelect <- NULL
    names <- beginName
  }
  
  
  res <- ldply(clusters, function(cl) {
    if(is.null(colSelect))
    {
      # modulation <- fread(file.path(path, cl, "modulation.txt"), colClasses = "numeric")
      modulation <- fread_antares(opts = opts, file = file.path(path, cl, "modulation.txt"), colClasses = "numeric")
    }else{
      # modulation <- fread(file.path(path, cl, "modulation.txt"), select = colSelect, colClasses = "numeric")
      modulation <- fread_antares(opts = opts, file = file.path(path, cl, "modulation.txt"), select = colSelect, colClasses = "numeric")
    }
    
    setnames(modulation, 
             names(modulation), names)
    
    if (all(modulation$minGenModulation == 0)) 
      modulation[, minGenModulation := NA_real_]
    modulation$area <- area
    modulation$cluster <- cl
    modulation <- modulation[opts$timeIdMin:opts$timeIdMax]
    modulation$timeId <- opts$timeIdMin:opts$timeIdMax
    
    changeTimeStep(modulation, timeStep, "hourly", fun = "mean")
  })
}

# .changeNameInput <- function(path, opts){
#   out <- sub(pattern = "studies", "file", path)
#   out <- gsub(" ", "%20", out)
# }


# "st-storage" (>=v860)
.importSTStorage <- function(area, timeStep, opts, ...){

  if (!area %in% opts$areasWithSTClusters) 
    return(NULL)
  
  if(!"api" %in% opts$typeLoad){
    clusters <- list.files(
      file.path(opts$inputPath, 
                "st-storage/series", 
                area)
      )
    
    # "st-storage" have 5 txt files output for each cluster (v860)
      # + new optional TS (v920)
    list_names_txt_files <- unique(
      list.files(
        file.path(opts$inputPath,  
                  "st-storage/series", 
                  area, 
                  clusters)
        )
    )
    
    list_names_less_txt <- sub(pattern = ".txt", 
                               replacement = "", 
                               x = list_names_txt_files)
    
  } else {
    list_info_clusters <- read_secure_json(
      file.path(opts$inputPath, 
                "st-storage/series", 
                area), 
      token = opts$token, 
      timeout = opts$timeout, 
      config = opts$httr_config
      )
    
    clusters <- names(list_info_clusters)
    
    files_names <- names(list_info_clusters[[1]])
    
    list_names_txt_files <- paste0(files_names, ".txt")
    list_names_less_txt <- files_names 
  }
  
  # read TS for every cluster
  ldply(clusters, function(cl) {
    pattern <- paste0("%s/%s/%%s/", 
                      list_names_txt_files)
    filePatterns <- sprintf(pattern, 
                            "st-storage/series", area)
    
    res <- lapply(filePatterns, 
                  function(.x){
                    index_name_file <- which(filePatterns %in% .x)
                    
                    res_temp <- .importInputTS(area= cl, 
                                               timeStep= timeStep, 
                                               opts= opts, 
                                               fileNamePattern= .x,
                                               colnames= "st-storage",
                                               inputTimeStep = "hourly", 
                                               type = "matrix")
                    res_temp$name_file <- list_names_less_txt[index_name_file]
                    res_temp
                    })
    
    res <- rbindlist(res)

    if (is.null(res)) 
      return(NULL)
    
    res$area <- area
    res$cluster <- cl
    
    setcolorder(res, 
                c("area", 
                  "cluster", 
                  "timeId", 
                  setdiff(
                    names(res), 
                    c("area", 
                      "cluster", 
                      "timeId")))
                )
  })
  
  # added a column "name_file" to tag the file name
  
}