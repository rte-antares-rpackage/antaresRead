# Copyright © 2016 RTE Réseau de transport d’électricité

#' .getOutputHeader
#'
#' Private function that uses the first lines of an output file to generate
#' column names for this file.
#'
#' @param path
#' Path of the output file
#' @param objectName
#' (character) object type represented in the file (area ou link)
#'
#' @return
#' Vector containing the generated column names.
#'
#' @noRd
#'
.getOutputHeader <- function(path, objectName) {
  colname <- read.table(path, header=F, skip = 4, nrows = 3, sep = "\t")
  colname <- apply(colname[c(1,3),], 2, paste, collapse = "_")
  colname[1:2] <- c(objectName, "timeId")
  colname <- gsub("^_|_EXP$|_values$|_$", "", colname)
  colname
}

#' .importOutput
#'
#' Private function used to import the results of a simulation. The type of result
#' is determined by the arguments "folder" and "file"
#' - "areas", "values"  => areas
#' - "areas", "details" => clusters
#' - "links", "values"  => links
#'
#' @return
#' a table if synthesis=TRUE or a list of tables (one table per Monte-Carlo year)
#'
#' @noRd
#'
.importOutput <- function(folder, fileName, objectName, ids, timeStep, select = NULL, 
                          mcYears = NULL, 
                          showProgress, opts, processFun = NULL, sameNames = TRUE,
                          objectDisplayName = objectName, parallel) {
  if (is.null(ids)) return(NULL)
  
  if (showProgress) cat("Importing ", objectDisplayName, "s\n", sep = "")
  
  if (is.null(mcYears)) {
    args <- expand.grid(id = ids)
    args$path <- sprintf("%s/mc-all/%s/%s/%s-%s.txt", 
                         opts$simDataPath, folder, args$id, fileName, timeStep)
  } else {
    args <- expand.grid(id = ids, mcYear = mcYears)
    args$path <- sprintf("%s/mc-ind/%05.0f/%s/%s/%s-%s.txt",
                         opts$simDataPath, args$mcYear, folder, args$id, fileName, timeStep)
  }
  
  outputMissing <- !file.exists(args$path)
  
  if (all(outputMissing)) {
    message("No data corresponding to your query.")
    return(NULL)
  } else if (any(outputMissing)) {
    message("Some requested output files are missing.")
    args <- args[file.exists(args$path), ]
  }
  
  # columns to retrieve
  if (sameNames) {
    colNames <- .getOutputHeader(args$path[1], objectName)
  
    if (is.null(select)) {
      # read all columns except the time variables that will be recreated
      selectCol <- which(!colNames %in% pkgEnv$idVars)
    } else {
      selectCol <- which(colNames %in% select)
    }
    colNames <- colNames[selectCol]
  }
  
  # time ids
  if (timeStep == "annual") {
    timeIds <- 1L
  } else {
    timeRange <- .getTimeId(c(opts$timeIdMin, opts$timeIdMax), timeStep, opts)
    timeIds <- seq(timeRange[1], timeRange[2])
  }
  
  
  if(!is.null((getDefaultReactiveDomain())))
  {
    n <- nrow(args)
    withProgress(message = 'antaresRead', value = 0, {
  res <- llply(
    1:nrow(args), 
    function(i) {
      incProgress(1/n, detail = paste0("Importing ", folder, " data"))
      
      if (!sameNames) {
        colNames <- .getOutputHeader(args$path[i], objectName)
        selectCol <- which(!colNames %in% pkgEnv$idVars)
        colNames <- colNames[selectCol]
      }
      
      if (length(selectCol) == 0) {
        data <- data.table(timeId = timeIds)
      } else {
        data <- fread(args$path[i], sep = "\t", header = F, skip = 7,
                      select = selectCol, integer64 = "numeric")
        
        # fix data.table bug on integer64
        any_int64 <- colnames(data)[which(sapply(data, function(x) "integer64" %in% class(x)))]
        if(length(any_int64) > 0){
          data[, c(any_int64) := lapply(.SD, as.numeric), .SDcols = any_int64]
        }
        
        setnames(data, names(data), colNames)
        data[, timeId := timeIds]
      }
      
      data[, c(objectName) := args$id[i]]
      if (!is.null(mcYears)) data[, mcYear := args$mcYear[i]]
      
      if (!is.null(processFun)) data <- processFun(data)
      
      data
    }, 
    .progress = ifelse(showProgress, "text", "none"),
    .parallel = parallel,
    .paropts = list(.packages = "antaresRead")
  )
    })
  
  }else{
    res <- llply(
      1:nrow(args), 
      function(i) {
        
        if (!sameNames) {
          colNames <- .getOutputHeader(args$path[i], objectName)
          selectCol <- which(!colNames %in% pkgEnv$idVars)
          colNames <- colNames[selectCol]
        }
        
        if (length(selectCol) == 0) {
          data <- data.table(timeId = timeIds)
        } else {
          data <- fread(args$path[i], sep = "\t", header = F, skip = 7,
                        select = selectCol, integer64 = "numeric")
          
          # fix data.table bug on integer64
          any_int64 <- colnames(data)[which(sapply(data, function(x) "integer64" %in% class(x)))]
          if(length(any_int64) > 0){
            data[, c(any_int64) := lapply(.SD, as.numeric), .SDcols = any_int64]
          }
          
          setnames(data, names(data), colNames)
          data[, timeId := timeIds]
        }
        
        data[, c(objectName) := args$id[i]]
        if (!is.null(mcYears)) data[, mcYear := args$mcYear[i]]
        
        if (!is.null(processFun)) data <- processFun(data)
        
        data
      }, 
      .progress = ifelse(showProgress, "text", "none"),
      .parallel = parallel,
      .paropts = list(.packages = "antaresRead")
    )
  }
  
  rbindlist(res)
}

#' .importOutputForArea
#'
#' Private function used to import the output for one area.
#'
#' @return
#' a data.table
#'
#' @noRd
#'
.importOutputForAreas <- function(areas, timeStep, select = NULL, mcYears = NULL, 
                                  showProgress, opts, parallel) {
  suppressWarnings(
    .importOutput("areas", "values", "area", areas, timeStep, select, 
                  mcYears, showProgress, opts, parallel = parallel)
  )
}

.importOutputForDistricts <- function(districts, timeStep, select = NULL, mcYears = NULL, 
                                      showProgress, opts, parallel) {
  if (is.null(districts)) return(NULL)
  
  processFun <- function(dt) {
    dt[, district := as.factor(gsub("^@ ", "", district))]
  }
  
  suppressWarnings(
    .importOutput("areas", "values", "district", paste("@", districts), timeStep, select, 
                  mcYears, showProgress, opts, processFun, parallel = parallel)
  )
}

#' .importOutputForClusters
#'
#' Private function used to import the output for the clusters of one area
#'
#' @return
#' a data.table
#'
#' @noRd
#'
.importOutputForClusters <- function(areas, timeStep, select = NULL, mcYears = NULL, 
                                  showProgress, opts, mustRun = FALSE, parallel) {
  
  # In output files, there is one file per area with the follwing form:
  # cluster1-var1 | cluster2-var1 | cluster1-var2 | cluster2-var2
  # the following function reshapes the result to have variable cluster in column.
  # To improve greatly the performance we use our knowledge of the position of 
  # the columns instead of using more general functions like dcast.
  reshapeFun <- function(x) {
    # Get cluster names
    n <- names(x)
    idx <- ! n %in% pkgEnv$idVars
    clusterNames <- tolower(unique(n[idx]))
    
    # Id vars names
    idVarsId <- which(!idx)
    idVarsNames <- n[idVarsId]
    
    # Get final value columns
    if (sum(idx) / length(clusterNames) == 3) {
      colNames <- c("production", "NP Cost", "NODU")
    } else if (sum(idx) / length(clusterNames) == 2) {
      colNames <- c("production", "NP Cost")
    } else {
      colNames <- c("production")
    }
    
    # Loop over clusters
    nclusters <- length(clusterNames)
    ncols <- length(colNames)
    
    res <- llply(1:nclusters, function(i) {
      dt <- x[, c(nclusters * 0:(ncols - 1) + i, idVarsId), with = FALSE]
      setnames(dt, c(colNames, idVarsNames))
      dt[, cluster := as.factor(clusterNames[i])]
      dt
    })
    
    rbindlist(res)
  }
  
  if (!mustRun) {
    suppressWarnings(
      .importOutput("areas", "details", "area", areas, timeStep, NULL, 
                    mcYears, showProgress, opts, reshapeFun, sameNames = FALSE,
                    objectDisplayName = "cluster", parallel = parallel)
    )
    
  } else {
    # The partial must run for a cluster is defined as:
    # sum_t(min(production_t, capacity * minGenModulation_t))
    # This formula is non-linear, so if we need to get hourly data to compute
    # it. 
    # To avoid importing large amount of data, we first check if minGenModulation
    # is non null for at least one cluster.
    # If we have to use hourly data, we aggregate it directly at the desired
    # timestep to limit the amount of RAM required.
    
    # Get cluster capacity and must run mode
    clusterDesc <- readClusterDesc(opts)
    if(is.null(clusterDesc$must.run)) clusterDesc$must.run <- FALSE
    clusterDesc[is.na(must.run), must.run := FALSE]
    if (is.null(clusterDesc$min.stable.power)) clusterDesc$min.stable.power <- 0
    clusterDesc[is.na(min.stable.power), min.stable.power := 0]
    clusterDesc <- clusterDesc[, .(area, cluster,
                                   capacity = nominalcapacity * unitcount,
                                   min.stable.power,
                                   must.run)]
    
    # Are clusters in partial must run mode ?
    mod <- llply(areas, .importThermalModulation, opts = opts, timeStep = "hourly")
    mod <- rbindlist(mod)
    
    # Should we compute the partial must run ?
    if (is.null(mod$minGenModulation) || all(is.na(mod$minGenModulation) | mod$minGenModulation == 0)) {
      
      # We should not \o/
      res <- suppressWarnings(
        suppressWarnings(
          .importOutput("areas", "details", "area", areas, timeStep, NULL, 
                        mcYears, showProgress, opts, reshapeFun, sameNames = FALSE,
                        objectDisplayName = "cluster", parallel = parallel)
        )
      )
      res[, mustRunPartial := 0L]
      
    } else {
      
      # Worst case ! We have to !
      #
      if(timeStep!="hourly"){
        warning('Hourly data will be imported to compute partial must run min(production_t, capacity * minGenModulation_t). These data will be aggregated at the desired `timeStep`. ')
        
        #copy of warning in ChangeTimeStep
        warning('Aggregation will be perform approximatively because optimization variables in ANTARES are doubles but ANTARES write only integers in TXT files, with this transformation we lose precision. If you want accurate data then you must import the corresponding data with `readAntares`')
        
        messageWarningMcYears<-paste0("When mcYears is set to all or NULL : ", mcYears, " and timeStep is set to : " ,timeStep , " result for mustRun are not accurate. Hourly `synthetic` or `details` results will be aggregated at the desired `timeStep`.  " )
        
        if( is.null(mcYears) ){
          warning(messageWarningMcYears, call. = FALSE)
        }else if (is.character(mcYears)){
          if (mcYears=="all"){
            warning(messageWarningMcYears, call. = FALSE)
          }
        }else if (length(mcYears) > 1){
          warning(messageWarningMcYears, call. = FALSE)
        }
      }
      
      mod[is.na(minGenModulation), minGenModulation := 0]
      
      .mergeByRef(mod, clusterDesc)
      mod[, mustRunPartial := minGenModulation * capacity]
      
      setkey(mod, area, cluster, timeId)
      
      processFun <- function(x) {
        x <- reshapeFun(x)
        mustRunPartial <- mod[J(x$area, x$cluster, x$timeId), mustRunPartial]
        x[, mustRunPartial := pmin(production, mustRunPartial)]
        changeTimeStep(x, timeStep, "hourly", fun = "sum", opts = opts)
      }
      
      res <- suppressWarnings(
        .importOutput("areas", "details", "area", areas, "hourly", NULL, 
                      mcYears, showProgress, opts, processFun, 
                      sameNames = FALSE, objectDisplayName = "cluster", 
                      parallel = parallel)
      )
      
    }
    
    .mergeByRef(res, clusterDesc[,.(area, cluster, must.run, min.stable.power)])
    
    if (is.null(res$NODU)) res[, thermalPmin := 0]
    else res[, thermalPmin := min.stable.power * NODU]
    
    res[, `:=`(
      mustRun = production * must.run,
      mustRunTotal = production * must.run + mustRunPartial,
      must.run = NULL,
      min.stable.power = NULL
    )]
    
    res[, thermalPmin := pmax(thermalPmin, mustRunTotal)]
    
    res
  }
  
}

#' .importOutputForLink
#'
#' Private function used to import the output of one link.
#'
#' @return
#' a data.table
#'
#' @noRd
#'
.importOutputForLinks <- function(links, timeStep, select = NULL, mcYears = NULL, 
                                 showProgress, opts, parallel) {
  suppressWarnings(
    .importOutput("links", "values", "link", links, timeStep, select, 
                  mcYears, showProgress, opts, parallel = parallel)
  )
}

# The two following functions read input time series that are eventually
# stored in output and rebuild the actual time series used in each Monte-Carlo
# simulation

.importThermal <- function(area, synthesis, timeStep, mcYears, opts, ...) {
  if (!area %in% opts$areasWithClusters) return(NULL)
  if (synthesis) mcYears <- opts$mcYears
  
  # Path to the files containing the IDs of the time series used for each
  # Monte-Carlo years.
  pathTSNumbers <- file.path(opts$simPath, "ts-numbers/thermal")
  
  # Path to the time series. Ideally, time series are stored in output. If it is
  # not the case, read the series in the input.
  pathInput <- file.path(opts$simPath, "ts-generator/thermal/mc-0")
  
  if (dir.exists(pathInput)) {
    filePattern <- sprintf("%s/%s/%%s.txt", pathInput, area)
  } else {
    pathInput <- file.path(opts$inputPath, "thermal/series")
    filePattern <- sprintf("%s/%s/%%s/series.txt", pathInput, area)
  }
  
  # Read the Ids of the time series used in each Monte-Carlo Scenario.
  cls <- list.files(file.path(pathTSNumbers, area))
  if (length(cls) == 0) return(NULL)
  
  nameCls <- gsub(".txt", "", cls)
  
  tsIds <- llply(cls, function(cl) {
    as.numeric(readLines(file.path(pathTSNumbers, area, cl))[-1])
  })
  
  names(tsIds) <- nameCls
  
  # Two nested loops: clusters, Monte Carlo simulations.
  series <- ldply(nameCls, function(cl) {
    ids <- tsIds[[cl]][mcYears]
    colToRead <- sort(unique(ids)) # Columns to read in the ts file
    colIds <- sapply(ids, function(i) which(colToRead == i)) # correspondance between the initial ids and the columns in the generated table
    
    ts <- fread(sprintf(filePattern, cl), integer64 = "numeric", select = colToRead)
    
    ldply(1:length(ids), function(i) {
      data.frame(
        area = area, 
        cluster = cl, 
        mcYear = mcYears[i],
        timeId = 1:nrow(ts),
        thermalAvailability = ts[[ colIds[i] ]]
      )
    })
  })
  
  series <- data.table(series)
  
  series <- series[timeId %in% opts$timeIdMin:opts$timeIdMax]
  
  # Compute the number of available units
  clusterDesc <- readClusterDesc(opts)
  series <- merge(series, clusterDesc[, .(area, cluster, nominalcapacity)],
                  by = c("area", "cluster"))
  .mergeByRef(series, clusterDesc, on = c("area", "cluster"), "nominalcapacity")
  
  series[, availableUnits :=  ceiling(thermalAvailability / nominalcapacity)]
  series[, nominalcapacity := NULL]
  
  # Aggregation
  res <- changeTimeStep(series, timeStep, "hourly", opts=opts, fun = c("sum", "mean"))
  
  if (synthesis) {
    res <- res[, .(thermalAvailability=mean(thermalAvailability),
                   availableUnits = mean(availableUnits)), 
               keyby = .(area, cluster, timeId)]
  }
  
  res
}

.importHydroStorage <- function(area, synthesis, timeStep, mcYears, opts, ...) {
  if (synthesis) mcYears <- opts$mcYears
  
  pathTSNumbers <- file.path(opts$simPath, "ts-numbers/hydro")
  
  
  # Read the Ids of the time series used in each Monte-Carlo Scenario.
  tsIds <- as.numeric(readLines(file.path(pathTSNumbers, paste0(area, ".txt")))[-1])
  tsIds <- tsIds[mcYears]
  
  # Input time series
  pathInput <- file.path(opts$simPath, "ts-generator/hydro/mc-0")
  
  if (dir.exists(pathInput)) {
    f <- file.path(pathInput, area, "storage.txt")
  } else {
    pathInput <- file.path(opts$inputPath, "hydro/series")
    f <- file.path(pathInput, area, "mod.txt")
  }
  
  timeRange <- range(.getTimeId(opts$timeIdMin:opts$timeIdMax, "monthly", opts))
  
  if (file.size(f) == 0) {
    series <- ldply(1:length(tsIds), function(i) {
      data.frame(
        area = area, 
        mcYear = mcYears[i],
        timeId = timeRange[1]:timeRange[2],
        hydroStorage = rep(0L, length(timeRange[1]:timeRange[2]))
      )
    })
  } else {
    colToRead <- sort(unique(tsIds)) # Columns to read in the ts file
    colIds <- sapply(tsIds, function(i) which(colToRead == i)) # link between the initial ids and the columns in the generated table
    
    
    ts <- fread(f, integer64 = "numeric", select = colToRead)
    ts <- ts[timeRange[1]:timeRange[2]]
    
    series <- ldply(1:length(tsIds), function(i) {
      data.frame(
        area = area, 
        mcYear = mcYears[i],
        timeId = timeRange[1]:timeRange[2],
        hydroStorage = ts[[ colIds[i] ]]
      )
    })
  }
  
  series <- data.table(series)
  
  
  res <- changeTimeStep(series, timeStep, "monthly", opts=opts)
  
  if (synthesis) {
    res <- res[, .(hydroStorage=mean(hydroStorage)), keyby = .(area, timeId)]
  }
  
  res
  
}

