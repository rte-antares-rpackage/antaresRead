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
                          objectDisplayName = objectName) {
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
  timeRange <- .getTimeId(c(opts$timeIdMin, opts$timeIdMax), timeStep, opts)
  timeIds <- seq(timeRange[1], timeRange[2])
  
  
  res <- llply(1:nrow(args), function(i) {
    if (!sameNames) {
      colNames <- .getOutputHeader(args$path[i], objectName)
      selectCol <- which(!colNames %in% pkgEnv$idVars)
      colNames <- colNames[selectCol]
    }
    data <- fread(args$path[i], sep = "\t", header = F, skip = 7,
                  select = selectCol, integer64 = "numeric")
    
    setnames(data, names(data), colNames)
    
    data[, c(objectName) := args$id[i]]
    data[, timeId := timeIds]
    if (!is.null(mcYears)) data[, mcYear := args$mcYear[i]]
    
    if (!is.null(processFun)) data <- processFun(data)
    
    data
  }, .progress = ifelse(showProgress, "text", "none"))
  
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
                                 showProgress, opts) {
  .importOutput("areas", "values", "area", areas, timeStep, select, 
                mcYears, showProgress, opts)
}

.importOutputForDistricts <- function(districts, timeStep, select = NULL, mcYears = NULL, 
                                      showProgress, opts) {
  if (is.null(districts)) return(NULL)
  
  processFun <- function(dt) {
    dt[, district := as.factor(gsub("^@ ", "", district))]
  }
  
  .importOutput("areas", "values", "district", paste("@", districts), timeStep, select, 
                mcYears, showProgress, opts, processFun)
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
                                  showProgress, opts) {
  processFun <- function(x) {
    n <- names(x)
    idx <- ! n %in% pkgEnv$idVars
    
    clusterNames <- unique(n[idx])
    colnames <- c(paste0(clusterNames, "|MWh"), paste0(clusterNames, "|NP Cost"))
    if (sum(idx) / length(clusterNames) == 3) colnames <- c(colnames, paste0(clusterNames, "|NODU"))
    n[idx] <- colnames
    
    setnames(x, 1:ncol(x), n)
    
    # reshape data
    x <- data.table::melt(x, id.vars = .idCols(x))
    x$cluster <- as.factor(tolower(gsub("\\|.*$", "", x$variable)))
    x$unit <- gsub("^.*\\|", "", x$variable)
    x$variable <- NULL
    data.table::dcast(x, ... ~ unit, value.var = "value", fun.aggregate = sum)
  }
  
  .importOutput("areas", "details", "area", areas, timeStep, NULL, 
                mcYears, showProgress, opts, processFun, sameNames = FALSE,
                objectDisplayName = "cluster")
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
                                 showProgress, opts) {
  .importOutput("links", "values", "link", links, timeStep, select, 
                mcYears, showProgress, opts)
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
    
    ts <- fread(sprintf(filePattern, cl), integer64 = "numeric", select = colToRead)[1:(24*7*52),]
    
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

