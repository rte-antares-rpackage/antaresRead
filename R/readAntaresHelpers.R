#' .getOutputHeader
#'
#' Private function that uses the first lines of an output file to generate
#' column names for this file.
#'
#' @param path
#' Path of the output file
#' @param objectName
#' (character) object type represented in the file (node ou link)
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
#' - "areas", "values"  => nodes
#' - "areas", "details" => clusters
#' - "links", "values"  => links
#'
#' @return
#' a table if synthesis=TRUE or a list of tables (one table per Monte-Carlo year)
#'
#' @noRd
#'
.importOutput <- function(folder, file, id, objectName, synthesis, mcYears, timeStep, opts, select) {

  if (synthesis) { # Only get synthesis results ################################

    path <- sprintf("%s/%s/mc-all/%s/%s/%s-%s.txt",
                    opts$path, opts$opath, folder, id, file, timeStep)

    # Check existence of the output file
    if (!file.exists(path)) {
      message(timeStep,  " output not found for ", objectName, " ", id)
      return(NULL)
    }

    colNames <- .getOutputHeader(path, objectName)

    # Select columns to import
    if (is.null(select)) {
      selectCol <- 1:length(colNames)
    } else {
      selectCol <- which(colNames %in% c(pkgEnv$idVars, select))
      colNames <- colNames[selectCol]
    }

    res <- fread(path, sep = "\t", header = F, skip = 7, select = selectCol,
                 stringsAsFactors = TRUE, integer64 = "numeric")
    setnames(res, names(res), colNames)

    res[, objectName] <- as.factor(rep(id, nrow(res)))

  } else { # Get Monte Carlo scenarios #########################################

    path <- sprintf("%s/%s/mc-ind/%%05.0f/%s/%s/%s-%s.txt",
                    opts$path, opts$opath, folder, id, file, timeStep)

    # Check output is available.
    if (!file.exists(sprintf(path, 1))) {
      message(timeStep,  " output not found for ", objectName, " ", id)
      return(NULL)
    }

    # Loop over Monte-Carlo years.
    res <- llply(mcYears, function(i) {

      colNames <- .getOutputHeader(sprintf(path,i), objectName)

      # Select columns to import
      if (is.null(select)) {
        selectCol <- 1:length(colNames)
      } else {
        selectCol <- which(colNames %in% c(pkgEnv$idVars, select))
        colNames <- colNames[selectCol]
      }

      # Import output
      x <- fread(sprintf(path,i), sep = "\t", header = F, skip = 7,
                 select = selectCol,
                 stringsAsFactors = TRUE, integer64 = "numeric")
      x$mcYear <- i
      setnames(x, names(x), c(colNames, "mcYear"))

      x[, objectName] <- as.factor(rep(id, nrow(x)))

      x
    })
  }

  res
}

#' .importOutputForNode
#'
#' Private function used to import the output for one node.
#'
#' @return
#' a data.table
#'
#' @noRd
#'
.importOutputForNode <- function(node, synthesis, ...) {
  res <- .importOutput("areas", "values", node, "node", synthesis, ...)
  if (is.null(res)) return (NULL)

  if (!synthesis)  res <- rbindlist(res)

  res
}

#' .importOutputForClusters
#'
#' Private function used to import the output for the clusters of one node
#'
#' @return
#' a data.table
#'
#' @noRd
#'
.importOutputForClusters <- function(node, synthesis, ...) {
  res <- .importOutput("areas", "details", node, "node", synthesis, ...)
  if (is.null(res)) return(NULL)

  .reshapeData <- function(x) {
    # For each cluster, there are two or three columns with same name but 
    #different content. Fix that.

    n <- names(x)
    idx <- ! n %in% pkgEnv$idVars
    
    clusterNames <- unique(n[idx])
    colnames <- c(paste0(clusterNames, "|MWh"), paste0(clusterNames, "|NP Cost"))
    if (sum(idx) / length(clusterNames) == 3) colnames <- c(colnames, paste0(clusterNames, "|NODU"))
    n[idx] <- colnames
    
    setnames(x, 1:ncol(x), n)

    # reshape data
    x <- data.table::melt(x, id.vars = intersect(pkgEnv$idVars, names(x)))
    x$cluster <- as.factor(tolower(gsub("\\|.*$", "", x$variable)))
    x$unit <- gsub("^.*\\|", "", x$variable)
    x$variable <- NULL
    data.table::dcast(x, ... ~ unit, value.var = "value", fun.aggregate = sum)
  }

  if (synthesis) {
    res <- .reshapeData(res)
  } else {
    res <- llply(res, .reshapeData)
    res <- rbindlist(res)
  }
  
  setnames(res, "MWh", "production")

  res
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
.importOutputForLink <- function(link, synthesis, ...) {
  res <- .importOutput("links", "values", link, "link", synthesis, ...)
  if (is.null(res)) return (NULL)

  if (!synthesis)  res <- rbindlist(res)

  res
}

# The two following functions read input time series that are eventually
# stored in output and rebuild the actual time series used in each Monte-Carlo
# simulation

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

