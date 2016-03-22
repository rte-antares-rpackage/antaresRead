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
    # For each cluster, there are two columns with same name but different content.
    # Fix that.

    n <- names(x)
    idx <- ! n %in% pkgEnv$idVars
    n[idx] <- paste0(n[idx] , ifelse(duplicated(n[idx]), "|NP Cost", "|MWh"))
    setnames(x, 1:ncol(x), n)

    # reshape data
    x <- data.table::melt(x, id.vars = intersect(pkgEnv$idVars, names(x)))
    x$cluster <- as.factor(gsub("\\|.*$", "", x$variable))
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


#' .importMisc
#'
#' Private function used to read misc input files.
#'
#' @return
#' a data.table
#' @noRd
#'
.importMisc <- function(nodes, opts, timeStep) {
  if (is.null(nodes)) return(NULL)

  res <- llply(nodes, function(n) {
    path <- file.path(opts$path, "../../input/misc-gen",
                      sprintf("miscgen-%s.txt", n))

    if(file.size(path) == 0) misc <- data.table(matrix(0L, 24*7*52,length(pkgEnv$miscNames)))
    else misc <- fread(path, sep="\t", header = FALSE, integer64 = "numeric")

    setnames(misc, names(misc), pkgEnv$miscNames)

    misc$node <- n
    misc$timeId <- 1:nrow(misc)

    misc[1:(24 * 7 * 52)]
  })

  res <- rbindlist(res)
  setcolorder(res, c("node", "timeId", pkgEnv$miscNames))

  .aggregateByTimeStep(res, timeStep, opts)
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
.aggregateByTimeStep <- function(x, timeStep = c("hourly", "daily", "weekly", "monthly", "annual"), opts) {
  if (timeStep == "hourly") return(x)

  if (timeStep == "daily") {

    x$timeId <- (x$timeId - 1) %/% 24 + 1

  } else if (timeStep == "weekly") {

    tmp <- opts$start
    hour(tmp) <- hour(tmp) + 1:(24*7*52) - 1
    x$wday <- wday(tmp)

    startWeek <- which(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday") == opts$firstWeekday)

    x[, change := wday == startWeek & wday != shift(wday), by = node]
    x[is.na(change), change := TRUE]
    x[, timeId := cumsum(change), by = node]
    x$wday <- x$change <- NULL

  } else if (timeStep == "monthly") {

    tmp <- opts$start
    hour(tmp) <- hour(tmp) + 1:(24*7*52) - 1
    x$month <- month(tmp)

    x[, change :=  month != shift(month), by = node]
    x[is.na(change), change := TRUE]
    x[, timeId := cumsum(change), by = node]
    x$month <- x$change <- NULL

  } else if (timeStep == "annual") {

    x$timeId <- rep("annual", nrow(x))

  }

  x[, lapply(.SD, sum), keyby=.(node, timeId)]

}
