#' Import the output of an Antares simulation
#'
#' This function imports the output of a antares simulation for some nodes,
#' links and Monte-Carlo years specified by the user.
#'
#' @param nodes
#' vector containing the names of the nodes of interest. NULL if you do not want to
#' import results for any node.
#' @param links
#' vector containing the name of links of interest. NULL if you do not want to
#' import results for any link.
#' @param clusters
#' vector containing the name of the nodes for wich you want to import results at
#' cluster level.
#' @param inputs
#' vector containing the name of the nodes for wich you want to import inputs.
#' @param misc
#' vector containing the name of the nodes for wich you want to import misc.
#' @param synthesis
#' TRUE if you want to import the synthetic results. FALSE if you prefer to import
#' year by year results.
#' @param mcYears
#' Index of the Monte-Carlo years to import. Used only if synthesis is FALSE.
#' @param timeStep
#' Resolution of the data to import: hourly (default), daily, weekly, monthly or
#' annual.
#'
#' @return
#' An object of class "antaresOutput". It is a list with the following elements:
#' \itemize{
#'    \item{nodes: }{data.table containing}
#' }
#'
#' @export
#'
importOutput <- function(nodes = getOption("antares")$nodeList,
                         links = getOption("antares")$linkList,
                         clusters = NULL, inputs = NULL, misc = NULL,
                         synthesis = getOption("antares")$synthesis,
                         mcYears = 1:getOption("antares")$mcYears,
                         timeStep = c("hourly", "daily", "weekly", "monthly", "annual")) {

  timeStep <- match.arg(timeStep)
  res <- list()

  # Node results
  if (!is.null(nodes)) {
    cat("Importing nodes\n")
    res$nodes <- llply(nodes, .importOutputForNode,
                       synthesis = synthesis, mcYears=mcYears, timeStep=timeStep,
                       .progress="text")

    res$nodes <- rbindlist(res$nodes)
  }

  # Link results
  if (!is.null(links)) {
    cat("Importing links\n")
    res$links <- llply(links, .importOutputForLink,
                       synthesis = synthesis, mcYears=mcYears, timeStep=timeStep,
                       .progress="text")

    res$links <- rbindlist(res$links)
  }

  # cluster results
  if (!is.null(clusters)) {
    cat("Importing clusters\n")
    res$clusters <- llply(clusters, .importOutputForClusters,
                          synthesis = synthesis, mcYears=mcYears, timeStep=timeStep,
                         .progress="text")

    res$clusters <- rbindlist(res$clusters)
  }

  class(res) <- append("antaresOutput", class(res))

  res
}


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
.importOutput <- function(folder, file, id, objectName, synthesis, mcYears, timeStep) {
  opts <- getOption("antares")

  if (synthesis) {
    path <- sprintf("%s/%s/mc-all/%s/%s/%s-%s.txt",
                    opts$path, opts$opath, folder, id, file, timeStep)

    if (!file.exists(path)) {
      message(timeStep,  " output not found for ", objectName, " ", id)
      return(NULL)
    }

    res <- fread(path, sep = "\t", header = F, skip = 7, stringsAsFactors = TRUE,
                 integer64 = "numeric")
    setnames(res, names(res), .getOutputHeader(path, objectName))

  } else {
    path <- sprintf("%s/%s/mc-ind/%%05.0f/%s/%s/%s-%s.txt",
                    opts$path, opts$opath, folder, id, file, timeStep)

    if (!file.exists(sprintf(path, 1))) {
      message(timeStep,  " output not found for ", objectName, " ", id)
      return(NULL)
    }

    res <- llply(mcYears, function(i) {
      x <- fread(sprintf(path,i), sep = "\t", header = F, skip = 7,
                 stringsAsFactors = TRUE, integer64 = "numeric")
      x$mcYear <- i
      setnames(x, names(x),
               c(.getOutputHeader(sprintf(path, i), objectName), "mcYear"))

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
.importOutputForNode <- function(node, synthesis, mcYears, timeStep) {
  res <- .importOutput("areas", "values", node, "node", synthesis, mcYears, timeStep)
  if (is.null(res)) return (NULL)

  if (!synthesis)  res <- rbindlist(res)

  res$node <- as.factor(rep(node, nrow(res)))

  res
}

#' .importOutputForClusters
#'
#' Private function used to import the output for the clusters of one node
#'
#' @return
#' a data.table
.importOutputForClusters <- function(node, synthesis, mcYears, timeStep) {
  res <- .importOutput("areas", "details", node, "node", synthesis, mcYears, timeStep)
  if (is.null(res)) return(NULL)

  .reshapeData <- function(x) {
    # For each cluster, there are two columns with same name but diffrent content.
    # Fix that.
    idVars <- c("node", "timeId", "day", "week", "month", "hour", "mcYear")

    n <- names(x)
    idx <- ! n %in% idVars
    n[idx] <- paste0(n[idx] , ifelse(duplicated(n[idx]), "|NP Cost", "|MWh"))
    setnames(x, 1:ncol(x), n)

    # reshape data
    x <- data.table::melt(x, id.vars = intersect(idVars, names(x)))
    x$cluster <- gsub("\\|.*$", "", x$variable)
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

  res$node <- as.factor(rep(node, nrow(res)))

  res
}

#' .importOutputForLink
#'
#' Private function used to import the output of one link.
#'
#' @return
#' a data.table
.importOutputForLink <- function(link, synthesis, mcYears, timeStep) {
  res <- .importOutput("links", "values", link, "link", synthesis, mcYears, timeStep)
  if (is.null(res)) return (NULL)

  if (!synthesis)  res <- rbindlist(res)

  res$link <- as.factor(rep(link, nrow(res)))

  res
}
