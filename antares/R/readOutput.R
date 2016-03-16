#' Read the output of an Antares simulation
#'
#' This function reads the output of a antares simulation for a set of nodes,
#' links and/or clusters at a desired resolution (hourly, daily, weekly,
#' monthly, annual). It can import synthetic results or Monte-Carlo simulations.
#'
#' @param nodes
#'   vector containing the names of the nodes to import. If
#'   \code{NULL} no node is imported. The special value \code{"all"} tells the
#'   function to import all nodes.
#' @param links
#'   vector containing the name of links to import. If \code{NULL} no
#'   node is imported. The special value \code{"all"} tells the function to
#'   import all nodes. Use function \code{\link{getLinks}} to import all links
#'   connected to some nodes.
#' @param clusters
#'   vector containing the name of the nodes for wich you want to
#'   import results at cluster level. If \code{NULL} no cluster is imported. The
#'   special value \code{"all"} tells the function to import clusters from all
#'   nodes.
#' @param inputs
#'   vector containing the name of the nodes for wich you want to
#'   import inputs.
#' @param misc
#'   vector containing the name of the nodes for wich you want to
#'   import misc.
#' @param select
#'   character vector containing the name of the columns to import. If this
#'   argument is \code{NULL}, all variables are imported. Special names
#'   \code{"allNodes"} and \code{"allLinks"} indicate to the function to import
#'   all variables for nodes or for links. The list of available variables can
#'   be seen with the command \code{getOption("antares")$variables}.  Id
#'   variables like \code{node}, \code{link} or \code{timeId} are automatically
#'   imported.
#' @param synthesis
#'   TRUE if you want to import the synthetic results. FALSE if
#'   you prefer to import year by year results.
#' @param mcYears
#'   Index of the Monte-Carlo years to import. Used only if
#'   synthesis is FALSE.
#' @param timeStep
#'   Resolution of the data to import: hourly (default), daily,
#'   weekly, monthly or annual.
#' @param parallel
#'   Should the importation be parallelized ? (See details)
#' @param simplify
#'   If TRUE and only one type of output is imported then a
#'   data.table is returned. If FALSE, the result will always be a list of class
#'   "antaresOutput".
#'
#' @details If all arguments are unspecified, the default behavior of the
#' function is to return the synthetized output for all nodes.
#'
#' If you import several elements of the same type (nodes, links, clusters), you
#' can use parallelized importation to improve performance. Setting the
#' parameter \code{parallel = TRUE} is not enough to parallelize the
#' importation, you also have to install the package
#' \href{https://cran.r-project.org/web/packages/foreach/index.html}{foreach}
#' and a package that provides a parallel backend (for instance the package
#' \href{https://cran.r-project.org/web/packages/doParallel/index.html}{doParallel}).
#'
#' Before running the function with argument \code{parallel=TRUE}, you need to
#' register your parallel backend. For instance, if you use package "doParallel"
#' you need to use the function \code{\link{registerDoParallel}} once per
#' session.
#'
#' @return If \code{simplify = TRUE} and only one type of output is imported
#' then the result is a data.table.
#'
#' Else an object of class "antaresOutput" is returned. It is a list of
#' data.tables, each element representing one type of element (nodes, links,
#' clusters)
#'
#' @examples
#' if (is.null(getOption("antares"))) setSimulationPath()
#'
#' # Import nodes and links separately
#' nodes <- readOutput()
#' links <- readOutput(links="all")
#'
#' # Import nodes and links at same time
#' output <- readOutput(nodes = "all", links = "all")
#'
#' # Get all output for one node
#' myNode <- sample(getOption("antares")$nodeList, 1)
#' myNode
#'
#' myNodeOutput <- readOutput(node = myNode, links = getLinks(myNode),
#'                            clusters = myNode)
#'
#' @export
#'
readOutput <- function(nodes = NULL, links = NULL, clusters = NULL,
                       inputs = NULL, misc = NULL,
                       select = NULL,
                       synthesis = getOption("antares")$synthesis,
                       mcYears = 1:getOption("antares")$mcYears,
                       timeStep = c("hourly", "daily", "weekly", "monthly", "annual"),
                       parallel = FALSE, simplify = TRUE) {

  timeStep <- match.arg(timeStep)
  if (!is.list(select)) select <- list(nodes = select, links = select)
  opts <- getOption("antares")

  # If all arguments are NULL, import all nodes
  if (is.null(nodes) & is.null(links) & is.null(clusters)) nodes <- "all"

  # Manage special values ("all", "allNode", "allLink")
  if (identical(nodes, "all")) nodes <- opts$nodeList
  if (identical(links, "all")) links <- opts$linkList
  if (identical(clusters, "all")) clusters <- opts$nodesWithClusters

  select$nodes <- if("allNodes" %in% select$nodes) NULL else select$nodes
  select$links <- if("allLinks" %in% select$links) NULL else select$links

  # Can the importation be parallelized ?
  if (parallel) {
    if(!require(foreach)) stop("Parallelized importation impossible. Please install the 'foreach' package and a parallel backend provider like 'doParallel'.")
    if (!getDoParRegistered()) stop("Parallelized importation impossible. Please register a parallel backend, for instance with function 'registerDoParallel'")
  }

  res <- list() # Object the function will return

  # local function that add a type of output to the object "res"
  .addOutputToRes <- function(name, ids, outputFun, select) {
    if (is.null(ids) | length(ids) == 0) return(NULL)

    cat(sprintf("Importing %s\n", name))

    tmp <- llply(ids, function(x, ...) outputFun(x, ...), .progress="text",
                 synthesis=synthesis, mcYears=mcYears,timeStep=timeStep,
                 opts=opts, select = select,
                 .parallel = parallel,
                 .paropts = list(.packages="antares"))

    res[[name]] <<- rbindlist(tmp)
  }

  # Add output to res object.
  .addOutputToRes("nodes", nodes, .importOutputForNode, select$nodes)
  .addOutputToRes("links", links, .importOutputForLink, select$links)
  .addOutputToRes("clusters", clusters, .importOutputForClusters, NULL)

  class(res) <- append("antaresOutput", class(res))

  # Simplify the result if possible
  if (simplify & length(res) == 1) res <- res[[1]]

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
