#' Read the output of an Antares simulation
#'
#' This function reads the output of a antares simulation for a set of nodes,
#' links and/or clusters at a desired resolution (hourly, daily, weekly,
#' monthly, annual). It can import synthetic results or Monte-Carlo simulations.
#'
#' @aliases readOutput
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
#'   vector containing the name of the nodes for which you want to
#'   import results at cluster level. If \code{NULL} no cluster is imported. The
#'   special value \code{"all"} tells the function to import clusters from all
#'   nodes.
#' @param sets
#'   Vector containing the names of the set of nodes to import. If \code{NULL},
#'   no is importer. The special value \code{"all"} tells the function to import all
#'   sets.
#' @param misc
#'   vector containing the name of the nodes for which you want to
#'   import misc.
#' @param  thermalAvailabilities
#'   Vector of node names for which to import thermal capacity. If \code{NULL},
#'   thermal capacity is not imported.
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
#' @param showProgress
#'   If TRUE show the function display information about the progress of the
#'   importation.
#'
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
#' nodes <- readAntares()
#' links <- readAntares(links="all")
#'
#' # Import nodes and links at same time
#' output <- readAntares(nodes = "all", links = "all")
#'
#' # Get all output for one node
#' myNode <- sample(getOption("antares")$nodeList, 1)
#' myNode
#'
#' myNodeOutput <- readAntares(node = myNode, links = getLinks(myNode),
#'                            clusters = myNode)
#'
#' @export
#'
readAntares <- function(nodes = NULL, links = NULL, clusters = NULL,
                        sets = NULL, misc = NULL,  thermalAvailabilities = NULL,
                        select = NULL,
                        synthesis = getOption("antares")$synthesis,
                        mcYears = 1:getOption("antares")$mcYears,
                        timeStep = c("hourly", "daily", "weekly", "monthly", "annual"),
                        parallel = FALSE, simplify = TRUE, showProgress = TRUE) {

  timeStep <- match.arg(timeStep)
  if (!is.list(select)) select <- list(nodes = select, links = select, sets = select)
  opts <- getOption("antares")

  # If all arguments are NULL, import all nodes
  if (is.null(nodes) & is.null(links) & is.null(clusters) & is.null(sets) & is.null(misc) & is.null( thermalAvailabilities)) nodes <- "all"

  # Manage special value "all"
  if (identical(nodes, "all")) nodes <- opts$nodeList
  if (identical(links, "all")) links <- opts$linkList
  if (identical(clusters, "all")) clusters <- opts$nodesWithClusters
  if (identical(sets, "all")) sets <- opts$setList
  if (identical(misc, "all")) misc <- opts$nodeList
  if (identical( thermalAvailabilities, "all"))  thermalAvailabilities <- opts$nodesWithClusters

  # Aliases for groups of variables
  select <- llply(select, function(x) {
    for (alias in names(pkgEnv$varAliases)) {
      if (alias %in% tolower(x)) x <- append(x, pkgEnv$varAliases[[alias]])
    }
    x
  })

  # Special aliases allNode and allLink
  select$nodes <- if(any(grepl("allNode", select$nodes, ignore.case = TRUE))) NULL else select$nodes
  select$links <- if(any(grepl("allLink", select$links, ignore.case = TRUE))) NULL else select$links

  # Can the importation be parallelized ?
  if (parallel) {
    if(!require(foreach)) stop("Parallelized importation impossible. Please install the 'foreach' package and a parallel backend provider like 'doParallel'.")
    if (!getDoParRegistered()) stop("Parallelized importation impossible. Please register a parallel backend, for instance with function 'registerDoParallel'")
  }

  res <- list() # Object the function will return

  # local function that add a type of output to the object "res"
  .addOutputToRes <- function(name, ids, outputFun, select) {
    if (is.null(ids) | length(ids) == 0) return(NULL)

    if (showProgress) cat(sprintf("Importing %s\n", name))

    tmp <- llply(ids, function(x, ...) outputFun(x, ...),
                 synthesis=synthesis, mcYears=mcYears,timeStep=timeStep,
                 opts=opts, select = select,
                 .progress = ifelse(showProgress, "text", "none"),
                 .parallel = parallel,
                 .paropts = list(.packages="antares"))
    
    tmp <- rbindlist(tmp)
    
    class(tmp) <- append("antaresTable", class(tmp))
    attr(tmp, "type") <- name
    attr(tmp, "timeStep") <- timeStep
    attr(tmp, "synthesis") <- synthesis
    
    res[[name]] <<- tmp
  }

  # Add output to res object. The ".importOutputForXXX" functions are
  # defined in the file "readOutputHelpers.R".
  .addOutputToRes("nodes", nodes, .importOutputForNode, select$nodes)
  .addOutputToRes("links", links, .importOutputForLink, select$links)
  .addOutputToRes("clusters", clusters, .importOutputForClusters, NULL)
  .addOutputToRes("sets", sets, .importOutputForNode, select$sets)
  
  # Add inputs
  .addOutputToRes("misc", misc, .importMisc, NA)
  .addOutputToRes("thermalAvailabilities", thermalAvailabilities, .importThermal, NA)
  
  # Class and attributes
  class(res) <- append("antaresOutput", class(res))
  attr(res, "timeStep") <- timeStep
  attr(res, "synthesis") <- synthesis

  # Simplify the result if possible
  if (simplify & length(res) == 1) res <- res[[1]]

  res
}

#' @export
readOutput <- readAntares

#' Read output for a list of nodes
#' 
#' This a function is a wrapper for "antaresOutput" that reads all data for a
#' list of nodes.
#' 
#' @param links
#'   should links connected to the nodes be imported ?
#' @param clusters
#'   should the clusters of the nodes be imported ?
#' @param misc
#'   should misc generation of the nodes be imported ?
#' @param thermalAvailabilities
#'   Should the thermal availabilities of the nodes be imported ?
#' @param internalLinksOnly
#'   If FALSE, all links connected to one of the nodes is imported, else
#'   only links connecting two nodes of the list are imported
#' @param ...
#'   Other arguments passed to the function \code{\link{readAntares}}
#'   
#' @return If \code{simplify = TRUE} and only one type of output is imported
#' then the result is a data.table.
#'
#' Else an object of class "antaresOutput" is returned. It is a list of
#' data.tables, each element representing one type of element (nodes, links,
#' clusters)
#'  
#' @examples 
#' \dontrun{
#' mynode <- getOption("antares")$nodeList[1]
#' data <- readAntaresNodes(mynode)
#' 
#' # Equivalent but more concise than:
#' data2 <- readAntares(mynode, links = getLinks(mynode), clusters = mynode)
#' 
#' all.equal(data, data2)
#' } 
#'  
#' @export
readAntaresNodes <- function(nodes, links=TRUE, clusters = TRUE, misc = FALSE, 
                             thermalAvailabilities = FALSE, internalLinksOnly = FALSE, ...) {
  
  links <- if (links) getLinks(nodes, internalLinksOnly) else NULL
  clusters <- if(clusters) nodes else NULL
  misc <- if(misc) nodes else NULL
  thermalAvailabilities <- if(thermalAvailabilities) nodes else NULL
  
  readAntares(nodes, links, clusters, misc, thermalAvailabilities=thermalAvailabilities, ...)
}
