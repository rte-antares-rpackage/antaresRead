#' Read the data of an Antares simulation
#'
#' @description 
#' \code{readAntares} is a swiss-army-knife function used to read almost every 
#' possible time series of an antares Project at any desired time resolution 
#' (hourly, daily, weekly, monthly or annual). 
#' 
#' It was first designed to read 
#' output time series, but it can also read input time series. The input time
#' series are processed by the function to fit the query of the user (timeStep,
#' synthetic results or Monte-Carlo simulation, etc.). The few data that are not
#' read by \code{readAntares} can generally by read with other functions of the
#' package starting with "read" (\code{\link{readClusterDesc}},
#' \code{\link{readLayout}}, \code{\link{readBindingConstraints}})
#' 
#' @details 
#' If parameters \code{nodes}, \code{links}, \code{clusters} and \code{districts}
#' are all \code{NULL}, \code{readAntares} will read output for all nodes.
#' By default the function reads synthetic results if they are available.
#' 
#' \code{readAntares} is able to read input time series, but when they are not 
#' stored in output, these time series may have changed since a simulation has
#' been run. In such a case the function will remind you this danger with a
#' warning.
#' 
#' When individual Monte-Carlo simulations are read, the function may crash
#' because of insufficient memory. In such a case, it is necessary to reduce
#' size of the output. Different strategies are available depending on your
#' objective:
#' 
#' \itemize{
#'   \item Use a larger time step (parameter \code{timeStep})
#'   \item Filter the elements to import (parameters \code{nodes},\code{links},
#'     \code{clusters} and \code{districts})
#'   \item Select only a few columns (parameter \code{select})
#'   \item read only a subset of Monte-Carlo simulations (parameter 
#'     \code{mcYears}). For instance one can import a random sample of
#'     100 simulations with \code{mcYears = sample(simOptions()$mcYears, 100)}
#' }
#' 
#' @section Parallelization:
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
#' @param nodes
#'   Vector containing the names of the nodes to import. If
#'   \code{NULL} no node is imported. The special value \code{"all"} tells the
#'   function to import all nodes. By default, the value is "all" when no other argument is enter and "NULL" when other arguments are enter. 
#' @param links
#'   Vector containing the name of links to import. If \code{NULL} no
#'   node is imported. The special value \code{"all"} tells the function to
#'   import all nodes. Use function \code{\link{getLinks}} to import all links
#'   connected to some nodes.
#' @param clusters
#'   Vector containing the name of the nodes for which you want to
#'   import results at cluster level. If \code{NULL} no cluster is imported. The
#'   special value \code{"all"} tells the function to import clusters from all
#'   nodes.
#' @param districts
#'   Vector containing the names of the districts to import. If \code{NULL},
#'   no district is imported. The special value \code{"all"} tells the function to import all
#'   districts.
#' @param misc
#'   Vector containing the name of the nodes for which you want to
#'   import misc.
#' @param  thermalAvailabilities
#'   Should thermal availabilities of clusters be imported ?
#' @param hydroStorage
#'   Should hydro storage be imported ?
#' @param hydroStorageMaxPoser
#'   Should hydro storage maximum power be imported ? 
#' @param reserve
#'   Should reserve be imported ?
#' @param linkCapacity
#'   Should link capacities be imported ?
#' @param mustRun
#'   Should mustRun and partialMustRun columns be added to the result ?
#' @param select
#'   Character vector containing the name of the columns to import. If this
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
#' @param opts
#'   list of simulation parameters returned by the function 
#'   \code{\link{setSimulationPath}}
#' @param parallel
#'   Should the importation be parallelized ? (See details)
#' @param simplify
#'   If TRUE and only one type of output is imported then a
#'   data.table is returned. If FALSE, the result will always be a list of class
#'   "antaresData".
#' @param showProgress
#'   If TRUE the function displays information about the progress of the
#'   importation.
#'
#'
#' @return If \code{simplify = TRUE} and only one type of output is imported
#' then the result is a data.table.
#'
#' Else an object of class "antaresDataList" is returned. It is a list of
#' data.tables, each element representing one type of element (nodes, links,
#' clusters)
#' 
#' @seealso 
#' \code{\link{setSimulationPath}}, \code{\link{getNodes}},
#' \code{\link{getLinks}}, \code{\link{getDistricts}}
#'
#' @examples
#' \dontrun{
#' # Import nodes and links separately
#' 
#' nodes <- readAntares() # equivalent to readAntares(nodes="all")
#' links <- readAntares(links="all")
#'
#' # Import nodes and links at same time
#' 
#' output <- readAntares(nodes = "all", links = "all")
#' 
#' # Add input time series to the object returned by the function
#' nodes <- readAntares(nodes = "all", misc = TRUE, reserve = TRUE)
#'
#' # Get all output for one node
#' 
#' myNode <- sample(simOptions()$nodeList, 1)
#' myNode
#'
#' myNodeOutput <- readAntares(node = myNode, 
#'                             links = getLinks(myNode, regexpSelect=FALSE),
#'                             clusters = myNode)
#'                            
#' # Or equivalently:
#' myNodeOutput <- readAntaresNodes(myNode)
#' 
#' # Use parameter "select" to read only some columns.
#' 
#' nodes <- readAntares(select = c("LOAD", "OV. COST"))
#' 
#' # Aliases can be used to select frequent groups of columns. use showAliases()
#' # to view a list of available aliases
#' 
#' nodes <- readAntares(select="economy")
#' 
#' }
#' @export
#'
readAntares <- function(nodes = NULL, links = NULL, clusters = NULL,
                        districts = NULL, misc = FALSE, thermalAvailabilities = FALSE,
                        hydroStorage = FALSE, hydroStorageMaxPower = FALSE,
                        reserve = FALSE, linkCapacity = FALSE, mustRun = FALSE,
                        select = NULL,
                        synthesis = simOptions()$synthesis,
                        mcYears = 1:simOptions()$mcYears,
                        timeStep = c("hourly", "daily", "weekly", "monthly", "annual"),
                        opts = simOptions(),
                        parallel = FALSE, simplify = TRUE, showProgress = TRUE) {

  if (opts$mode == "Input") stop("Cannot use 'readAntares' in 'Input' mode.")
  
  timeStep <- match.arg(timeStep)
  if (!is.list(select)) select <- list(nodes = select, links = select, districts = select)
  
  # If all arguments are NULL, import all nodes
  if (is.null(nodes) & is.null(links) & is.null(clusters) & is.null(districts)) {
    nodes <- "all"
  }
  
  # Check that when a user wants to import input time series, the corresponding
  # output is also imported
  if (is.null(nodes) & (misc | hydroStorage | hydroStorageMaxPower | reserve)) {
    stop("When misc, hydroStorage, hydroStorageMaxPower or reserve is TRUE, argument 'nodes' needs to be specified.")
  }
  if (is.null(links) & linkCapacity) stop("When 'linkCapacity' is TRUE, argument 'links' needs to be specified.")
  if (is.null(clusters) & thermalAvailabilities) stop("When 'thermalAvailabilities' is TRUE, argument 'clusters' needs to be specified.")
  
  # If user asks input time series, first throw an error if monte-carlo scenarii
  # are not available. Then check if time series are available in output. If it
  # not the case, throw a warning.
  if (thermalAvailabilities) {
    if (! opts$scenarios) {
      stop("Cannot import thermal availabilities because Monte Carlo scenarii have not been stored in output.")
    }
    if (!file.exists(file.path(opts$path, "ts-generator/thermal/mc-0"))) {
      warning("Time series of thermal availability have not been stored in output. Time series stored in input will be used, but the result may be wrong if they have changed since the simulation has been run.")
    }
  }
  
  if (hydroStorage) {
    if (! opts$scenarios) {
      stop("Cannot import hydro storage because Monte Carlo scenarii have not been stored in output.")
    }
    if (!file.exists(file.path(opts$path, "ts-generator/hydro/mc-0"))) {
      warning("Time series of hydro storage have not been stored in output. Time series stored in input will be used, but the result may be wrong if they have changed since the simulation has been run.")
    }
  }
  
  if (misc | hydroStorageMaxPower | reserve | linkCapacity) {
    warning("When misc, hydroStorageMaxPower, reserve or linkCapacity is not null, 'readAntares' reads input time series. Result may be wrong if these time series have changed since the simulation has been run.")
  }

  # Manage special value "all"
  if (identical(nodes, "all")) nodes <- opts$nodeList
  if (identical(links, "all")) links <- opts$linkList
  if (identical(clusters, "all")) clusters <- opts$nodesWithClusters
  if (identical(districts, "all")) districts <- opts$setList

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
  .addOutputToRes <- function(name, ids, outputFun, select, ts = timeStep) {
    if (is.null(ids) | length(ids) == 0) return(NULL)

    if (showProgress) cat(sprintf("Importing %s\n", name))

    tmp <- llply(ids, function(x, ...) outputFun(x, ...),
                 synthesis=synthesis, mcYears=mcYears,timeStep=ts,
                 opts=opts, select = select,
                 .progress = ifelse(showProgress, "text", "none"),
                 .parallel = parallel,
                 .paropts = list(.packages="antares"))
    
    tmp <- rbindlist(tmp)
    
    res[[name]] <<- tmp
    gc() # Ensures R frees unused memory
  }

  # Add output to res object. The ".importOutputForXXX" functions are
  # defined in the file "readOutputHelpers.R".
  .addOutputToRes("nodes", nodes, .importOutputForNode, select$nodes)
  .addOutputToRes("links", links, .importOutputForLink, select$links)
  .addOutputToRes("clusters", clusters, .importOutputForClusters, NULL)
  .addOutputToRes("districts", districts, .importOutputForNode, select$districts)
  
  # Add inputs
  if (misc) {
    .addOutputToRes("misc", nodes, .importMisc, NA)
    res$nodes <- merge(res$nodes, res$misc, by=c("node", "timeId"))
    res$misc <- NULL
  }
  if (thermalAvailabilities) {
    .addOutputToRes("thermalAvailabilities", clusters, .importThermal, NA)
    
    by <- c("node", "cluster", "timeId")
    if (!synthesis) by <- append(by, "mcYear")
    
    res$clusters <- merge(res$clusters, res$thermalAvailabilities, by = by)
    res$thermalAvailabilities <- NULL
  }
  if (hydroStorage) {
    .addOutputToRes("hydroStorage", nodes, .importHydroStorage, NA)
    by <- c("node", "timeId")
    if (!synthesis) by <- append(by, "mcYear")
    res$nodes <- merge(res$nodes, res$hydroStorage, by = by)
    res$hydroStorage <- NULL
  }
  if (hydroStorageMaxPower) {
    .addOutputToRes("hydroStorageMaxPower", nodes, .importHydroStorageMaxPower, NA)
    res$nodes <- merge(res$nodes, res$hydroStorageMaxPower, by=c("node", "timeId"))
    res$hydroStorageMaxPower <- NULL
  }
  
  if (reserve) {
    .addOutputToRes("reserve", nodes, .importReserves, NA)
    res$nodes <- merge(res$nodes, res$reserve, by=c("node", "timeId"))
    res$reserve <- NULL
  }

  if (linkCapacity) {
    .addOutputToRes("linkCapacity", links, .importLinkCapacity, NA)
    res$links <- merge(res$links, res$linkCapacity, by=c("link", "timeId"))
    res$linkCapacity <- NULL
  }
  
  # construct mustRun and mustRunPartial columns
  if (mustRun) {
    if (is.null(res$clusters) | timeStep != "hourly") {
      local({
        timeStep <- "hourly"
        nodes <- intersect(opts$nodesWithClusters, union(nodes, clusters))
        .addOutputToRes("mustRun", nodes, .importOutputForClusters, NULL, "hourly")
      })
    } else res$mustRun <- res$clusters
    
    clusterDesc <- readClusterDesc(opts)
    if (is.null(clusterDesc$must.run)) clusterDesc$must.run <- FALSE
    else clusterDesc[is.na(must.run), must.run := FALSE]
    
    clusterDesc <- clusterDesc[, .(node, cluster,
                                   capacity = nominalcapacity * unitcount,
                                   must.run)]
    
    .addOutputToRes("mustRunModulation", union(nodes, clusters), .importMustRunModulation, NULL)
    
    res$mustRun <- merge(res$mustRun, clusterDesc, by = c("node", "cluster"))
    if (nrow(res$mustRunModulation) > 0) {
      res$mustRun <- merge(res$mustRun, res$mustRunModulation, 
                           by = c("node","cluster", "timeId"), all.x = TRUE)
    } else res$mustRun$mustRunModulation <- NA_real_
    
    
    res$mustRun$mustRun <- res$mustRun[, capacity * must.run ]
    res$mustRun$mustRunPartial <- 0
    res$mustRun[!is.na(mustRunModulation), 
                c("mustRun", "mustRunPartial") := list(0, capacity * mustRunModulation)]
    
    res$mustRun[mustRun > production, mustRun := as.numeric(production)]
    res$mustRun[mustRunPartial > production, mustRunPartial := as.numeric(production)]
    
    res$mustRun$mustRunTotal <- res$mustRun$mustRun + res$mustRun$mustRunPartial
    
    res$mustRun <- res$mustRun[, .(node, cluster, timeId, mustRun, mustRunPartial, mustRunTotal)]
    
    res$mustRun <- changeTimeStep(res$mustRun, timeStep, "hourly", opts = opts)
    
    if (!is.null(res$clusters)) {
      res$clusters <- merge(res$clusters, res$mustRun, by = c("node", "cluster", "timeId"))
    }
    
    if (!is.null(res$nodes)) {
      res$mustRun <- res$mustRun[,.(mustRun = sum(mustRun), 
                                    mustRunPartial = sum(mustRunPartial),
                                    mustRunTotal = sum(mustRunTotal)), 
                                 keyby = .(node, timeId)]
      res$nodes <- merge(res$nodes, res$mustRun, by = c("node", "timeId"), all.x = TRUE)
      
      res$nodes[is.na(mustRunTotal), c("mustRun", "mustRunPartial", "mustRunTotal") := 0]
      
    }
    
    res$mustRun <- NULL
    res$mustRunModulation <- NULL
  }
  
  # Class and attributes
  .addClassAndAttributes(res, synthesis, timeStep, opts, simplify)
}

#' Read output for a list of nodes
#' 
#' This a function is a wrapper for "antaresData" that reads all data for a
#' list of nodes.
#' 
#' @param links
#'   should links connected to the nodes be imported ?
#' @param clusters
#'   should the clusters of the nodes be imported ?
#' @param ...
#'   Other arguments passed to the function \code{\link{readAntares}}
#' @inheritParams getLinks
#' @inheritParams readAntares
#'   
#' @return If \code{simplify = TRUE} and only one type of output is imported
#' then the result is a data.table.
#'
#' Else an object of class "antaresData" is returned. It is a list of
#' data.tables, each element representing one type of element (nodes, links,
#' clusters)
#'  
#' @examples 
#' \dontrun{
#' mynode <- simOptions()$nodeList[1]
#' data <- readAntaresNodes(mynode)
#' 
#' # Equivalent but more concise than:
#' data2 <- readAntares(mynode, links = getLinks(mynode), clusters = mynode)
#' 
#' all.equal(data, data2)
#' } 
#'  
#' @export
readAntaresNodes <- function(nodes, links=TRUE, clusters = TRUE, internalOnly = FALSE, opts = simOptions(), ...) {
  
  if (missing(nodes)) stop("The function 'readAntaresNodes' expects a vector of node names as argument. You can use 'getNodes' to build such a vector.")
  
  if (is.null(opts)) {
    message("Please choose a directory containing an Antares simulation")
    opts <- setSimulationPath()
  }
  
  links <- if (links) getLinks(nodes, regexpSelect = FALSE, internalOnly=internalOnly, opts = opts) else NULL
  clusters <- if(clusters) nodes else NULL
  
  readAntares(nodes, links, clusters, opts = opts, ...)
  
}
