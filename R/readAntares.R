#' Read the data of an Antares simulation
#'
#' This function reads the data of a antares simulation for a set of nodes,
#' links and/or clusters at a desired resolution (hourly, daily, weekly,
#' monthly, annual). It can import synthetic results or Monte-Carlo simulations.
#'
#' @aliases readOutput
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
#'   Should reserve be imported
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
#' Else an object of class "antaresData" is returned. It is a list of
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
                        districts = NULL, misc = FALSE, thermalAvailabilities = FALSE,
                        hydroStorage = FALSE, hydroStorageMaxPower = FALSE,
                        reserve = FALSE, linkCapacity = FALSE, mustRun = FALSE,
                        select = NULL,
                        synthesis = getOption("antares")$synthesis,
                        mcYears = 1:getOption("antares")$mcYears,
                        timeStep = c("hourly", "daily", "weekly", "monthly", "annual"),
                        opts = simOptions(),
                        parallel = FALSE, simplify = TRUE, showProgress = TRUE) {

  timeStep <- match.arg(timeStep)
  if (!is.list(select)) select <- list(nodes = select, links = select, districts = select)

  if (is.null(opts)) {
    message("Please choose a directory containing an Antares simulation")
    opts <- setSimulationPath()
  }
  
  # If all arguments are NULL, import all nodes
  if (is.null(nodes) & is.null(links) & is.null(clusters) & is.null(districts)) {
    nodes <- "all"
  }
  
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
    
    .addOutputToRes("mustRunModulation", nodes, .importMustRunModulation, NULL)
    
    res$mustRun <- merge(res$mustRun, clusterDesc, by = c("node", "cluster"))
    res$mustRun <- merge(res$mustRun, res$mustRunModulation, 
                         by = c("node","cluster", "timeId"), all.x = TRUE)
    
    res$mustRun$mustRun <- res$mustRun[, capacity * must.run ]
    res$mustRun$mustRunPartial <- 0
    res$mustRun[!is.na(mustRunModulation), 
                c("mustRun", "mustRunPartial") := list(0, mustRun * mustRunModulation)]
    
    res$mustRun[mustRun > MWh, mustRun := as.numeric(MWh)]
    res$mustRun[mustRunPartial > MWh, mustRunPartial := as.numeric(MWh)]
    
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
  for (n in names(res)) {
    class(res[[n]]) <- append("antaresTable", class(res[[n]]))
    attr(res[[n]], "type") <- n
    attr(res[[n]], "timeStep") <- timeStep
    attr(res[[n]], "synthesis") <- synthesis
    attr(res[[n]], "opts") <- opts
  }
  
  class(res) <- append("antaresData", class(res))
  attr(res, "timeStep") <- timeStep
  attr(res, "synthesis") <- synthesis
  attr(res, "opts") <- opts

  # Simplify the result if possible
  if (simplify & length(res) == 1) res <- res[[1]]

  res
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
#' @param misc
#'   should misc generation of the nodes be imported ?
#' @param thermalAvailabilities
#'   Should the thermal availabilities of the nodes be imported ?
#' @param hydroStorage
#'   Should hydro storage of the nodes be imported ?
#' @param hydroStorageMaxPower
#'   Should the max power of hydro storage of the nodes be imported ?
#' @param reserve
#'   Should the reserve of the nodes be imported ?
#' @param linkCapacity
#'   Should the capacity of the links connected to the nodes be imported ?
#' @param ...
#'   Other arguments passed to the function \code{\link{readAntares}}
#' @inheritParams getLinks
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
                             thermalAvailabilities = FALSE, 
                             hydroStorage = FALSE,
                             hydroStorageMaxPower = FALSE,
                             reserve = FALSE,
                             linkCapacity = FALSE,
                             internalOnly = FALSE, ...) {
  
  links <- if (links) getLinks(nodes, internalOnly) else NULL
  clusters <- if(clusters) nodes else NULL
  misc <- if(misc) nodes else NULL
  thermalAvailabilities <- if(thermalAvailabilities) nodes else NULL
  hydroStorage <- if(hydroStorage) nodes else NULL
  hydroStorageMaxPower <- if(hydroStorageMaxPower) nodes else NULL
  reserve <- if(reserve) nodes else NULL
  linkCapacity <- if (linkCapacity) getLinks(nodes, internalOnly) else NULL
  
  readAntares(nodes, links, clusters, misc, thermalAvailabilities=thermalAvailabilities,
              hydroStorage=hydroStorage, hydroStorageMaxPower=hydroStorageMaxPower, 
              reserve=reserve, linkCapacity = linkCapacity, ...)
  
}
