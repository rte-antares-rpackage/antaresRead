#Copyright © 2016 RTE Réseau de transport d’électricité

# NOTE FOR DEVELOPERS
#--------------------
#
# The function readAntares defined below does not contain the code to import
# data from an antares project. This code is located in files importOutput.R
# and importInput.R. Every function nameds ".importXXX" is located in one of
# these two files depending on what the function tries to import (input or
# output).



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
#' If parameters \code{areas}, \code{links}, \code{clusters} and \code{districts}
#' are all \code{NULL}, \code{readAntares} will read output for all areas.
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
#'   \item Filter the elements to import (parameters \code{areas},\code{links},
#'     \code{clusters} and \code{districts})
#'   \item Select only a few columns (parameter \code{select})
#'   \item read only a subset of Monte-Carlo simulations (parameter
#'     \code{mcYears}). For instance one can import a random sample of
#'     100 simulations with \code{mcYears = sample(simOptions()$mcYears, 100)}
#' }
#'
#' @section Parallelization:
#'
#' If you import several elements of the same type (areas, links, clusters), you
#' can use parallelized importation to improve performance. Setting the
#' parameter \code{parallel = TRUE} is not enough to parallelize the
#' importation, you also have to install the package
#' \href{https://CRAN.R-project.org/package=foreach}{foreach}
#' and a package that provides a parallel backend (for instance the package
#' \href{https://CRAN.R-project.org/package=doParallel}{doParallel}).
#'
#' Before running the function with argument \code{parallel=TRUE}, you need to
#' register your parallel backend. For instance, if you use package "doParallel"
#' you need to use the function \code{registerDoParallel} once per
#' session.
#'
#' @param areas
#'   Vector containing the names of the areas to import. If
#'   \code{NULL} no area is imported. The special value \code{"all"} tells the
#'   function to import all areas. By default, the value is "all" when no other argument is enter and "NULL" when other arguments are enter.
#' @param links
#'   Vector containing the name of links to import. If \code{NULL} no
#'   area is imported. The special value \code{"all"} tells the function to
#'   import all areas. Use function \code{\link{getLinks}} to import all links
#'   connected to some areas.
#' @param clusters
#'   Vector containing the name of the areas for which you want to
#'   import results at cluster level. If \code{NULL} no cluster is imported. The
#'   special value \code{"all"} tells the function to import clusters from all
#'   areas.
#' @param districts
#'   Vector containing the names of the districts to import. If \code{NULL},
#'   no district is imported. The special value \code{"all"} tells the function to import all
#'   districts.
#' @param misc
#'   Vector containing the name of the areas for which you want to
#'   import misc.
#' @param  thermalAvailabilities
#'   Should thermal availabilities of clusters be imported ? If TRUE, the column
#'   "thermalAvailability" is added to the result and a new column "availableUnits"
#'   containing the number of available units in a cluster is created.If synthesis is set to TRUE then
#'   "availableUnits" contain the mean of avaible units on all MC Years.
#' @param hydroStorage
#'   Should hydro storage be imported ?
#' @param hydroStorageMaxPower
#'   Should hydro storage maximum power be imported ?
#' @param reserve
#'   Should reserve be imported ?
#' @param linkCapacity
#'   Should link capacities be imported ?
#' @param mustRun
#'   Should must run productions be added to the result? If TRUE,
#'   then four columns are added: \code{mustRun} contains the production of 
#'   clusters that are in complete must run mode; \code{mustRunPartial}
#'   contains the partial must run production of clusters; \code{mustRunTotal}
#'   is the sum of the two previous columns. Finally \code{thermalPmin} is
#'   similar to mustRunTotal except it also takes into account the production
#'   induced by the minimum stable power of the units of a cluster. More
#'   precisely, for a given cluster and a given time step, it is equal to 
#'   \code{min(NODU x min.stable.power, mustRunTotal)}.
#' @param select
#'   Character vector containing the name of the columns to import. If this 
#'   argument is \code{NULL}, all variables are imported. Special names 
#'   \code{"allAreas"} and \code{"allLinks"} indicate to the function to import 
#'   all variables for areas or for links. Since version 1.0, values "misc", 
#'   "thermalAvailabilities", "hydroStorage", "hydroStorageMaxPower", "reserve",
#'   "linkCapacity", "mustRun", "thermalModulation" are also accepted and can
#'   replace the corresponding arguments. The list of available variables can be
#'   seen with the command \code{simOptions()$variables}.  Id variables like
#'   \code{area}, \code{link} or \code{timeId} are automatically imported.
#' @param thermalModulation
#'   Should thermal modulation time series be imported ? If \code{TRUE}, the
#'   columns "marginalCostModulation", "marketBidModulation", "capacityModulation"
#'   and "minGenModulation" are added to the cluster data.
#' @param mcYears
#'   Index of the Monte-Carlo years to import. If \code{NULL}, synthetic results
#'   are read, else the specified Monte-Carlo simulations are imported. The 
#'   special value \code{all} tells the function to import all Monte-Carlo
#'   simulations.
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
#' data.tables, each element representing one type of element (areas, links,
#' clusters)
#'
#' @seealso
#' \code{\link{setSimulationPath}}, \code{\link{getAreas}},
#' \code{\link{getLinks}}, \code{\link{getDistricts}}
#'
#' @examples
#' \dontrun{
#' # Import areas and links separately
#'
#' areas <- readAntares() # equivalent to readAntares(areas="all")
#' links <- readAntares(links="all")
#'
#' # Import areas and links at same time
#'
#' output <- readAntares(areas = "all", links = "all")
#'
#' # Add input time series to the object returned by the function
#' areas <- readAntares(areas = "all", misc = TRUE, reserve = TRUE)
#'
#' # Get all output for one area
#'
#' myArea <- sample(simOptions()$areaList, 1)
#' myArea
#'
#' myAreaOutput <- readAntares(area = myArea,
#'                             links = getLinks(myArea, regexpSelect=FALSE),
#'                             clusters = myArea)
#'
#' # Or equivalently:
#' myAreaOutput <- readAntaresAreas(myArea)
#'
#' # Use parameter "select" to read only some columns.
#'
#' areas <- readAntares(select = c("LOAD", "OV. COST"))
#'
#' # Aliases can be used to select frequent groups of columns. use showAliases()
#' # to view a list of available aliases
#'
#' areas <- readAntares(select="economy")
#'
#' }
#' @export
#'
readAntares <- function(areas = NULL, links = NULL, clusters = NULL,
                        districts = NULL, misc = FALSE, thermalAvailabilities = FALSE,
                        hydroStorage = FALSE, hydroStorageMaxPower = FALSE,
                        reserve = FALSE, linkCapacity = FALSE, mustRun = FALSE,
                        thermalModulation = FALSE,
                        select = NULL,
                        mcYears = NULL,
                        timeStep = c("hourly", "daily", "weekly", "monthly", "annual"),
                        opts = simOptions(),
                        parallel = FALSE, simplify = TRUE, showProgress = TRUE) {
  
  
  
  timeStep <- match.arg(timeStep)
  
  
  ##Controle size of data load
  size <- .giveSize(opts = opts, areas = areas, links = links, 
                    clusters = clusters, districts = districts, select = select,
                    mcYears = mcYears ,timeStep = timeStep, misc = misc, thermalAvailabilities = thermalAvailabilities,
                    hydroStorage = hydroStorage, hydroStorageMaxPower = hydroStorageMaxPower, reserve = reserve,
                    linkCapacity = linkCapacity, mustRun = mustRun, thermalModulation = thermalModulation)/1024
  
  
  if(is.null(getOption("maxSizeLoadOnComp"))){
    options(maxSizeLoadOnComp = utils::memory.limit()*0.7/1000)
    
  }
  
  if(is.null(getOption("maxSizeLoad"))){
    options(maxSizeLoad = 10)
  }
  if(getOption("maxSizeLoad")<size)
  {
    stop("You want to load more than 10Go of data,
         if you want you can modify antaresRead rules of RAM control with setRam()")
  }
  
  if(getOption("maxSizeLoadOnComp")<size)
  {
    stop("You want to load more than 70% of your computer capacity,
         if you want you can modify antaresRead rules of RAM control with setRam()")
  }
  
  
  if(!is.null(select)){
    if(!is.list(select)){
      if("all" %in% select){
        select <- c("all", select[!"all" == select])
      }
    }
  }
  if(isH5Opts(opts)){
    
    if(requireNamespace("rhdf5", versionCheck = list(op = ">=", version = "2.20.0"))){
      return(.h5ReadAntares(path = opts$h5path, 
                                        areas = areas,
                                        links = links,
                                        clusters = clusters,
                                        districts = districts,
                                        misc = misc,
                                        thermalAvailabilities = thermalAvailabilities,
                                        hydroStorage = hydroStorage,
                                        hydroStorageMaxPower = hydroStorageMaxPower,
                                        reserve = reserve,
                                        linkCapacity = linkCapacity,
                                        mustRun = mustRun,
                                        thermalModulation = thermalModulation,
                                        select = select,
                                        mcYears = mcYears,
                                        timeStep = timeStep[1],
                                        showProgress = showProgress,
                                        simplify = simplify))
    } else {
      stop(rhdf5_message)
    }
  }
  
  if (opts$mode == "Input") stop("Cannot use 'readAntares' in 'Input' mode.")
  
  reqInfos <- .giveInfoRequest(select = select,
                               areas = areas,
                               links = links,
                               clusters = clusters,
                               districts = districts,
                               mcYears = mcYears)
  
  select <- reqInfos$select
  areas <- reqInfos$areas
  links <- reqInfos$links
  clusters <- reqInfos$clusters
  districts <- reqInfos$districts
  mcYears <- reqInfos$mcYears
  synthesis <- reqInfos$synthesis
  unselect <- reqInfos$unselect
  
  if(length(reqInfos$computeAdd)>0)
  {
    for (v in reqInfos$computeAdd) {
      assign(v, TRUE)
    }
  }
  
  # If all arguments are NULL, import all areas
  if (is.null(areas) & is.null(links) & is.null(clusters) & is.null(districts)) {
    areas <- "all"
  }
  
  # Check arguments validity. The function .checkArgs is defined below
  synthesis <- is.null(mcYears)
  
  areas <- .checkArg(areas, opts$areaList, "Areas %s do not exist in the simulation.")
  links <- .checkArg(links, opts$linkList, "Links %s do not exist in the simulation.")
  clusters <- .checkArg(clusters, opts$areasWithClusters, "Areas %s do not exist in the simulation or do not have any cluster.")
  districts <- .checkArg(districts, opts$districtList, "Districts %s do not exist in the simulation.")
  mcYears <- .checkArg(mcYears, opts$mcYears, "Monte-Carlo years %s have not been exported.", allowDup = TRUE)
  
  # Special aliases allArea and allLink
  select$areas <- if(any(grepl("allArea", select$areas, ignore.case = TRUE))) NULL else select$areas
  select$links <- if(any(grepl("allLink", select$links, ignore.case = TRUE))) NULL else select$links
  
  # Check that when a user wants to import input time series, the corresponding
  # output is also imported
  if ((is.null(areas) & is.null(districts)) & (misc | hydroStorage | hydroStorageMaxPower | reserve)) {
    stop("When misc, hydroStorage, hydroStorageMaxPower or reserve is TRUE, arguments 'areas' or 'districts' need to be specified.")
  }
  if (is.null(links) & linkCapacity) stop("When 'linkCapacity' is TRUE, argument 'links' needs to be specified.")
  if (is.null(clusters) & (thermalAvailabilities | thermalModulation)) stop("When 'thermalAvailabilities' or 'thermalModulation' is TRUE, argument 'clusters' needs to be specified.")
  
  # If user asks input time series, first throw an error if monte-carlo scenarii
  # are not available. Then check if time series are available in output. If it
  # not the case, throw a warning.
  if (thermalAvailabilities) {
    if (! opts$scenarios) {
      stop("Cannot import thermal availabilities because Monte Carlo scenarii have not been stored in output.")
    }
    if (!file.exists(file.path(opts$simPath, "ts-generator/thermal/mc-0"))) {
      warning("Time series of thermal availability have not been stored in output. Time series stored in input will be used, but the result may be wrong if they have changed since the simulation has been run.")
    }
  }
  
  if (hydroStorage) {
    if (! opts$scenarios) {
      stop("Cannot import hydro storage because Monte Carlo scenarii have not been stored in output.")
    }
    if (!file.exists(file.path(opts$simPath, "ts-generator/hydro/mc-0"))) {
      warning("Time series of hydro storage have not been stored in output. Time series stored in input will be used, but the result may be wrong if they have changed since the simulation has been run.")
    }
  }
  
  if (misc | hydroStorageMaxPower | reserve | linkCapacity) {
    warning("When misc, hydroStorageMaxPower, reserve or linkCapacity is not null, 'readAntares' reads input time series. Result may be wrong if these time series have changed since the simulation has been run.")
  }
  
  # Can the importation be parallelized ?
  if (parallel) {
    if(!requireNamespace("foreach")) stop("Parallelized importation impossible. Please install the 'foreach' package and a parallel backend provider like 'doParallel'.")
    if (!foreach::getDoParRegistered()) stop("Parallelized importation impossible. Please register a parallel backend, for instance with function 'registerDoParallel'")
  }
  
  colSelect = NULL
  res <- list()
  # local function that add a type of output to the object "res"
  .addOutputToRes <- function(name, ids, outputFun, select, ts = timeStep) {
    if (is.null(ids) | length(ids) == 0) return(NULL)
    
    if (showProgress) cat(sprintf("Importing %s\n", name))
    
    tmp <- suppressWarnings(
      llply(ids, function(x, ...) outputFun(x, ...),
            synthesis=synthesis, mcYears=mcYears,timeStep=ts,
            opts=opts, select = select,
            colSelect = colSelect,
            names = names,
            unselect = unselect,
            .progress = ifelse(showProgress, "text", "none"),
            .parallel = parallel,
            .paropts = list(.packages="antaresRead"))
    )
    
    tmp <- rbindlist(tmp)
    
    res[[name]] <<- tmp
    gc() # Ensures R frees unused memory
  }
  
  # Add output to res object. The ".importOutputForXXX" functions are
  # defined in the file "importOutput.R".
  
  res$areas <- .importOutputForAreas(areas, timeStep, select$areas, mcYears, showProgress, opts,
                                     parallel = parallel)
  res$links <- .importOutputForLinks(links, timeStep, select$links, mcYears, showProgress, opts,
                                     parallel = parallel)
  res$districts <- .importOutputForDistricts(districts, timeStep, select$areas, mcYears,
                                             showProgress, opts, parallel = parallel)
  
  # Add to parameter areas the areas present in the districts the user wants
  if (!is.null(districts)) {
    districts <- opts$districtsDef[district %in% districts]
    areas <- union(areas, districts$area)
  }
  
  # Import clusters and eventually must run
  
  if (!mustRun) {
    res$clusters <- .importOutputForClusters(clusters, timeStep, NULL, mcYears,
                                             showProgress, opts, mustRun = FALSE, parallel = parallel)
  } else {
    clustersAugmented <- intersect(opts$areasWithClusters, union(areas, clusters))
    if(length(clustersAugmented) == 0)
    {
      if (!is.null(res$areas)) {
        res$areas$thermalPmin <- 0
        res$areas$mustRun <- 0
        res$areas$mustRunPartial <- 0
        res$areas$mustRunTotal <- 0
      }
    }
    else{
    res$clusters <- .importOutputForClusters(clustersAugmented, timeStep, NULL, mcYears,
                                             showProgress, opts, mustRun = TRUE, parallel = parallel)
    
    if (!is.null(res$areas)) {
      tmp <- copy(res$clusters)
      tmp[, cluster := NULL]
      tmp <- tmp[,.(thermalPmin = sum(thermalPmin),
                    mustRun = sum(mustRun),
                    mustRunPartial = sum(mustRunPartial),
                    mustRunTotal = sum(mustRunTotal)),
                 keyby = c(.idCols(tmp))]
      res$areas <- .mergeByRef(res$areas, tmp)
      
      res$areas[is.na(mustRunTotal), c("thermalPmin","mustRun", "mustRunPartial", "mustRunTotal") := 0]
    }
    
    if (!is.null(districts)) {
      if(nrow(districts ) > 0)
      {
        tmp <- copy(res$clusters)
        tmp <- merge(tmp, districts, by = "area", allow.cartesian = TRUE)
        tmp[, area := NULL]
        tmp[, cluster := NULL]
        tmp <- tmp[,.(thermalPmin = sum(thermalPmin),
                      mustRun = sum(mustRun),
                      mustRunPartial = sum(mustRunPartial),
                      mustRunTotal = sum(mustRunTotal)),
                   keyby = c(.idCols(tmp))]
        res$districts <- .mergeByRef(res$districts, tmp)
        res$districts[is.na(mustRunTotal), c("thermalPmin", "mustRun", "mustRunPartial", "mustRunTotal") := 0]
      }
    }
    
    # Suppress that has not been asked
    if (is.null(clusters)) {
      res$clusters <- NULL
    } else if (length(clustersAugmented) > length(clusters)) {
      res$clusters <- res$clusters[area %in% clusters]
    }
    
  }
  }
  
  # Add input time series
  
  if (misc) {
    .addOutputToRes("misc", areas, .importMisc)
    if (!is.null(res$areas)) .mergeByRef(res$areas, res$misc)
    if (!is.null(districts)) {
      if(nrow(districts ) > 0)
      {
        res$misc <- merge(res$misc, districts, by = "area", allow.cartesian = TRUE)
        res$misc[, area := NULL]
        res$misc <- res$misc[, lapply(.SD, sum), by = .(district, timeId)]
        .mergeByRef(res$districts, res$misc)
      }
    }
    res$misc <- NULL
  }
  
  if (thermalAvailabilities) {
    .addOutputToRes("thermalAvailabilities", clusters, .importThermal, NA)
    .mergeByRef(res$clusters, res$thermalAvailabilities)
    res$thermalAvailabilities <- NULL
  }
  
  if (hydroStorage) {
    .addOutputToRes("hydroStorage", areas, .importHydroStorage, NA)
    if (!is.null(res$areas)) .mergeByRef(res$areas, res$hydroStorage)
    
    if (!is.null(districts)) {
      if(nrow(districts ) > 0)
      {
        res$hydroStorage <- merge(res$hydroStorage, districts, by = "area", allow.cartesian = TRUE)
        res$hydroStorage[, area := NULL]
        res$hydroStorage <- res$hydroStorage[, lapply(.SD, sum), by = c(.idCols(res$hydroStorage))]
        .mergeByRef(res$districts, res$hydroStorage)
      }
      
    }
    
    res$hydroStorage <- NULL
  }
  
  if (hydroStorageMaxPower) {
    .addOutputToRes("hydroStorageMaxPower", areas, .importHydroStorageMaxPower, NA)
    if (!is.null(res$areas)) .mergeByRef(res$areas, res$hydroStorageMaxPower)
    if (!is.null(res$districts)) {
      if(nrow(districts ) > 0)
      {
        res$hydroStorageMaxPower <- merge(res$hydroStorageMaxPower, districts, by = "area", allow.cartesian = TRUE)
        res$hydroStorageMaxPower[, area := NULL]
        res$hydroStorageMaxPower <- res$hydroStorageMaxPower[, lapply(.SD, sum), by = .(district, timeId)]
        .mergeByRef(res$districts, res$hydroStorageMaxPower)
      }
    }
    res$hydroStorageMaxPower <- NULL
  }
  
  if (reserve) {
    .addOutputToRes("reserve", areas, .importReserves, NA)
    if(!is.null(res$areas)) .mergeByRef(res$areas, res$reserve)
    if (!is.null(districts)) {
      if(nrow(districts ) > 0)
      {
        res$reserve <- merge(res$reserve, districts, by = "area", allow.cartesian = TRUE)
        res$reserve[, area := NULL]
        res$reserve <- res$reserve[, lapply(.SD, sum), by = .(district, timeId)]
        .mergeByRef(res$districts, res$reserve)
      }
    }
    res$reserve <- NULL
  }
  
  if (linkCapacity) {
    .addOutputToRes("linkCapacity", links, .importLinkCapacity, NA)
    res$links <- merge(res$links, res$linkCapacity, by=c("link", "timeId"))
    res$linkCapacity <- NULL
  }
  
  if (thermalModulation) {
    .addOutputToRes("thermalModulation", union(areas, clusters), .importThermalModulation, NA)
    
    if (!is.null(res$clusters)) {
      .mergeByRef(res$clusters, res$thermalModulation)
    }
    res$thermalModulation <- NULL
  }
  
  
  #Remove no nedeed column(s)
  for(i in 1:length(unselect)){
    oldw <- getOption("warn")
    options(warn = -1)
    if("antaresDataList" %in%class(res))
    {
      for(j in 1:length(res)){
        if(!is.null(res[[j]][[names(unselect)[i]]]) & length(unselect[[i]]) > 0){
          res[[j]][[names(unselect)[i]]][, c(unselect[[i]]) := NULL]
        }}
    }else{
      if(!is.null(res[[names(unselect)[i]]]) & length(unselect[[i]]) > 0){
        res[[names(unselect)[i]]][, c(unselect[[i]]) := NULL]
      }}
    options(warn = oldw)
  }
  
  # Class and attributes
  res <- .addClassAndAttributes(res, synthesis, timeStep, opts, simplify)
  
  # Add date/time columns
  addDateTimeColumns(res)
  
  res
}



#' Function for preprocessing arguments areas, links, clusters and districts
#' of readAntares.
#'
#' @param list
#'   value of the argument areas, links, clusters or districts
#' @param reference
#'   vector containing the reference list of elements. For "areas", it is the list
#'   of areas from the simulation, etc.
#' @param msg
#'   warning message to display when an element does not exist in the reference
#'   list
#' @param allowDup
#'   If FALSE, then duplicated values in "list" are filtered.
#' @return
#' If the argument is empty it returns NULL.
#' If it contains "all", it returns the reference list
#' Else it returns the parameter "list" without the non-existent elements
#'
#' @noRd
#'
.checkArg <- function(list, reference, msg, allowDup = FALSE) {
  if (is.null(list) || length(list) == 0) return(NULL)
  if (any(list == "all")) return(reference)
  
  if (allowDup) res <- list[list %in% reference]
  else res <- intersect(list, reference)
  if (length(res) < length(list)) {
    missingElements <- setdiff(list, reference)
    warning(sprintf(msg, paste(missingElements, collapse = ", ")), call. = FALSE)
    if (length(res) == 0) return(NULL)
  }
  
  res
}



#' Read output for a list of areas
#'
#' This a function is a wrapper for "antaresData" that reads all data for a
#' list of areas.
#'
#' @param links
#'   should links connected to the areas be imported ?
#' @param clusters
#'   should the clusters of the areas be imported ?
#' @param ...
#'   Other arguments passed to the function \code{\link{readAntares}}
#' @inheritParams getLinks
#' @inheritParams readAntares
#'
#' @return If \code{simplify = TRUE} and only one type of output is imported
#' then the result is a data.table.
#'
#' Else an object of class "antaresData" is returned. It is a list of
#' data.tables, each element representing one type of element (areas, links,
#' clusters)
#'
#' @examples
#' \dontrun{
#' myarea <- simOptions()$areaList[1]
#' data <- readAntaresAreas(myarea)
#'
#' # Equivalent but more concise than:
#' data2 <- readAntares(myarea, links = getLinks(myarea), clusters = myarea)
#'
#' all.equal(data, data2)
#' }
#'
#' @export
readAntaresAreas <- function(areas, links=TRUE, clusters = TRUE, internalOnly = FALSE, opts = simOptions(), ...) {
  
  if (missing(areas)) stop("The function 'readAntaresAreas' expects a vector of area names as argument. You can use 'getAreas' to build such a vector.")
  
  links <- if (links) getLinks(areas, internalOnly=internalOnly, opts = opts) else NULL
  clusters <- if(clusters) areas else NULL
  
  readAntares(areas, links, clusters, opts = opts, ...)
  
}





#' Use to transform inputs arguments to be passable to reading function
#'
#' 
#'
#' @param select Character vector containing the name of the columns to import. See \link{readAntares} for further information.
#' @param areas Vector containing the names of the areas to import. See \link{readAntares} for further information.
#' @param links Vector containing the names of the links to import. See \link{readAntares} for further information.
#' @param clusters Vector containing the names of the clusters to import. See \link{readAntares} for further information.
#' @param districts Vector containing the names of the districts to import. See \link{readAntares} for further information.
#' @param mcYears Index of the Monte-Carlo years to import. See \link{readAntares} for further information.
#' 
#' @return \code{list}
#' \itemize{
#' \item select
#' \item areas
#' \item links
#' \item clusters
#' \item districts
#' \item mcYears
#' \item synthesis
#' \item computeAdd
#' }
#' 
#' @noRd
.giveInfoRequest <- function(select,
                             areas,
                             links,
                             clusters,
                             districts,
                             mcYears){
  
  if (!is.list(select)) select <- list(areas = select, links = select, districts = select)
  
  
  ##Get unselect columns (by - operator)
  unselect <- lapply(select, function(X){
    minusColumns <- grep("^-", X)
    if(length(minusColumns)>0)
    {
      uns <- X[minusColumns]
      gsub("^-", "", uns)
    }else{
      NULL
    }
  })
  
  ##Remove unselect columns
  select <- lapply(select, function(X){
    minusColumns <- grep("^-", X)
    if(length(minusColumns) > 0){
      X[-c(minusColumns)]
    }else{
      X
    }
  })
  
  
  # Aliases for groups of variables
  select <- llply(select, function(x) {
    for (alias in names(pkgEnv$varAliases)) {
      if (tolower(alias) %in% tolower(x)) x <- append(x, pkgEnv$varAliases[[alias]]$select)
    }
    x
  })
  
  allCompute <- pkgEnv$allCompute
  computeAdd <- allCompute[allCompute%in%unlist(select)]
  
  if ("areas" %in% unlist(select) & is.null(areas)) areas <- "all"
  if ("links" %in% unlist(select) & is.null(links)) {
    if (!is.null(areas)) links <- getLinks(getAreas(areas, regexpSelect = FALSE))
    else links <- "all"
  }
  if ("clusters" %in% unlist(select) & is.null(clusters)) {
    if (!is.null(areas)) clusters <- areas
    else clusters <- "all"
  }
  if ("mcYears" %in% unlist(select) & is.null(mcYears)) mcYears <- "all"
  
  # If all arguments are NULL, import all areas
  if (is.null(areas) & is.null(links) & is.null(clusters) & is.null(districts)) {
    areas <- "all"
  }
  
  # Check arguments validity. The function .checkArgs is defined below
  synthesis <- is.null(mcYears)
  
  return(list(select = select,
              areas = areas,
              links = links,
              clusters = clusters,
              districts = districts,
              mcYears = mcYears,
              synthesis = synthesis,
              computeAdd = computeAdd,
              unselect = unselect))
}
