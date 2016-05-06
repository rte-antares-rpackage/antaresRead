#' Read Input time series
#' 
#' @description 
#' \code{readInputTS} is a function that reads time series from an antares 
#' project. But contrary to \code{\link{readAntares}}, it only reads time series
#' stored in the input folder, so it can work in "input" mode. 
#' 
#' @param load
#'   vector of nodes names for which load time series must be read.
#' @param thermalAvailabilities
#'   vector of nodes names for which thermal availabilities of clusters must be read.
#' @param ror
#'   vector of nodes names for which run of river time series must be read.
#' @param hydroStorage
#'   vector of nodes names for which hydrolic storage time series must be read.
#' @param hydroStorageMaxPower
#'   vector of nodes names for which hydrolic storage maximum power time series must be read.
#' @param wind
#'   vector of nodes names for which wind time series must be read
#' @param solar
#'   vector of nodes names for which solar time series must be read
#' @param misc
#'   vector of nodes names for which misc time series must be read
#' @param reserve
#'   vector of nodes names for which reserve time series must be read
#' @param linkCapacity
#'   vector of links names for which links characteristics time series must be read
#' @inheritParams readAntares
#' 
#' @return 
#' If \code{simplify = TRUE} and only one type of inpuàt is imported
#' then the result is a data.table with class "antaresDataTable".
#' 
#' Else an object of class "antaresDataList" is returned. It is a list of
#' data.tables, each element representing one type of element (load, wind,
#' solar, etc.).
#' 
#' @note 
#' All parameters expecting a vector of nodes or links names also accept the
#' special value "all". It indicates the function to read the desired time 
#' series for all nodes or links.
#' 
#' @seealso 
#' \code{\link{setSimulationPath}}, \code{\link{readAntares}}, 
#' \code{\link{getNodes}}, \code{\link{getLinks}}
#' 
#' @examples 
#' \dontrun{
#' # Set an antares study in "input" mode. This is useful when one want to
#' # inspect input time series before running a simulation.
#' # Note that readAntares do not function in input mode, but readInputTS
#' # works with any mode.
#' 
#' setSimulationPath("path_to_the_study", "input")
#' 
#' # Read load time series
#' readInputTS(load = "all")
#' 
#' # Read hydrolic storage and maximum power in the same call:
#' readInputTS(hydroStorage = "all", hydroStorageMaxPower = "all")
#' 
#' # Use a different time step
#' myNode <- readInputTS(load= "myNode", timeStep = "monthly")
#' 
#' # Quick plot to visualize the variability of the series
#' matplot(myNode[, - (1:2), with = FALSE], type = "l")
#' }
#' 
#' @export
readInputTS <- function(load = NULL, thermalAvailabilities = NULL, ror = NULL, 
                        hydroStorage = NULL, hydroStorageMaxPower = NULL, 
                        wind = NULL, solar = NULL, misc = NULL,
                        reserve = NULL, linkCapacity = NULL, opts = simOptions(),
                        timeStep = c("hourly", "daily", "weekly", "monthly", "annual"),
                        simplify = TRUE, parallel = FALSE,
                        showProgress = TRUE) {
  
  timeStep <- match.arg(timeStep)
  
  # Can the importation be parallelized ?
  if (parallel) {
    if(!require(foreach)) stop("Parallelized importation impossible. Please install the 'foreach' package and a parallel backend provider like 'doParallel'.")
    if (!getDoParRegistered()) stop("Parallelized importation impossible. Please register a parallel backend, for instance with function 'registerDoParallel'")
  }
  
  # Manage special value "all"
  if(identical(load, "all")) load <- opts$nodeList
  if(identical(thermalAvailabilities, "all")) thermalAvailabilities <- opts$nodesWithClusters
  if(identical(ror, "all")) ror <- opts$nodeList
  if(identical(hydroStorage, "all")) hydroStorage <- opts$nodeList
  if(identical(hydroStorageMaxPower, "all")) hydroStorageMaxPower <- opts$nodeList
  if(identical(wind, "all")) wind <- opts$nodeList
  if(identical(solar, "all")) solar <- opts$nodeList
  if(identical(misc, "all")) misc <- opts$nodeList
  if(identical(reserve, "all")) reserve <- opts$nodeList
  if(identical(linkCapacity, "all")) linkCapacity <- opts$linkList
  
  res <- list() # Object the function will return
  
  # local function that add a type of output to the object "res"
  .addOutputToRes <- function(name, ids, outputFun, ts = timeStep) {
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
  
  # Add input to res object. The ".import****" functions are
  # defined in the file "readInputHelpers.R"
  .addOutputToRes("load", load, .importLoad)
  .addOutputToRes("thermalAvailabilities", thermalAvailabilities, .importThermalAvailabilities)
  .addOutputToRes("ror", ror, .importROR)
  .addOutputToRes("hydroStorage", hydroStorage, .importHydroStorageInput)
  .addOutputToRes("hydroStorageMaxPower", hydroStorageMaxPower, .importHydroStorageMaxPower)
  .addOutputToRes("wind", wind, .importWind)
  .addOutputToRes("solar", solar, .importSolar)
  .addOutputToRes("misc", misc, .importMisc)
  .addOutputToRes("reserve", reserve, .importReserves)
  .addOutputToRes("linkCapacity", linkCapacity, .importLinkCapacity)
  
  # Class and attributes
  .addClassAndAttributes(res, NULL, timeStep, opts, simplify)
}
