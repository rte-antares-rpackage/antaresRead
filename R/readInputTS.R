#Copyright © 2016 RTE Réseau de transport d’électricité

#' Read Input time series
#' 
#' @description 
#' \code{readInputTS} is a function that reads time series from an antares 
#' project. But contrary to \code{\link{readAntares}}, it only reads time series
#' stored in the input folder, so it can work in "input" mode. 
#' 
#' @param load
#'   vector of areas names for which load time series must be read.
#' @param thermalAvailabilities
#'   vector of areas names for which thermal availabilities of clusters must be read.
#' @param ror
#'   vector of areas names for which run of river time series must be read.
#' @param hydroStorage
#'   vector of areas names for which hydrolic storage time series must be read.
#' @param hydroStorageMaxPower
#'   vector of areas names for which hydrolic storage maximum power time series must be read.
#' @param wind
#'   vector of areas names for which wind time series must be read
#' @param solar
#'   vector of areas names for which solar time series must be read
#' @param misc
#'   vector of areas names for which misc time series must be read
#' @param reserve
#'   vector of areas names for which reserve time series must be read
#' @param linkCapacity
#'   vector of links names for which links characteristics time series must be read
#' @inheritParams readAntares
#' 
#' @return 
#' If \code{simplify = TRUE} and only one type of input is imported
#' then the result is a data.table with class "antaresDataTable".
#' 
#' Else an object of class "antaresDataList" is returned. It is a list of
#' data.tables, each element representing one type of element (load, wind,
#' solar, etc.).
#' 
#' @note 
#' All parameters expecting a vector of areas or links names also accept the
#' special value "all". It indicates the function to read the desired time 
#' series for all areas or links.
#' 
#' @seealso 
#' \code{\link{setSimulationPath}}, \code{\link{readAntares}}, 
#' \code{\link{getAreas}}, \code{\link{getLinks}}
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
#' myArea <- readInputTS(load= "myArea", timeStep = "monthly")
#' 
#' # Quick plot to visualize the variability of the series
#' matplot(myArea[, - (1:2), with = FALSE], type = "l")
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
    if(!requireNamespace("foreach")) stop("Parallelized importation impossible. Please install the 'foreach' package and a parallel backend provider like 'doParallel'.")
    if (!foreach::getDoParRegistered()) stop("Parallelized importation impossible. Please register a parallel backend, for instance with function 'registerDoParallel'")
  }
  
  # Manage special value "all"
  if(identical(load, "all")) load <- opts$areaList
  if(identical(thermalAvailabilities, "all")) thermalAvailabilities <- opts$areasWithClusters
  if(identical(ror, "all")) ror <- opts$areaList
  if(identical(hydroStorage, "all")) hydroStorage <- opts$areaList
  if(identical(hydroStorageMaxPower, "all")) hydroStorageMaxPower <- opts$areaList
  if(identical(wind, "all")) wind <- opts$areaList
  if(identical(solar, "all")) solar <- opts$areaList
  if(identical(misc, "all")) misc <- opts$areaList
  if(identical(reserve, "all")) reserve <- opts$areaList
  if(identical(linkCapacity, "all")) linkCapacity <- opts$linkList
  
  res <- list() # Object the function will return
  
  # local function that add a type of output to the object "res"
  .addOutputToRes <- function(name, ids, outputFun, ts = timeStep) {
    if (is.null(ids) | length(ids) == 0) return(NULL)
    if (showProgress) cat(sprintf("Importing %s\n", name))
    
    tmp <- llply(ids, function(x, ...) outputFun(x, ...),
                 timeStep=ts, opts=opts,
                 .progress = ifelse(showProgress, "text", "none"),
                 .parallel = parallel,
                 .paropts = list(.packages="antaresRead"))
    
    tmp <- rbindlist(tmp)
    
    if (nrow(tmp) == 0) warning("Could not find '", name, "' time series.")
    
    res[[name]] <<- tmp
    gc() # Ensures R frees unused memory
  }
  
  # Add input to res object. The ".import****" functions are
  # defined in the file "importInput.R"
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
  
  if (length(res) == 0) stop("At least one argument of readInputTS has to be defined.")
  
  # Class and attributes
  res <- .addClassAndAttributes(res, NULL, timeStep, opts, simplify)
  addDateTimeColumns(res)
}
