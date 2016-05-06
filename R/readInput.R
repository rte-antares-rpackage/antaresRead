#' Read Input time series
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
