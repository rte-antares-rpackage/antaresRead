#' @importFrom utils URLencode
#' @import httr
fread_antares <- function(opts, file, ...){
  if(opts$typeLoad == "api"){
    file <- gsub(".txt$", "", file)
    file <- paste0(file, "&formatted=false")
    if(!is.null(opts$token) && opts$token != ""){
      httpResponse <- GET(utils::URLencode(file), timeout(opts$timeout), 
                          add_headers(Authorization = paste0("Bearer ", opts$token)), config = opts$httr_config)
    } else {
      httpResponse <- GET(utils::URLencode(file), timeout(opts$timeout), config = opts$httr_config)
    }
    
    tryCatch({fread(content(httpResponse, "parsed"), ...)}, error = function(e) NULL)
  } else {
    fread(file, ...)
  }
}

#' @importFrom utils URLencode
#' @import httr
read_secure_json <- function(url, token = NULL, timeout = 60, config = list()){
  
  if(!is.null(token) && token != ""){
    httpResponse <- GET(utils::URLencode(url), timeout(timeout), 
                        add_headers(Authorization = paste0("Bearer ", token)), config = config)
  } else {
    httpResponse <- GET(utils::URLencode(url), timeout(timeout), config = config)
  }
  
  x <- jsonlite::fromJSON(content(httpResponse, type = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  
  recustiveTF <- function(X){
    if(is.list(X)){
      lapply(X, recustiveTF)
    }else{
      if(!is.null(X)){
        if (X == "")  return(NA)
        if (X == "true")  return(TRUE)
        if (X == "false")  return(FALSE)
        return(X)
      }
      return(X)
    }
  }
  
  recustiveTF(x)
  
}

.getPathsAPI <- function(host, study_id, simulation, ...){
  simNames <- NULL
  path <- paste0(host, "/v1/studies/", study_id)
  path <- gsub("[/\\]$", "", path)
  path <- paste0(path, "/raw?path=")
  inputPath <- file.path(path,  "input")
  outputPath <- file.path(path,  "output")
  if(is.null(simulation) | (!is.null(simulation) && !simulation %in% c(0, "input"))){
    outputContent <- names(read_secure_json(paste0(outputPath, "&depth=4"), ...))
    simNames <- setdiff(basename(outputContent), "maps")
  }
  if (length(simNames) == 0) {
    if (length(simulation) > 0 && !simulation %in% c(0, "input")) {
      stop("Cannot find any simulation result")
    } else {
      simulation <- 0
    }
  }
  
  if (is.null(simulation)) {
    if (length(simNames) == 1) { # Case 2
      simulation <- 1
    } else { # Case 3
      cat("Please, choose a simulation\n")
      for (i in 1:length(simNames)) {
        cat(sprintf("   %s - %s\n", i, simNames[i]))
      }
      simulation <- type.convert(scan(what = character(), nmax = 1), as.is = TRUE)
    }
  }
  
  if (simulation %in% c(0, "input")) {
    studyPath <- path
    simPath <- NULL
  } else {
    out <- .giv_sim(simulation, simNames, path)
    
    std_sel <- which(unlist(lapply(simNames, function(X){
      grepl(paste0(X, "$"), out$simPath)
    })))
    
    # out$simPath <-  gsub(simNames[std_sel], std_sel, out$simPath)
    out$simOutputName <- simNames[std_sel]
    return(out)
  }
  
  list(studyPath = studyPath,
       simPath = simPath,
       inputPath = inputPath)
  
}


.getSimOptionsAPI <- function(paths, host, ...){
  
  ## Read info from json
  simPath <- paths$simPath
  
  # Get basic information about the simulation
  params <- read_secure_json(file.path(simPath, "about-the-study", "parameters"), ...)
  
  info <- read_secure_json(file.path(simPath, "info", "general"), ...)
  
  # Where are located the results ?
  simDataPath <- file.path(simPath, tolower(as.character(info$mode)))
  
  synthesis <- .getSuccess(file.path(simDataPath, "mc-all&depth=1"), ...)
  yearByYear <-  .getSuccess(file.path(simDataPath, "mc-ind&depth=1"), ...)
  scenarios <- .getSuccess(file.path(simPath, "ts-numbers&depth=1"), ...)
  
  
  if(yearByYear) {
    year_no_filter <- names(read_secure_json(file.path(simDataPath, "mc-ind&depth=1"), ...))
    mcYears <- as.numeric(year_no_filter[grep("^\\d{5}$", year_no_filter)])
  } else mcYears <- numeric()
  
  if (!synthesis & !yearByYear) stop("No results/data found in API", call. = FALSE)
  
  # List of available areas and links
  if (synthesis) {
    dataPath <- file.path(simDataPath, "mc-all")
  } else {
    dataPath <- file.path(simDataPath, "mc-ind",sprintf("%05d", mcYears[1]))
  }
  
  areaList <- gsub("\r$", "", tolower(strsplit(
    read_secure_json(file.path(paths$simPath, "about-the-study", "areas"), ...), "\n")[[1]]
  ))
  districtList <- grep("^@", areaList, value=TRUE)
  areaList <- areaList[!areaList %in% districtList]
  
  linkList <- read_secure_json(file.path(dataPath, "links&depth=2"), ...)
  linkList <- unlist(mapply(function(X, Y){
    if(length(Y) >= 1){
      paste(X, names(Y), sep = " - ")
    } else {
      NULL
    }
  }, names(linkList), linkList))
  names(linkList) <- NULL
  
  # Areas containing clusters
  hasClusters <- unlist(
    lapply(
      read_secure_json(file.path(dataPath, "areas&depth=2"), ...),
      function(x) any(grepl("(details-annual)|(details-daily)|(details-hourly)|(details-monthly)|(details-weekly)", names(x)))
    )
  )
  
  areasWithClusters <- names(hasClusters)[hasClusters]
  
  # Areas containing clusters
  hasResClusters <- unlist(
    lapply(
      read_secure_json(file.path(dataPath, "areas&depth=2"), ...),
      function(x) any(grepl("details-res-", names(x)))
    )
  )
  
  areasWithResClusters <- names(hasResClusters)[hasResClusters]
  # Available variables
  variables <- list()
  
  # Available variables for areas
  d <- file.path(dataPath, "areas", areaList[1])
  f <- names(read_secure_json(paste0(d, "&depth=1"), ...))
  f <- f[grep("values", f)]
  if (length(f) > 0) {
    v <- .getOutputHeader(file.path(d, f[1]), "area", api = TRUE, ...)
    if(exists("pkgEnv")){
      variables$areas <- setdiff(v, pkgEnv$idVars)
    } else {
      variables$areas <- v
    }
  }
  
  # Available variables for links
  if(length(linkList) > 0){
    d <- file.path(dataPath, "links", gsub(" - ", "/",linkList[1]))
    f <- names(read_secure_json(paste0(d, "&depth=1"), ...))
    f <- f[grep("values", f)]
    if (length(f) > 0) {
      v <- .getOutputHeader(file.path(d, f[1]), "link", api = TRUE, ...)
      if(exists("pkgEnv")){
        variables$links <- setdiff(v, pkgEnv$idVars)
      } else {
        variables$links <- v
      }
    }
  }
  
  linksDef <- .readLinksDef(strsplit(read_secure_json(file.path(paths$simPath, "about-the-study", "links"), ...), "\n")[[1]])
  
  return(
    list(
      simDataPath = simDataPath,
      name = as.character(info$name),
      mode = as.character(info$mode),
      synthesis = synthesis,
      yearByYear = yearByYear,
      scenarios = scenarios,
      mcYears = mcYears,
      antaresVersion = info$version,
      areaList = areaList,
      districtList = gsub("^@ ?", "", districtList),
      linkList = linkList[linkList %in% linksDef$link],
      linksDef = linksDef,
      areasWithClusters = intersect(areasWithClusters, areaList),
      areasWithResClusters = intersect(areasWithResClusters, areaList),
      variables = variables,
      parameters = params
    )
  )
}


.getSuccess <- function(path, token, timeout = 60, config = list()){
  if(!is.null(token) && token != ""){
    http_status(
      GET(
        path, timeout(timeout), 
        add_headers(Authorization = paste0("Bearer ", token)), config = config)
    )$category == "Success"
  } else {
    http_status(GET(path, timeout(timeout), config = config))$category == "Success"
  }
}


.getInputOptionsAPI <- function(paths, ...) {
  
  studyPath <- paths$studyPath
  inputPath <- paths$inputPath
  outputPath <- paths$simPath
  
  # Lists of areas, links and districts existing in the study
  areaList <- unique(
    tolower(unlist(read_secure_json(file.path(inputPath, "areas", "list"), ...)))
  )
  
  districtList <- unique(
    tolower(names(read_secure_json(file.path(inputPath, "areas", "sets"), ...)))
  )
  
  areasWithLinks <-   unique(names(read_secure_json(file.path(inputPath, "links&depth=1"), ...)))
  areasWithLinks <- intersect(areasWithLinks, areaList)
  
  allLinks <- read_secure_json(file.path(inputPath, "links&depth=3"), ...)
  linksDef <- data.table::rbindlist(mapply(function(X, Y){
    to = names(X$properties)
    if (length(to) == 0) return(NULL)
    
    data.frame(link = paste(Y, "-", to), from = Y, to = to, stringsAsFactors = TRUE)
    
  }, allLinks, names(allLinks)))
  
  
  info <- read_secure_json(studyPath, ...)
  
  antaresVersion <- info$study$antares$version
  params <- read_secure_json(file.path(studyPath, "settings", "generaldata"), ...)
  
  # Areas with clusters
  
  clusterList <- read_secure_json(file.path(inputPath, "thermal", "clusters", "&depth=4"), ...)
  areaHasClusters <- vapply(areaList, FUN.VALUE = logical(1), function(a) {
    TF <- FALSE
    try({
      f <- clusterList[[a]]$list
      if(!is.null(f))return(TRUE)
    })
    return(TF)
  })
  
  # Areas with renewable clusters
  clusterResList <- read_secure_json(file.path(inputPath, "renewables", "clusters", "&depth=4"), ...)
  areaHasResClusters <- vapply(areaList, FUN.VALUE = logical(1), function(a) {
    TF <- FALSE
    try({
      f <- clusterResList[[a]]$list
      if(!is.null(f))return(TRUE)
    })
    return(TF)
  })
  
  list(
    mode = "Input",
    antaresVersion = antaresVersion,
    areaList = areaList,
    districtList = districtList,
    linkList = as.character(linksDef$link),
    linksDef = linksDef,
    areasWithClusters = areaList[areaHasClusters],
    areasWithResClusters = areaList[areaHasResClusters],
    parameters = params
  )
}

# valid_url <- function(url_in, t = 2){
#   con <- url(url_in)
#   check <- suppressWarnings(try(open.connection(con, open = "rt",timeout = t), silent = T)[1])
#   suppressWarnings(try(close.connection(con), silent = T))
#   ifelse(is.null(check),TRUE ,FALSE )
# }

#' @import httr jsonlite
#' @export
#' @rdname setSimulationPath
setSimulationPathAPI <- function(host, study_id, token, simulation = NULL, 
                                 timeout = 60, httr_config = list()) {
  
  if (missing(host)) {
    stop("Please specify an url to antares API host")
  }
  
  if (missing(study_id)) {
    stop("Please specify the study_id")
  }
  
  if (missing(token)) {
    stop("Please specify your access token")
  }
  
  valid_host <- tryCatch({
    .getSuccess(file.path(host, "health"), token = "", timeout = timeout, config = httr_config)
  }, error = function(e) FALSE)
  
  if(!valid_host){
    stop("setSimulationPathAPI : invalid host '", host, "'")
  }
  
  stopifnot(timeout > 0)
  
  check_study <- tryCatch({
    read_secure_json(file.path(host, "v1/studies", study_id), token = token, 
                     timeout = timeout, config = httr_config
    )
  }, error = function(e){
    stop("Can't connect to API. Please verify host & token")
  })
  
  if(isTRUE(all.equal(names(check_study), "detail"))){
    stop("Can't connect to API. Please verify token")
  }
  
  if(!study_id %in% check_study$id){
    stop("Can't find your 'study_id' on the API")
  }
  
  res <- .getPathsAPI(host, study_id, simulation, token = token, timeout = timeout, config = httr_config)
  
  res$studyName <- read_secure_json(file.path(res$studyPath, "study"), token = token, timeout = timeout, config = httr_config)$antares$caption
  
  # If "input mode", read options from the input folder, else read them from
  # the simulation folder.
  if (is.null(res$simPath)) {
    res <- append(res, .getInputOptionsAPI(res, token = token, timeout = timeout, config = httr_config))
  } else {
    res <- append(res, .getSimOptionsAPI(res, host, token = token, timeout = timeout, config = httr_config))
  }
  
  # dates, TimeId min and max
  tmin <- res$parameters$general$simulation.start
  tmax <- res$parameters$general$simulation.end
  
  res$timeIdMin <- 1 + (tmin - 1) * 24
  res$timeIdMax <- ((tmax - tmin + 1) %/% 7 * 7 + tmin - 1) * 24
  
  res$start <- .getStartDate(res$parameters)
  res$firstWeekday <- as.character(res$parameters$general$first.weekday)
  
  # Other informations that has to be read in input folder
  res$districtsDef <- .readDistrictsDefAPI(res$inputPath, res$areaList, token, timeout)
  
  
  res$energyCosts <- .readEnergyCostsAPI(res$inputPath, token, timeout)
  
  res$typeLoad <- "api"
  res$host <- host
  res$study_id <- study_id
  res$token <- token
  res$timeout <- timeout
  res$httr_config <- httr_config
  
  class(res) <- c("simOptions")
  
  options(antares = res)
  
  res
}

#' Change API Timeout
#'
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{\link{setSimulationPathAPI}}
#' @param timeout \code{numeric} API timeout (seconds). Default to 60.
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' opts <- setTimeoutAPI(opts, timeout = 45)
#' }
#' 
setTimeoutAPI <- function(opts, timeout){
  if(opts$typeLoad == 'api'){
    opts$timeout <- timeout
  } else {
    warning("setTimeoutAPI can only be use for API Simulation")
  }
  return(opts)
}

# Private function that reads the definition of the districts
.readDistrictsDefAPI <- function(inputPath, areas, token = NULL, timeout = 60) {
  districts <- read_secure_json(file.path(inputPath, "areas/sets"), token = token, timeout = timeout)
  if (length(districts) == 0) return(NULL)
  
  res <- ldply(names(districts), function(n) {
    x <- districts[[n]]
    if (any(unlist(x) == "add-all")) {
      areasToRemove <- unlist(x[names(x) == "-"], use.names = FALSE)
      areas <- setdiff(areas, areasToRemove)
    } else {
      areas <- unlist(x[names(x) == "+"], use.names = FALSE)
    }
    if (length(areas) == 0) return(NULL)
    
    data.frame(district = tolower(n), area = tolower(areas), stringsAsFactors = TRUE)
  })
  
  data.table(res)
}



# Private function that reads costs of unsuplied and spilled energy
.readEnergyCostsAPI <- function(inputPath, token = NULL, timeout = 60) {
  
  costs <- read_secure_json(file.path(inputPath, "thermal", "areas"), token = token, timeout = timeout)
  
  list(
    unserved  = unlist(costs$unserverdenergycost),
    spilled = unlist(costs$spilledenergycost)
  )
}
