#' @importFrom utils URLencode
#' @import httr
fread_antares <- function(opts, file, ...){
  if(opts$typeLoad == "api"){
    file <- gsub(".txt$", "", file)
    file <- paste0(file, "&formatted=false")
    httpResponse <- GET(utils::URLencode(file), add_headers(Authorization = paste0("Bearer ", opts$token)))
    tryCatch({fread(content(httpResponse, "parsed"), ...)}, error = function(e) NULL)
  } else {
    fread(file, ...)
  }
}

#' @importFrom utils URLencode
#' @import httr
read_secure_json <- function(url, token){
  httpResponse <- GET(utils::URLencode(url), add_headers(Authorization = paste0("Bearer ", token)))
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

.getPathsAPI <- function(path, simulation, token){
  simNames <- NULL
  path <- gsub("[/\\]$", "", path)
  path <- gsub("/studies/", "/v1/studies/", path, fixed = T)
  path <- paste0(path, "/raw?path=")
  inputPath <- file.path(path,  "input")
  outputPath <- file.path(path,  "output")
  if(is.null(simulation) | (!is.null(simulation) && !simulation %in% c(0, "input"))){
    outputContent <- names(read_secure_json(paste0(outputPath, "&depth=4"), token = token))
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


.getSimOptionsAPI <- function(paths, host, token){
  
  ## Read info from json
  simPath <- paths$simPath
  
  # Get basic information about the simulation
  params <- read_secure_json(file.path(simPath, "about-the-study", "parameters"), token)
  
  info <- read_secure_json(file.path(simPath, "info", "general"), token)
  
  # Where are located the results ?
  simDataPath <- file.path(simPath, tolower(as.character(info$mode)))
  
  synthesis <- .getSuccess(file.path(simDataPath, "mc-all&depth=1"), token)
  yearByYear <-  .getSuccess(file.path(simDataPath, "mc-ind&depth=1"), token)
  scenarios <- .getSuccess(file.path(simPath, "ts-numbers&depth=1"), token)
  
  
  if(yearByYear) {
    year_no_filter <- names(read_secure_json(file.path(simDataPath, "mc-ind&depth=1"), token))
    mcYears <- as.numeric(year_no_filter[grep("^\\d{5}$", year_no_filter)])
  }else mcYears <- numeric()
  
  if (!synthesis & !yearByYear) stop("No results/data found in API", call. = FALSE)
  
  # List of available areas and links
  if (synthesis) {
    dataPath <- file.path(simDataPath, "mc-all")
  } else {
    dataPath <- file.path(simDataPath, "mc-ind",sprintf("%05d", mcYears[1]))
  }
  
  areaList <- tolower(names(read_secure_json(file.path(dataPath, "areas&depth=1"), token)))
  districtList <- grep("^@", areaList, value=TRUE)
  areaList <- areaList[!areaList %in% districtList]
  
  linkList <- read_secure_json(file.path(dataPath, "links&depth=2"), token)
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
      read_secure_json(file.path(dataPath, "areas&depth=2"), token),
      function(x) any(grepl("details", names(x)))
    )
  )
  
  areasWithClusters <- names(hasClusters)[hasClusters]
  
  # Available variables
  variables <- list()
  
  # Available variables for areas
  d <- file.path(dataPath, "areas", areaList[1])
  f <- names(read_secure_json(paste0(d, "&depth=1"), token))
  f <- f[grep("values", f)]
  if (length(f) > 0) {
    v <- .getOutputHeader(file.path(d, f[1]), "area", api = TRUE, token = token)
    if(exists("pkgEnv")){
      variables$areas <- setdiff(v, pkgEnv$idVars)
    } else {
      variables$areas <- v
    }
  }
  
  # Available variables for links
  
  d <- file.path(dataPath, "links", gsub(" - ", "/",linkList[1]))
  f <- names(read_secure_json(paste0(d, "&depth=1"), token))
  f <- f[grep("values", f)]
  if (length(f) > 0) {
    v <- .getOutputHeader(file.path(d, f[1]), "link", api = TRUE, token = token)
    if(exists("pkgEnv")){
      variables$links <- setdiff(v, pkgEnv$idVars)
    } else {
      variables$links <- v
    }
  }
  
  linksDef <- .readLinksDef(strsplit(read_secure_json(file.path(paths$simPath, "about-the-study", "links"), token), "\n")[[1]])
  
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
    areasWithClusters = areasWithClusters,
    variables = variables,
    parameters = params
  )
  )
  
  
}


.getSuccess <- function(path, token){
  http_status(GET(path, add_headers(Authorization = paste0("Bearer ", token))))$category == "Success"
}


.getInputOptionsAPI <- function(paths, token) {
  
  studyPath <- paths$studyPath
  inputPath <- paths$inputPath
  outputPath <- paths$simPath
  
  # Lists of areas, links and districts existing in the study
  areaList <- unique(
    tolower(unlist(read_secure_json(file.path(inputPath, "areas", "list"), token)))
  )
  
  districtList <- unique(
    tolower(names(read_secure_json(file.path(inputPath, "areas", "sets"), token)))
  )
  
  areasWithLinks <-   unique(names(read_secure_json(file.path(inputPath, "links&depth=1"), token)))
  areasWithLinks <- intersect(areasWithLinks, areaList)
  
  allLinks <- read_secure_json(file.path(inputPath, "links&depth=3"), token)
  linksDef <- data.table::rbindlist(mapply(function(X, Y){
    to = names(X$properties)
    if (length(to) == 0) return(NULL)
    
    data.frame(link = paste(Y, "-", to), from = Y, to = to, stringsAsFactors = TRUE)
    
  }, allLinks, names(allLinks)))
  
  
  info <- read_secure_json(studyPath, token)
  
  antaresVersion <- info$study$antares$version
  params <- read_secure_json(file.path(studyPath, "settings", "generaldata"), token)
  
  # Areas with clusters
  
  clusterList <- read_secure_json(file.path(inputPath, "thermal", "clusters", "&depth=4"), token)
  areaHasClusters <- vapply(areaList, FUN.VALUE = logical(1), function(a) {
    TF <- FALSE
    try({
      f <- clusterList[[a]]$list
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
    parameters = params
  )
}


#' Set simualtion path for API
#'
#' @description
#' \code{setSimulationPathAPI} see setSimulationPath function 
#'
#'
#' @param path \code{character} see setSimulationPath help
#' @param simulation \code{character, numeric} see setSimulationPath help
#' 
#' @import httr jsonlite
#' @export
setSimulationPathAPI <- function(path, token, simulation = NULL) {
  
  if (missing(path)) {
    stop("Please specify an url to antares API")
  }
  
  host <- strsplit(path, "/studies/")[[1]][1]
  
  res <- .getPathsAPI(path, simulation, token)
  res$studyName <- read_secure_json(file.path(res$studyPath, "study"), token = token)$antares$caption
  
  # If "input mode", read options from the input folder, else read them from
  # the simulation folder.
  if (is.null(res$simPath)) {
    res <- append(res, .getInputOptionsAPI(res, token))
  } else {
    res <- append(res, .getSimOptionsAPI(res, host, token))
  }
  
  # dates, TimeId min and max
  tmin <- res$parameters$general$simulation.start
  tmax <- res$parameters$general$simulation.end
  
  res$timeIdMin <- 1 + (tmin - 1) * 24
  res$timeIdMax <- ((tmax - tmin + 1) %/% 7 * 7 + tmin - 1) * 24
  
  res$start <- .getStartDate(res$parameters)
  res$firstWeekday <- as.character(res$parameters$general$first.weekday)
  
  # Other informations that has to be read in input folder
  res$districtsDef <- .readDistrictsDefAPI(res$inputPath, res$areaList, token)
  
  
  #res$energyCosts <- .readEnergyCostsAPI(res$inputPath, token)
  res$typeLoad <- 'api'
  res$token <- token
  
  class(res) <- c("simOptions")
  
  options(antares = res)
  
  res
}

# Private function that reads the definition of the districts
.readDistrictsDefAPI <- function(inputPath, areas, token) {
  districts <- read_secure_json(file.path(inputPath, "areas/sets"), token)
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
.readEnergyCostsAPI <- function(inputPath, token) {
  
  costs <- read_secure_json(file.path(inputPath, "thermal", "areas"), token = token)
  
  list(
    unserved  = unlist(costs$unserverdenergycost),
    spilled = unlist(costs$spilledenergycost)
  )
}
