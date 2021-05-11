




.getPathsAPI <- function(path, simulation){
  simNames <- NULL
  path <- gsub("[/\\]$", "", path)
  inputPath <- file.path(path, "input")
  outputPath <- file.path(path, "output")
  if(!simulation %in% c(0, "input"))
  {
    outputContent <- unlist(lapply(jsonlite::read_json(paste0(outputPath, "?depth=2")), function(X)paste0(X$date, substr(X$type, 1, 3), "-", X$name)))
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
    inputPath <- file.path(studyPath, "input")
    
  } else {
    out <- .giv_sim(simulation, simNames, path)
    
    std_sel <- which(unlist(lapply(simNames, function(X){
      grepl(paste0(X, "$"), out$simPath)
      
    })))
    
    out$simPath <-  gsub(simNames[std_sel], std_sel, out$simPath)
    out$simOutputName <- simNames[std_sel]
    return(out)
  }
  
  
  
  
  list(studyPath = studyPath,
       simPath = simPath,
       inputPath = inputPath)
  
}


.getSimOptionsAPI <- function(paths, host){
  ###Read info from json
  simPath <- paths$simPath
  
  # Get basic information about the simulation
  params <- .readjsonAntares(file.path(simPath, "about-the-study", "parameters"))
  
  info <- read_json(file.path(simPath, "about-the-study", "study", "antares"))
  
  info$mode <- read_json(file.path(simPath, "type"))
  info$name <- read_json(file.path(simPath, "name"))
  
  # Where are located the results ?
  simDataPath <- as.character(info$mode)
  simDataPath <- file.path(simPath, simDataPath)
  
  
  
  synthesis <- .getSucess(file.path(simDataPath, "mc-all?depth=1"))
  yearByYear <-  .getSucess(file.path(simDataPath, "mc-ind?depth=1"))
  scenarios <- .getSucess(file.path(simPath, "ts-numbers?depth=1"))
  
  
  if(yearByYear) {
    year_no_filter <- names(read_json(file.path(simDataPath, "mc-ind?depth=1")))
    mcYears <- as.numeric(year_no_filter[grep("^\\d{5}$", year_no_filter)])
  }else numeric()
  
  if (!synthesis & !yearByYear) stop("No results found")
  
  # List of available areas and links
  
  
  if (synthesis) {
    dataPath <- file.path(simDataPath, "mc-all")
  } else {
    dataPath <- file.path(simDataPath, "mc-ind",sprintf("%05d", mcYears[1]) )
  }
  
  
  
  areaList <- names(read_json(file.path(dataPath, "areas?depth=1")))
  districtList <- grep("^@", areaList, value=TRUE)
  areaList <- areaList[!areaList %in% districtList]
  
  linkList <- read_json(file.path(dataPath, "links?depth=2"))
  linkList <- unlist(mapply(function(X, Y)paste(X, names(Y), sep = " - "), names(linkList), linkList))
  names(linkList) <- NULL
  
  # Areas containing clusters
  hasClusters <- unlist(lapply(read_json(file.path(dataPath, "areas?depth=2")),
                               function(X)any(grepl("details", X))))
  
  areasWithClusters <- names(hasClusters)[hasClusters]
  
  # Available variables
  variables <- list()
  
  # Available variables for areas
  d <- file.path(dataPath, "areas", areaList[1])
  f <- read_json(paste0(d, "?depth=1"))
  f <- f[grep("values", names(f))]
  if (length(f) > 0) {
    v <- .getOutputHeader(file.path(host, f[[1]]), "area")
    variables$areas <- setdiff(v, pkgEnv$idVars)
  }
  
  # Available variables for links
  
  d <- file.path(dataPath, "links", gsub(" - ", "/",linkList[1]))
  f <- read_json(paste0(d, "?depth=1"))
  f <- f[grep("values", f)]
  if (length(f) > 0) {
    v <- .getOutputHeader(file.path(host, f[1]), "link")
    variables$links <- setdiff(v, pkgEnv$idVars)
  }
  linksDef <- readLines(file.path(host, read_json(file.path(paths$simPath, "about-the-study", "links"))))
  linksDef <- .readLinksDef(linksDef)
  
  return(list(
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
    linkList = linkList,
    linksDef = linksDef,
    areasWithClusters = areasWithClusters,
    variables = variables,
    parameters = params
  ))
  
  
}


.getSucess <- function(path){
  http_status(GET(path))$category == "Success"
}


.getInputOptionsAPI <- function(paths) {
  
  studyPath <- paths$studyPath
  inputPath <- paths$inputPath
  outputPath <- paths$simPath
  
  # Lists of areas, links and districts existing in the study
  areaList <- tolower(readLines(file.path(host,read_json(file.path( inputPath, "areas", "list")))))
  
  districtList <- tolower(names(read_json(file.path( inputPath, "areas", "sets"))))
  
  areasWithLinks <-  names(read_json(paste0(file.path( inputPath, "links"), "?depth=1")))
  areasWithLinks <- intersect(areasWithLinks, areaList)
  
  
  
  
  allLinks <- read_json(paste0(file.path( inputPath, "links"), "?depth=3"))
  linksDef <- rbindlist(mapply(function(X, Y){
    to = names(X$properties)
    if (length(to) == 0) return(NULL)
    
    data.frame(link = paste(Y, "-", to), from = Y, to = to, stringsAsFactors = TRUE)
    
  }, allLinks, names(allLinks)))
  
  
  info <- read_json(file.path(studyPath))
  
  
  antaresVersion <- info$study$antares$version
  params <- .readjsonAntares(file.path(studyPath, "settings", "generaldata"))
  
  # Areas with clusters
  
  clusterList <- .readjsonAntares(file.path(inputPath, "thermal", "clusters", "?depath=4"))
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
setSimulationPathAPI <- function(path, simulation = NULL) {
  
  if (missing(path)) {
    if (exists("choose.dir", getNamespace("utils"))) {
      # choose.dir is defined only on Windows
      path <- utils::choose.dir(getwd(), "Select an Antares simulation directory")
      if (is.na(path)) stop("You have canceled the execution.")
    } else {
      stop("Please specify a path to an Antares simulation")
    }
  }
  
  
  host <- strsplit(path, "/studies/")[[1]][1]
  
  res <- .getPathsAPI(path, simulation)
  res$studyName <- .readjsonAntares(file.path(res$studyPath, "study"))$antares$caption
  
  # If "input mode", read options from the input folder, else read them from
  # the simulation folder.
  if (is.null(res$simPath)) {
    res <- append(res, .getInputOptionsAPI(res))
  } else {
    res <- append(res, .getSimOptionsAPI(res, host))
  }
  
  # dates, TimeId min and max
  tmin <- res$parameters$general$simulation.start
  tmax <- res$parameters$general$simulation.end
  
  res$timeIdMin <- 1 + (tmin - 1) * 24
  res$timeIdMax <- ((tmax - tmin + 1) %/% 7 * 7 + tmin - 1) * 24
  
  res$start <- .getStartDate(res$parameters)
  res$firstWeekday <- as.character(res$parameters$general$first.weekday)
  
  # Other informations that has to be read in input folder
  res$districtsDef <- .readDistrictsDefAPI(res$inputPath, res$areaList)
  
  
  #res$energyCosts <- .readEnergyCostsAPI(res$inputPath)
  res$typeLoad = 'api'
  class(res) <- c("simOptions")
  
  options(antares=res)
  
  res
}

# Private function that reads the definition of the districts
.readDistrictsDefAPI <- function(inputPath, areas) {
  districts <- .readjsonAntares(file.path(inputPath, "areas/sets"))
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
.readEnergyCostsAPI <- function(inputPath) {
  costs <- .readjsonAntares(file.path(inputPath, "thermal", "areas"))
  
  list(unserved  = unlist(costs$unserverdenergycost),
       spilled = unlist(costs$spilledenergycost))
}


