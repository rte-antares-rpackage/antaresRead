.getPathsAPI <- function(host, study_id, simulation, ...){
  simNames <- NULL
  path <- paste0(host, "/v1/studies/", study_id)
  path <- gsub("[/\\]$", "", path)
  path <- paste0(path, "/raw?path=")
  inputPath <- file.path(path,  "input")
  outputPath <- file.path(path,  "output")
  if(is.null(simulation) | (!is.null(simulation) && !simulation %in% c(0, "input"))){
    outputContent <- names(read_secure_json(paste0(outputPath, "&depth=4"), ...))
    simNames <- setdiff(basename(outputContent), c("maps", "logs"))
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
  
  mc_ind_path <- file.path(simDataPath, "mc-ind&depth=1")
  
  synthesis <- .getSuccess(file.path(simDataPath, "mc-all&depth=1"), ...)
  yearByYear <- .getSuccess(mc_ind_path, ...)
  scenarios <- .getSuccess(file.path(simPath, "ts-numbers&depth=1"), ...)
  
  if (yearByYear) {
    year_no_filter <- names(read_secure_json(mc_ind_path, ...))
    mcYears <- as.numeric(year_no_filter[grep("^\\d{5}$", year_no_filter)])
  } else {
    mcYears <- numeric()
  }
  
  if (!synthesis & !yearByYear) {
    stop("No results/data found in API", call. = FALSE)
  }
  
  # List of available areas and links
  if (synthesis) {
    dataPath <- file.path(simDataPath, "mc-all")
  } else {
    dataPath <- file.path(simDataPath, "mc-ind", sprintf("%05d", mcYears[1]))
  }

  areaList <- gsub("\r$", "", tolower(
    strsplit(read_secure_json(file.path(simPath, "about-the-study", "areas"), ...), "\n")[[1]]
  )
  )
  districtList <- grep("^@", areaList, value = TRUE)
  areaList <- areaList[!areaList %in% districtList]
  
  # linkList
  links_path <- file.path(dataPath, "links&depth=2")
  links_success <- .getSuccess(links_path, ...)
  
  linkList <- character(0)
  if (links_success) {
    linkList <- .scan_output_links_folder(links_path, ...)
  }
  
  # areasWithClusters areasWithResClusters areasWithSTClusters
  areas_path <- file.path(dataPath, "areas&depth=2")
  areas_success <- .getSuccess(areas_path, ...)
  
  areasWithClusters <- character(0)
  areasWithResClusters <- character(0)
  areasWithSTClusters <- character(0)
  if (areas_success) {
    areasWithClusters <- .detect_areas_with_clusters(path = areas_path,
                                                     type = "thermal",
                                                     ...)
    areasWithResClusters <- .detect_areas_with_clusters(path = areas_path,
                                                        type = "renewables",
                                                        ...)
    areasWithSTClusters <- .detect_areas_with_clusters(path = areas_path,
                                                       type = "st-storage",
                                                       ...)
  }
  
  # variables
  variables <- list()
  areas_variables <- character(0)
  links_variables <- character(0)
  if (areas_success) {
    areas_variables <- .get_available_output_variables(path = dataPath,
                                                       type = "areas",
                                                       linkList = linkList,
                                                       areaList = areaList,
                                                       ...)
  }
  if (links_success) {  
    links_variables <- .get_available_output_variables(path = dataPath,
                                                       type = "links",
                                                       linkList = linkList,
                                                       areaList = areaList,
                                                       ...)
  }
  
  if (length(areas_variables) > 0) {
    variables[["areas"]] <- areas_variables
  }
  if (length(links_variables) > 0) {
    variables[["links"]] <- links_variables
  }
  
  # linksDef
  linksDef <- .readLinksDef(strsplit(read_secure_json(file.path(simPath, "about-the-study", "links"), ...), "\n")[[1]])

  return(
    list(
      simDataPath = simDataPath,
      name = as.character(info$name),
      mode = as.character(info$mode),
      simDate = info$date,
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
      areasWithSTClusters = intersect(areasWithSTClusters, areaList),
      variables = variables,
      parameters = params
    )
  )
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

  # info <- read_secure_json(studyPath, ...)

  antaresVersion <- paths$version
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
  areaHasResClusters <- logical(0)
  if (!is.null(params$`other preferences`$`renewable-generation-modelling`)){
    if(params$`other preferences`$`renewable-generation-modelling` == "clusters"){
      clusterResList <- read_secure_json(file.path(inputPath, "renewables", "clusters", "&depth=4"), ...)
      areaHasResClusters <- vapply(areaList, FUN.VALUE = logical(1), function(a) {
        TF <- FALSE
        try({
          f <- clusterResList[[a]]$list
          if(!is.null(f))return(TRUE)
        })
        return(TF)
      })
    }
  }
  
  # Areas with st-storage (>=860) 
  if(paths$version>=860){
    clusterSTList <- read_secure_json(file.path(inputPath, "st-storage", "clusters", "&depth=4"), ...)
    areaHasSTClusters <- vapply(areaList, FUN.VALUE = logical(1), function(a) {
      TF <- FALSE
      try({
        f <- clusterSTList[[a]]$list
        if(!is.null(f))return(TRUE)
      })
      return(TF)
    })
    
    # return
    list(
      mode = "Input",
      antaresVersion = antaresVersion,
      areaList = areaList,
      districtList = districtList,
      linkList = as.character(linksDef$link),
      linksDef = linksDef,
      areasWithClusters = areaList[areaHasClusters],
      areasWithResClusters = areaList[areaHasResClusters],
      areasWithSTClusters = areaList[areaHasSTClusters],
      parameters = params
    )
  }else
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

#' @import jsonlite
#' @export
#' @return  
#' \item{sleep}{timer for api commande execute}
#' @rdname setSimulationPath
setSimulationPathAPI <- function(host, study_id, token, simulation = NULL,
                                 timeout = 600, httr_config = list()) {

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
    # catch message from api_get() (from API)
    stop(e)
  })

  # generic tests (legacy)
  if(isTRUE(all.equal(names(check_study), "detail"))){
    stop("Can't connect to API. Please verify token")
  }

  # generic tests (legacy)
  if(!study_id %in% check_study$id){
    stop("Can't find your 'study_id' on the API")
  }

  res <- .getPathsAPI(host, 
                      study_id, 
                      simulation, 
                      token = token, 
                      timeout = timeout, 
                      config = httr_config)

  res$studyName <- check_study$name
  
  res$version <- check_study$version
  
  # If "input mode", read options from the input folder, else read them from
  # the simulation folder.
  if (is.null(res$simPath) | length(res$simPath) == 0) {
    res <- append(res, 
                  .getInputOptionsAPI(res, 
                                      token = token, 
                                      timeout = timeout, 
                                      config = httr_config))
  } else {
    res$simPath <- URLencode(res$simPath)
    res <- append(res, 
                  .getSimOptionsAPI(res, 
                                    host, 
                                    token = token, 
                                    timeout = timeout, 
                                    config = httr_config))
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
  res$modeAPI <- "sync"
  
  # delete version to keep only "antares_version"
  res$version <- NULL
  
  # timer for api commande execute
  res$sleep <- 0.5
  
  # param "verbose" similar to disk mode
  res$verbose <- FALSE

  class(res) <- c("simOptions")

  options(antares = res)

  res
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


# Detect if there is at least one output by type of cluster
.detect_areas_with_clusters <- function(path, type, ...) {
  
  assertthat::assert_that(type %in% c("thermal", "renewables", "st-storage"))
  
  pattern_type <- switch(type,
                         "thermal" = "(details-annual)|(details-daily)|(details-hourly)|(details-monthly)|(details-weekly)",
                         "renewables" = "details-res-",
                         "st-storage" = "details-STstorage-"
                         )
  
  hasClusters <- unlist(
    lapply(
      read_secure_json(path, ...),
      function(x) any(grepl(pattern = pattern_type, x = names(x)))
    )
  )
  
  return(names(hasClusters)[hasClusters])
}


# Build the link list by scanning the output folder links
.scan_output_links_folder <- function(path, ...) {
  
  linkList <- read_secure_json(path, ...)
  linkList <- mapply(function(X, Y){
    if (length(Y) >= 1) {
      paste(X, names(Y), sep = " - ")
      } else {
        NULL
      }
    }, names(linkList), linkList
  )
  linkList <- unlist(linkList)
  names(linkList) <- NULL
  
  return(linkList)
}


.get_available_output_variables <- function(path, type, linkList, areaList, ...) {
  
  variables <- character(0)
  
  lst_type <- list("areas" = list("object_name" = "area", "elements" = areaList),
                   "links" = list("object_name" = "link", "elements" = linkList)
                   )
  lst_type_target <- lst_type[[type]]
  
  target_list <- lst_type_target[["elements"]]
  
  has_items <- length(target_list) > 0
  if (!has_items) {
    return(variables)
  }
  
  if (has_items) {
    path_element <- target_list[1]
    if (type == "links") {
      path_element <- gsub(pattern = " - ", replacement = "/", x = path_element)
    }
    path_element <- URLencode(path_element, reserved = TRUE)
    d <- file.path(path, type, path_element)
    d <- URLencode(d)
    f <- names(read_secure_json(paste0(d, "&depth=1"), ...))
    f <- f[grep("values", f)]
    if (length(f) > 0) {
      v <- .getOutputHeader(file.path(d, f[1]), lst_type_target[["object_name"]], api = TRUE, ...)
      if (exists("pkgEnv")) {
        variables <- setdiff(v, pkgEnv$idVars)
      } else {
        variables <- v
      }
    }
  }
  
  return(variables)
}
