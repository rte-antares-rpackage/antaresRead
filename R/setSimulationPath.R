#Copyright © 2016 RTE Réseau de transport d’électricité

#' Set Path to an Antares simulation
#'
#' This function has to be used before the \code{read} functions. It sets the path to
#' the Antares simulation to work on and other useful options (list of areas,
#' links, areas with clusters, variables, etc.). On local disk with \code{setSimulationPath} or
#' on an AntaREST API with \code{setSimulationPathAPI}
#' 
#' @details 
#' The simulation chosen with \code{setSimulationPath} or \code{setSimulationPathAPI} becomes the default
#' simulation for all functions of the package. This behavior is fine when
#' working on only one simulation, but it may become problematic when working
#' on multiple simulations at same time.
#' 
#' In such case, you can store the object returned by the function in a variable
#' and pass this variable to the functions of the package (see examples).
#'
#' @param path (optional) 
#'   Path to the simulation. It can either be the path to a directory containing
#'   an antares project or directly to the directory containing the output of a
#'   simulation.  If missing, a window opens and lets the user choose the
#'   directory of the simulation interactively. Can also choose .h5 file, if \code{rhdf5} is installed.
#' @param simulation
#'   (optional) Only used if "path" represents the path of a study and not of the
#'   output of a simulation. It can be either the name of the simulation or a
#'   number indicating which simulation to use. It is possible to use negative 
#'   values to select a simulation from the last one: for instance -1 will
#'   select the most recent simulation, -2 will the penultimate one, etc. There 
#'   are two special values 0 and "input" that tells the function that the user
#'   is not interested by the results of any simulation, but only by the inputs.
#'   In such a case, the function \code{\link{readAntares}} is unavailable. 
#'
#' @param host \code{character} host of AntaREST server API
#' @param study_id \code{character} id of the target study on the API
#' @param token \code{character} API personnal access token
#' @param timeout \code{numeric} API timeout (seconds). Default to 60. See also \code{\link{setTimeoutAPI}}
#' @param httr_config API httr configuration. See \code{\link[httr]{config}}
#' 
#' @return A list containing various information about the simulation, in particular:
#'   \item{studyPath}{path of the Antares study}
#'   \item{simPath}{path of the simulation}
#'   \item{inputPath}{path of the input folder of the study}
#'   \item{studyName}{Name of the study}
#'   \item{simDataPath}{path of the folder containing the data of the simulation}
#'   \item{name}{name of the simulation}
#'   \item{mode}{type of simulation: economy, adequacy, draft or input}
#'   \item{synthesis}{Are synthetic results available ?}
#'   \item{yearByYear}{Are the results for each Monte Carlo simulation available ?}
#'   \item{scenarios}{Are the Monte-Carlo scenarii stored in output ? This is 
#'     important to reconstruct some input time series that have been used in 
#'     each Monte-Carlo simulation.}
#'   \item{mcYears}{Vector containing the number of the exported Monte-Carlo scenarios}
#'   \item{antaresVersion}{Version of Antares used to run the simulation.}
#'   \item{areaList}{Vector of the available areas.}
#'   \item{districtList}{Vector of the available districts.}
#'   \item{linkList}{Vector of the available links.}
#'   \item{areasWithClusters}{Vector of areas containing clusters.}
#'   \item{areasWithResClusters}{Vector of areas containing clusters renewable.}
#'   \item{areasWithSTClusters}{Vector of areas containing clusters storage (>=v8.6.0).}
#'   \item{variables}{Available variables for areas, districts and links.}
#'   \item{parameters}{Other parameters of the simulation.}
#'   \item{binding}{Table of time series dimensions for each group (>=v8.7.0).}
#'   \item{timeIdMin}{
#'     Minimum time id of the simulation. It is generally equal to one but can
#'     be higher if working on a subperiod.
#'   }
#'   \item{timeIdMax}{maximum time id of the simulation.}
#'   \item{start}{
#'     Date of the first day of the year in the simulation. This date corresponds
#'     to timeId = 1.
#'   }
#'   \item{firstWeekday}{First day of the week.}
#'   \item{districtsDef}{data.table containing the specification of the districts.}
#'   \item{energyCosts}{list containing the cost of spilled and unsupplied energy.}
#'   \item{verbose}{`logical` default to FALSE, put to TRUE to manage diagnostic messages}
#' 
#' @seealso 
#'   \code{\link{simOptions}}, \code{\link{readAntares}}, \code{\link{readLayout}}, 
#'   \code{\link{readClusterDesc}}, \code{\link{readBindingConstraints}}
#'
#' @examples
#' 
#' \dontrun{
#' # Select interactively a study. It only works on windows.
#' 
#' setSimulationPath()
#' 
#' # Specify path of the study. Note: if there are more than one simulation
#' # output in the study, the function will asks the user to interactively choose
#' # one simulation.
#' 
#' setSimulationPath("path_of_the_folder_of_the_study")
#' 
#' # Select the first simulation of a study
#' 
#' setSimulationPath("path_of_the_folder_of_the_study", 1)
#' 
#' # Select the last simulation of a study
#' 
#' setSimulationPath("path_of_the_folder_of_the_study", -1)
#' 
#' # Select a simulation by name
#' 
#' setSimulationPath("path_of_the_folder_of_the_study", "name of the simulation")
#' 
#' # Just need to read input data
#' 
#' setSimulationPath("path_of_the_folder_of_the_study", "input")
#' # or
#' setSimulationPath("path_of_the_folder_of_the_study", 0)
#' 
#' # Working with API
#' #--------------------------
#' setSimulationPathAPI(
#'     host = "http://antares_api_adress", 
#'     study_id = "study_id_on_api", 
#'     token = "token"
#' )
#' 
#' ## Custom httr options ?
#' 
#' # global using httr package
#' require(httr)
#' set_config(verbose())
#' setSimulationPathAPI(
#'     host = "http://antares_api_adress", 
#'     study_id = "study_id_on_api", 
#'     token = "token"
#' )
#' 
#' reset_config()
#' 
#' # or in setSimulationPathAPI
#' setSimulationPathAPI(
#'     host = "http://antares_api_adress", 
#'     study_id = "study_id_on_api", 
#'     token = "token",
#'     httr_config = config(verbose = TRUE)
#' )
#' 
#' # disable ssl certificate checking ?
#' setSimulationPathAPI(
#'     host = "http://antares_api_adress", 
#'     study_id = "study_id_on_api", 
#'     token = "token",
#'     httr_config = config(ssl_verifypeer = FALSE)
#' )
#'
#' # WORKING WITH MULTIPLE SIMULATIONS
#' #----------------------------------
#' # Let us assume ten simulations have been run and we want to collect the
#' # variable "LOAD" for each area. We can create a list containing options
#' # for each simulation and iterate through this list.
#' 
#' opts <- lapply(1:10, function(i) {
#'    setSimulationPath("path_of_the_folder_of_the_study", i)
#' })
#' 
#' output <- lapply(opts, function(o) {
#'   res <- readAntares(areas = "all", select = "LOAD", timeStep = "monthly", opts = o)
#'   # Add a column "simulation" containing the name of the simulation
#'   res$simulation <- o$name
#'   res
#' })
#' 
#' # Concatenate all the tables in one super table
#' output <- rbindlist(output)
#' 
#' # Reshape output for easier comparisons: one line per timeId and one column
#' # per simulation
#' output <- dcast(output, timeId + areaId ~ simulation, value.var = "LOAD")
#' 
#' output
#' 
#' # Quick visualization
#' matplot(output[area == area[1], !c("area", "timeId"), with = FALSE], 
#'         type = "l")
#' }
#' 
#' @export
#'
#' @rdname setSimulationPath
setSimulationPath <- function(path, simulation = NULL) {
 
  if (missing(path)) {
    if (exists("choose.dir", getNamespace("utils"))) {
      # choose.dir is defined only on Windows
      path <- utils::choose.dir("./", "Select an Antares simulation directory")
      if (is.na(path)) stop("You have canceled the execution.")
    } else {
      stop("Please specify a path to an Antares simulation")
    }
  }
  
  # # Get study, simulation and input paths
  res <- .getPaths(path, simulation)
  
  res$studyName <- readIniFile(file.path(res$studyPath, "study.antares"))$antares$caption
  
  # If "input mode", read options from the input folder, else read them from
  # the simulation folder.
  if (is.null(res$simPath)) {
    res <- append(res, .getInputOptions(res))
    if(res$antaresVersion>=870)
      res <- append(res, .getDimBCGroups(res))
  } else {
    res <- append(res, .getSimOptions(res))
  }
  
  # dates, TimeId min and max
  tmin <- res$parameters$general$simulation.start
  tmax <- res$parameters$general$simulation.end
  
  res$timeIdMin <- 1 + (tmin - 1) * 24
  res$timeIdMax <- ((tmax - tmin + 1) %/% 7 * 7 + tmin - 1) * 24
  
  res$start <- .getStartDate(res$parameters)
  res$firstWeekday <- as.character(res$parameters$general$first.weekday)
  
  # Other informations that has to be read in input folder
  res$districtsDef <- .readDistrictsDef(res$inputPath, res$areaList)
  res$energyCosts <- .readEnergyCosts(res$inputPath)
  res$typeLoad <- "txt"
  res$verbose <- FALSE
  class(res) <- c("simOptions")

  options(antares=res)

  res
}

.giv_sim <- function(simulation, simNames, path){
  if (is.numeric(simulation)) {
    if(abs(simulation)>length(simNames))
      stop(paste0("Parameter 'simulation' is greater than 
                  the number of simulations (", length(simNames), ")"), call. = FALSE)
    if (simulation > 0) 
      sim <- simNames[simulation]
    else 
      sim <- rev(simNames)[abs(simulation)]
  } else {
    if (any(simNames == simulation)) sim <- simulation
    else {
      sim <- simNames[grep(paste0("(^\\d{8}-\\d{4})?", simulation, "$"), simNames, ignore.case = TRUE)]
      if (length(sim) == 0) stop ("Cannot find the simulation called ", simulation)
      if (length(sim) > 1) warning("Several simulations have the same name. The most recent will be used")
      sim <- last(sim)
    }
  }
  
  studyPath <- path
  simPath <- file.path(path, "output", sim[1])
  inputPath <- file.path(studyPath, "input")
  return(list(studyPath = studyPath,
              simPath = simPath,
              inputPath = inputPath))
}

# Private function that extracts study, simulation and input paths from the
# path specified by the user.
.getPaths <- function(path, simulation) {
  # check path must be length 1
  if(length(path)!=1)
    stop("Only one path is required", 
         call. = FALSE)
  
  path <- gsub("[/\\]$", "", path)
  path <- normalizePath(path, winslash = "/")
  
  # Check that the path is an Antares simulation
  if (file.exists(file.path(path, "info.antares-output"))) {
    
    studyPath <- normalizePath(file.path(path, "../.."), winslash = "/")
    simPath <- path
    inputPath <- file.path(studyPath, "input")
    
  } else {
    
    # The path is not a simulation but maybe it is a whole Antares study.
    # In this case, there are three scenarii:
    # - 1. use the value of parameter "simulation" if it is set
    # - 2. there is only one study in the output. Select it
    # - 3. asks the user to interactively choose one simulation
    
    if (!file.exists(file.path(path, "study.antares")))
      stop("Directory is not an Antares study.")
      
    outputPath <- file.path(path, "output")
    
    outputContent <- list.dirs(outputPath, recursive = FALSE)
    simNames <- setdiff(basename(outputContent), "maps")
    
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
      return(.giv_sim(simulation, simNames, path))
    }
  }
  
  list(studyPath = studyPath,
       simPath = simPath,
       inputPath = inputPath)
}

# Read simulation options from the input folder. 
# This function is called when the user only wants to read data in the input
# folder.
.getInputOptions <- function(paths) {
  studyPath <- paths$studyPath
  inputPath <- paths$inputPath
  
  # Lists of areas, links and districts existing in the study
  areaList <- tolower(readLines(file.path(inputPath, "areas/list.txt")))
  
  districtList <- tolower(names(readIniFile(file.path(inputPath, "areas/sets.ini"))))
  
  areasWithLinks <-list.files(file.path(inputPath, "links"))
  areasWithLinks <- intersect(areasWithLinks, areaList)
  linksDef <- ldply(areasWithLinks, function(f) {
    if (!dir.exists(file.path(inputPath, "links", f))) return(NULL)
    to <- names(readIniFile(file.path(inputPath, "links", f, "properties.ini")))
    
    if (length(to) == 0) return(NULL)
    
    data.frame(link = paste(f, "-", to), from = f, to = to, stringsAsFactors = TRUE)
  })
  linksDef <- data.table(linksDef)
  
  antaresVersion <- readIniFile(file.path(studyPath, "study.antares"))$antares$version
  # Convert the Antares number version (from 9.0) 
  if(antaresVersion<600 & antaresVersion>=9)
    antaresVersion <- .transform_antares_version(antares_version = antaresVersion)
  
  params <- readIniFile(file.path(studyPath, "settings/generaldata.ini"))
  
  # Areas with thermal clusters
  areaHasClusters <- vapply(areaList, FUN.VALUE = logical(1), function(a) {
    f <- file.path(inputPath, "thermal/clusters", a, "list.ini")
    file.exists(f) && file.info(f)$size > 0
  })
  
  # Areas with renewable clusters
  areaHasResClusters <- vapply(areaList, FUN.VALUE = logical(1), function(a) {
    f <- file.path(inputPath, "renewables/clusters", a, "list.ini")
    file.exists(f) && file.info(f)$size > 0
  })
  
  # Areas with st-storatge clusters
  areaHasSTClusters <- vapply(areaList, FUN.VALUE = logical(1), function(a) {
    f <- file.path(inputPath, "st-storage/clusters", a, "list.ini")
    file.exists(f) && file.info(f)$size > 0
  })
  
  res <- list(
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

  res
}

# Read simulation options from the simulation folder.
.getSimOptions <- function(paths) {
  
  simPath <- paths$simPath
  
  # Get basic information about the simulation
  info <- readIniFile(file.path(simPath, "info.antares-output"))$general
  params <- readIniFile(file.path(simPath, "about-the-study/parameters.ini"))
  
  # Where are located the results ?
  simDataPath <- switch(as.character(info$mode),
                        "draft" = "adequacy-draft",
                        "Economy" = "economy",
                        "Adequacy" = "adequacy",
                        "Expansion" = "economy")
  simDataPath <- file.path(simPath, simDataPath)
  
  # Which results are available ? Synthesis ? Monte-Carlo years ?
  synthesis <- file.exists(file.path(simDataPath, "mc-all"))
  yearByYear <- file.exists(file.path(simDataPath, "mc-ind"))
  scenarios <- file.exists(file.path(simPath, "ts-numbers"))
  
  mcYears <- if(yearByYear) as.numeric(list.files(file.path(simDataPath, "mc-ind"), pattern = "^\\d{5}$"))
  else numeric()
  
  if (!synthesis & !yearByYear) stop("No results found")
  
  dataPath_mc_all <- file.path(simDataPath, "mc-all")
  dataPath_mc_ind <- list.files(file.path(simDataPath, "mc-ind"), pattern = "^\\d{5}$", 
                                full.names = TRUE)[1]
  
  if (file.exists(file.path(simDataPath, "mc-all"))) dataPath <- dataPath_mc_all else
    dataPath <- dataPath_mc_ind
  
  # List of available areas and links
  areaList_mc_all <- list.files(file.path(dataPath_mc_all, "areas"))
  areaList_mc_ind <- list.files(file.path(dataPath_mc_ind, "areas"))
  areaList <- sort(union(areaList_mc_all, areaList_mc_ind))
  
  districtList <- areaList[areaList %like% "^@"]
  areaList <- areaList[!areaList %like% "^@"]
  
  linkList_mc_all <- list.files(file.path(dataPath_mc_all, "links"))
  linkList_mc_ind <- list.files(file.path(dataPath_mc_ind, "links"))
  linkList <- sort(union(linkList_mc_all, linkList_mc_ind))
  
  # Areas containing thermal clusters
  hasClusters_mc_all <- laply(file.path(dataPath_mc_all, "areas", areaList_mc_all), function(x) {
    f <- list.files(x)
    any(grepl("(details-annual)|(details-daily)|(details-hourly)|(details-monthly)|(details-weekly)", f))
  })
  hasClusters_mc_ind <- laply(file.path(dataPath_mc_ind, "areas", areaList_mc_ind), function(x) {
    f <- list.files(x)
    any(grepl("(details-annual)|(details-daily)|(details-hourly)|(details-monthly)|(details-weekly)", f))
  })
  
  areasWithClusters <- sort(union(areaList_mc_all[hasClusters_mc_all], areaList_mc_ind[hasClusters_mc_ind]))
  
  # Areas containing renewable clusters
  hasResClusters_mc_all <- laply(file.path(dataPath_mc_all, "areas", areaList_mc_all), function(x) {
    f <- list.files(x)
    any(grepl("details-res-", f))
  })
  hasResClusters_mc_ind <- laply(file.path(dataPath_mc_ind, "areas", areaList_mc_ind), function(x) {
    f <- list.files(x)
    any(grepl("details-res-", f))
  })
  
  areasWithResClusters <- sort(union(areaList_mc_all[hasResClusters_mc_all], 
                                     areaList_mc_ind[hasResClusters_mc_ind]))
  
  
  # Areas containing short-term clusters
  hasSTClusters_mc_all <- laply(file.path(dataPath_mc_all, "areas", areaList_mc_all), function(x) {
    f <- list.files(x)
    any(grepl("details-STstorage-", f))
  })
  hasSTClusters_mc_ind <- laply(file.path(dataPath_mc_ind, "areas", areaList_mc_ind), function(x) {
    f <- list.files(x)
    any(grepl("details-STstorage-", f))
  })
  
  areasWithSTClusters <- sort(union(areaList_mc_all[hasSTClusters_mc_all], 
                                     areaList_mc_ind[hasSTClusters_mc_ind]))
  
  # Available variables
  variables <- list()
  
  # Available variables for areas
  if (dataPath == dataPath_mc_all) d <- file.path(dataPath_mc_all, "areas", areaList_mc_all[1]) else 
    d <- file.path(dataPath_mc_ind, "areas", areaList_mc_ind[1])
  
  f <- list.files(d)
  f <- f[grep("values", f)]
  if (length(f) > 0) {
    v <- .getOutputHeader(file.path(d, f[1]), "area")
    variables$areas <- setdiff(v, pkgEnv$idVars)
  }
  
  # Available variables for links
  d <- file.path(dataPath, "links", linkList[1])
  f <- list.files(d)
  f <- f[grep("values", f)]
  if (length(f) > 0) {
    v <- .getOutputHeader(file.path(d, f[1]), "link")
    variables$links <- setdiff(v, pkgEnv$idVars)
  }
  
  lines <- readLines(file.path(simPath, "about-the-study/links.txt"))
  linksDef <- .readLinksDef(lines)
  
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
    linkList = linkList,
    linksDef = linksDef,
    areasWithClusters = areasWithClusters,
    areasWithResClusters = areasWithResClusters,
    areasWithSTClusters = areasWithSTClusters,
    variables = variables,
    parameters = params
  )
}

# Get the first date of the simulation, ie. the date corresponding to timeId == 1
.getStartDate <- function(params) {
  mNames <- c("january", "february", "march", "april", "may", "june", "july", "august",
              "september", "october", "november", "december")
  dNames <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

  p <- params$general

  # Extract year from the "horizon" parameter.
  m <- regexpr("\\d{4}", p$horizon)
  
   if (is.na(m) || length(m) == 0 || m == -1) {year <- lubridate::year(Sys.Date())
   }else {year <- as.numeric(substr(p$horizon,1,4))}

  
  
  # Is this year compatible with the parameters "january.1st" and "leapyear" ?
  start <- as.Date(paste(year, "01 01"), format = "%Y %m %d")
  jan1 <- which(dNames == p$january.1st)
  
  # If the year does not begin in january, then "january.1st" corresponds to
  # the first january of the second year
  dateJan1 <- start
  if (p$`first-month-in-year` != "january") lubridate::year(dateJan1) <- year(dateJan1) + 1
  
  # Similar problem with "leapyear"
  dateLeapYear <- start
  if (!p$`first-month-in-year` %in% c("january", "february")) 
    lubridate::year(dateLeapYear) <- year(dateLeapYear) + 1

  # If inconsistency, then choose a year that restores consistency
  if (jan1 != lubridate::wday(dateJan1) | lubridate::leap_year(dateLeapYear) != p$leapyear) {

    if (p$leapyear & p$`first-month-in-year` == "february") {
      newYear <- switch(jan1, 2045, 2029, 2041, 2025, 2037, 2021, 2033)
    } else if (p$leapyear & p$`first-month-in-year` != "february") {
      newYear <- switch(jan1, 2040, 2024, 2036, 2020, 2032, 2044, 2028)
    } else newYear <- switch(jan1, 2023, 2018, 2019, 2031, 2026, 2027, 2022)
    
    if (p$`first-month-in-year` != "january") newYear <- newYear - 1
    lubridate::year(start) <- newYear
    warning("Parameter 'horizon' is missing or inconsistent with 'january.1st' and 'leapyear'. Assume correct year is ", 
            newYear, 
            ".\nTo avoid this warning message in future simulations, open the study with Antares and go to the simulation tab, put a valid year number in the cell 'horizon' and use consistent values for parameters 'Leap year' and '1st january'.",
            call. = FALSE)

  }

  lubridate::month(start) <-  which(mNames == p$`first-month-in-year`)
  as.POSIXlt(start, tz = "UTC")
}


# Private function that reads the definition of the districts
.readDistrictsDef <- function(inputPath, areas) {
  districts <- readIniFile(file.path(inputPath, "areas/sets.ini"))
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
.readEnergyCosts <- function(inputPath) {
  costs <- readIniFile(file.path(inputPath, "thermal/areas.ini"))
  
  list(unserved  = unlist(costs$unserverdenergycost),
       spilled = unlist(costs$spilledenergycost))
}

.readLinksDef <- function(lines) {
  
  from <- character()
  to <- character()
  
  currentFrom <- lines[1]
  
  for (l in lines[-1]) {
    if (grepl("^\t", l)) {
      from <- append(from, currentFrom)
      to <- append(to, gsub("^\t", "", l))
    } else {
      currentFrom <- l
    }
  }
  if(length(from)> 0 & length(to) > 0)
  {
  data.table(link = paste(from, "-", to),
             from = from,
             to = to)
  }else{
    data.table(link = character(), from = character(), to = character())
  }
}

# >= v8.7.0 to have dimension of TS for binding constraints
.getDimBCGroups <- function(list_options){
  # list files
  bc_path <- file.path(list_options$inputPath, "bindingconstraints")
  bc_all_files <- list.files(bc_path, full.names = TRUE)
  vector_size <- file.size(bc_all_files)
  
  # return NULL if no BC
  if(sum(vector_size)==0)
    return(NULL)
  else{
    # return NULL if no .txt files (no values)
    search_values <- grepl(x = bc_all_files, pattern = ".txt")
    if(!any(search_values))
      return(NULL)
    
    # keep only values size >0
    bc_name_values_files <- gsub('(.*)_.*',
                                 '\\1',
                                 grep(x = list.files(bc_path), 
                                      pattern = ".txt", 
                                      value = TRUE))
    
    df_info_files <- data.table(path = bc_all_files[search_values], 
                                size = vector_size[search_values], 
                                bc_name = bc_name_values_files)
    df_info_files <- df_info_files[size>0,]
    
    # extract name + group from .ini properties
    properties_group <- readIniFile(file = bc_all_files[!search_values])
    
    df_groups <- do.call("rbind",
                         lapply(properties_group, function(x){
                           data.table(x$id,
                                      x$group)
                           }))
    names(df_groups)<-c("bc_name", "name_group")
    
    # merge information
    df_groups <- merge(df_info_files, df_groups)
    
    # read + dim values files
    res <- sapply(df_groups$path, function(x){
      file <- data.table::fread(file = x)
      dim(file)[2]
    })
    
    df_groups$dim <- res
    
    # filter df with only one group with dim > 1
    df_groups <- unique(df_groups[, c("name_group", "dim")])
    df_groups <- df_groups[dim>1]
    
    return(list(binding = df_groups))
  }
}

#' @title Convert the Antares number version
#' @description From V9.0, system version is 9.0 (2 digit for minor) instead of 900
#' 
#' @param antares_version ``numeric` Antares number version.
#'
#' @return `numeric` value according to study version
#' 
#' @keywords internal
.transform_antares_version <- function(antares_version) {
  antares_version <- format(antares_version, nsmall = 2)
  
  # Split major and minor parts
  antares_version_splitted <- unlist(strsplit(antares_version, split = "\\."))
  
  major <- antares_version_splitted[1]
  minor <- antares_version_splitted[2]  
  
  # max 1 digit for minor
  if (nchar(minor) > 2) 
    stop("Invalid antares_version format, good format is like '9.99'", 
         call. = FALSE)
  
  # convert to numeric for package understanding
  num_version <- round(as.numeric(antares_version)*100)
  
  return(num_version)
}




