#' Set Path to an Antares simulation
#'
#' This function has to be used before the \code{read} functions. It sets the path to
#' the Antares simulation to work on and other useful options (list of areas,
#' links, areas with clusters, variables, etc.). 
#' 
#' @details 
#' The simulation choosen with \code{setSimulationPath} becomes the default
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
#'   directory of the simulation interactively
#' @param simulation
#'   (optional) Only used if "path" represents the path of a study and not of the
#'   output of a simulation. It can be either the name of the simulation or a
#'   number indicating which simulation to use. It is possible to use negative 
#'   values to select a simulation from the last one: for instance -1 will
#'   select the most recent simulation, -2 will the penultimate one, etc. There 
#'   are two special values 0 and "input" that tells the function that the user
#'   is not interested by the results of any simulation, but only by the inputs.
#'   In such a case, the function \code{\link{readAntares}} is anavailable. 
#'
#' @return A list containing various information about the simulation, in particular:
#'   \item{path}{path of the simulation}
#'   \item{name}{name of the study}
#'   \item{synthesis}{Are synthetic results available ?}
#'   \item{yearByYear}{Are the results for each Monte Carlo simulation available ?}
#'   \item{scenarios}{Are the Monte-Carlo scenarii stored in output ? This is 
#'     important to reconstruct some input time series that have been used in 
#'     each Monte-Carlo simulation.}
#'   \item{mcYears}{Number of Monte-Carlo scenarii}
#'   \item{start}{Date of the first day of the year in the simulation.}
#'   \item{areaList}{Vector of the available areas}
#'   \item{setList}{Vector of the available clusters}
#'   \item{linkList}{Vector of the available links}
#'   \item{areasWithClusters}{Vector of areas containing clusters}
#'   \item{variables}{available variables for areas, districts and links}
#'   \item{parameters}{Other parameters of the simulation.}
#' 
#' @seealso 
#'   \code{\link{simOptions}}, \code{\link{readAntares}}, \code{\link{readLayout}}, 
#'   \code{\link{readClusterDesc}}, \code{\link{readBindingConstraints}}
#'
#' @examples
#' 
#' \dontrun{
#' # Select interactively a study. This only works on windows.
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
setSimulationPath <- function(path, simulation) {
  if (missing(path)) {
    # /!\ MAY WORK ONLY ON WINDOWS
    path <- choose.dir(getwd(), "Select an Antares simulation directory")
    if (is.na(path)) stop("You have canceled the execution.")
  }

  oldwd <- getwd()
  setwd(path)

  # Check that the path id an Antares simulation
  if (!file.exists("info.antares-output")) {
    # So the path is not a simulation but maybe it is the path to a study.
    # If it is the case, there are three scenarii:
    # - 1. asks the user to interactively choose one simulation
    # - 2. use the value of parameter "simulation" if it is set
    # - 3. there is only one study in the output. Select it
    if (file.exists("study.antares")) {
      if (!dir.exists("output") || length(list.dirs("output", recursive = FALSE)) == 0)
        stop("Cannot find any simulation result")

      f <- list.dirs("output", recursive = FALSE)
      
      if (missing(simulation)) {
        if (length(f) == 1) {
          simulation <- 1
        } else { # Case 3
          cat("Please, choose a simulation\n")
          for (i in 1:length(f)) cat(sprintf("   %s - %s\n", i, f[i]))
          simulation <- type.convert(scan(what = character(), nmax = 1), as.is = TRUE)
        }
      }
        
      if (is.numeric(simulation)) {
        if (simulation == 0) return(.inputSimOptions(path))
        
        if (simulation > 0) path <- file.path(path, f[simulation])
        else path <- file.path(path, rev(f)[abs(simulation)])
      } else {
        if (simulation == "input") return(.inputSimOptions(path))
        
        f <- f[grep(paste0("-", simulation, "$"), f, ignore.case = TRUE)]
        if (length(f) == 0) stop ("Cannot find the simulation called ", simulation)
        
        path <- file.path(path, f[1])
      }

      setwd(path)

    } else {
      stop("Directory is not an Antares study.")
    }
  }

  # Get basic information about the simulation
  info <- readIniFile("info.antares-output")$general
  params <- readIniFile("about-the-study/parameters.ini")

  # Where are located the results ?
  opath <- switch(as.character(info$mode),
                  "draft" = "adequacy-draft",
                  "Economy" = "economy",
                  "Adequacy" = "adequacy")

  # Which results are available ? Synthesis ? Monte-Carlo years ?
  synthesis <- file.exists(file.path(opath, "mc-all"))
  yearByYear <- file.exists(file.path(opath, "mc-ind"))
  scenarios <- file.exists("ts-numbers")

  mcYears <- ifelse(yearByYear,
                    length(list.files(file.path(opath, "mc-ind"))),
                    0)

  if (!synthesis & !yearByYear) stop("No results found")

  # List of available areas and links
  opath2 <- file.path(opath, ifelse(synthesis, "mc-all", "mc-ind/00001"))

  areaList <- list.files(file.path(opath2, "areas"))
  setList <- areaList[areaList %like% "^@"]
  areaList <- areaList[!areaList %like% "^@"]

  linkList <- list.files(file.path(opath2, "links"))

  # Areas containing clusters
  hasClusters <- laply(file.path(opath2, "areas", areaList), function(x) {
    f <- list.files(x)
    any(grepl("details", f))
  })

  areasWithClusters <- areaList[hasClusters]

  # Available variables
  variables <- list()

  # Available variables for areas
  d <- file.path(opath2, "areas", areaList[1])
  f <- list.files(d)
  f <- f[grep("values", f)]
  if (length(f) > 0) {
    v <- .getOutputHeader(file.path(d, f[1]), "area")
    variables$areas <- setdiff(v, pkgEnv$idVars)
  }

  # Available variables for links
  d <- file.path(opath2, "links", linkList[1])
  f <- list.files(d)
  f <- f[grep("values", f)]
  if (length(f) > 0) {
    v <- .getOutputHeader(file.path(d, f[1]), "link")
    variables$links <- setdiff(v, pkgEnv$idVars)
  }
  
  # Optimisation parameters
  parameters <- readIniFile("about-the-study/parameters.ini")

  res <- list(
    studyPath = normalizePath(file.path(path, "../..")),
    studyName = readIniFile(file.path(path, "../../study.antares"))$antares$caption,
    path = path,
    opath = opath,
    inputPath = file.path(path, "../../input"),
    name = as.character(info$name),
    mode = as.character(info$mode),
    synthesis = synthesis,
    yearByYear = yearByYear,
    scenarios = scenarios,
    mcYears = mcYears,
    antaresVersion = info$version,
    start = .getStartDate(params),
    firstWeekday = as.character(params$general$first.weekday),
    areaList = areaList,
    setList = setList,
    linkList = linkList,
    areasWithClusters = areasWithClusters,
    variables = variables,
    parameters = parameters
  )
  
  class(res) <- c("simOptions")

  options(antares=res)

  setwd(oldwd)

  res
}

# Private function called when the user just wants to read input time series.
# It returns a "simOptions" object with less parameters.
.inputSimOptions <- function(path) {
  study <- readIniFile(file.path(path, "study.antares"))
  params <- readIniFile(file.path(path, "settings/generaldata.ini"))
  
  areaList <- tolower(readLines(file.path(path, "input/areas/list.txt")))
  setList <- names(readIniFile(file.path(path, "input/areas/sets.ini")))
  
  linkList <- unlist(llply(list.files(file.path(path, "input/links")), function(f) {
    if (!dir.exists(file.path(path, "input/links", f))) return(NULL)
    to <- list.files(file.path(path, "input/links", f))
    to <- to[to != "properties.ini"]
    to <- gsub(".txt", "", to)
    
    if (length(to) == 0) return(NULL)
    
    paste(f, "-", to)
  }))
  
  res <- list(
    studyPath = normalizePath(path),
    studyName = study$antares$caption,
    inputPath = file.path(path, "input"),
    mode = "Input",
    antaresVersion = study$antares$version,
    start = .getStartDate(params),
    firstWeekday = as.character(params$general$first.weekday),
    areaList = areaList,
    setList = setList,
    linkList = linkList,
    areasWithClusters = NA
  )
  
  class(res) <- c("simOptions")
  
  options(antares=res)
  
  res
}

.getStartDate <- function(params) {
  mNames <- c("january", "february", "march", "april", "may", "june", "july",
              "september", "october", "november", "december")
  dNames <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

  p <- params$general

  # Extract year from the "horizon" parameter.
  m <- regexpr("\\d{4}", p$horizon)
  if (m == -1) year <- 2017
  else year <- as.numeric(regmatches(p$horizon, m))

  # Is this year compatible with the parameters "january.1st" and "leapyear" ?
  start <- as.Date(paste(year, "01 01"), format = "%Y %m %d")
  jan1 <- which(dNames == p$january.1st)

  # If inconsistency, then choose a year that restores consistency
  if (jan1 != wday(start) | lubridate::leap_year(start) != p$leapyear) {

    if (p$leapyear) newYear <- switch(jan1, 2040, 2024, 2036, 2020, 2032, 2044, 2028)
    else newYear <- switch(jan1, 2017, 2018, 2019, 2025, 2026, 2021, 2022)

    lubridate::year(start) <- newYear
    message("Date parameters are inconsistent. Assume correct year is ", newYear)

  }

  lubridate::month(start) <-  which(mNames == p$`first-month-in-year`)
  as.POSIXlt(start, tz = "UTC")
}
