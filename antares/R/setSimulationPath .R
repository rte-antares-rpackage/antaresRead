#' Set Path to an Antares simulation
#'
#' This function has to be used before the import functions. It sets the path to
#' the Antares simulation and displays information about the simulation
#'
#' @param path (optional) Path to the simulation. If missing, a window opens and
#' lets the user choose the directory of the simulation interactively
#' @param trace Level of information to print: 0 for no output, 1 for basic output, 2
#' for list of nodes and links
#'
#' @return A list containing various information about the simulation
#'
#' @examples
#' opts <- setSimulationPath()
#'
#' @export
#'
setSimulationPath <- function(path, trace=1) {
  if (missing(path)) {
    # /!\ MAY WORK ONLY ON WINDOWS
    path <- choose.dir(getwd(), "Select an Antares simulation directory")
  }

  oldwd <- getwd()
  setwd(path)

  # Check that the path id an Antares simulation
  if (!file.exists("info.antares-output")) {
    # So the path is not a simulation but maybe it is the path to a study.
    # If it is the case, either choose the unique simulation or ask the user to
    # choose the results of a simulation if there are several simulations.
    if (file.exists("study.antares")) {
      if (!dir.exists("output") || length(list.dirs("output", recursive = FALSE)) == 0)
        stop("Cannot find any simulation result")

      f <- list.dirs("output", recursive = FALSE)
      if (length(f) == 1) {

        path <- file.path(path, f)

      } else {

        cat("Please, choose a simulation\n")
        for (i in 1:length(f)) cat(sprintf("   %s - %s\n", i, f[i]))
        i <- scan(what = numeric(), nmax = 1)
        path <- file.path(path, f[i])

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

  # List of available nodes and links
  opath2 <- file.path(opath, ifelse(synthesis, "mc-all", "mc-ind/00001"))

  nodeList <- list.files(file.path(opath2, "areas"))
  setList <- nodeList[nodeList %like% "^@"]
  nodeList <- nodeList[!nodeList %like% "^@"]

  linkList <- list.files(file.path(opath2, "links"))

  # Nodes containing clusters
  hasClusters <- laply(file.path(opath2, "areas", nodeList), function(x) {
    f <- list.files(x)
    any(grepl("details", f))
  })

  nodesWithClusters <- nodeList[hasClusters]

  # Available variables
  variables <- list()

  # Available variables for nodes
  d <- file.path(opath2, "areas", nodeList[1])
  f <- list.files(d)
  f <- f[grep("values", f)]
  if (length(f) > 0) {
    v <- .getOutputHeader(file.path(d, f[1]), "node")
    variables$nodes <- setdiff(v, pkgEnv$idVars)
  }

  # Available variables for nodes
  d <- file.path(opath2, "links", linkList[1])
  f <- list.files(d)
  f <- f[grep("values", f)]
  if (length(f) > 0) {
    v <- .getOutputHeader(file.path(d, f[1]), "link")
    variables$links <- setdiff(v, pkgEnv$idVars)
  }

  res <- list(
    path = path,
    opath = opath,
    name = as.character(info$name),
    mode = as.character(info$mode),
    synthesis = synthesis,
    yearByYear = yearByYear,
    scenarios = scenarios,
    mcYears = mcYears,
    antaresVersion = info$version,
    start = .getStartDate(params),
    firstWeekday = as.character(params$general$first.weekday),
    nodeList = nodeList,
    setList = setList,
    linkList = linkList,
    nodesWithClusters = nodesWithClusters,
    variables = variables
  )

  options(antares=res)

  printInfo(res, trace)

  setwd(oldwd)

  invisible(res)
}

# Private function that prints info about the simulation
printInfo <- function(res, trace) {
  if (trace == 0) return()
  if (trace >= 1) {
    cat(sprintf("Antares simulation '%s'\nMode %s\n", res$name, res$mode))
    cat(sprintf("\nContent:\n - synthesis: %s\n - year by year: %s\n - MC Scenarios: %s\n",
                res$synthesis, res$yearByYear, res$scenarios))
    cat(sprintf(" - Number of nodes: %s\n - Number of sets: %s\n - Number of links: %s\n",
                length(res$nodeList), length(res$setList), length(res$linkList)))

    if (res$yearByYear) cat(sprintf(" - Number of Monte-Carlo years: %s\n", res$mcYears))
  }
  if (trace == 2) {
    cat("\nNodes:\n")
    print(res$nodeList)
    cat("\nLinks:\n")
    print(res$linkList)
  }
  cat('\nuse getOption("antares")$variables to see the list of available variables.\n')
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
  if (jan1 != wday(start) | leap_year(start) != p$leapyear) {

    if (p$leapyear) newYear <- switch(jan1, 2040, 2024, 2036, 2020, 2032, 2044, 2028)
    else newYear <- switch(jan1, 2017, 2018, 2019, 2025, 2026, 2021, 2022)

    year(start) <- newYear
    message("Date parameters are inconsistent. Assume correct year is ", newYear)

  }

  month(start) <-  which(mNames == p$`first-month-in-year`)
  as.POSIXlt(start, tz = "UTC")
}
