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
  if (!file.exists("info.antares-output")) stop("Not an Antares simulation")

  # Get basic information about the simulation
  info <- readIniFile("info.antares-output")$general

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
    start = as.POSIXct(Sys.time()),
    nodeList = nodeList,
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
    cat(sprintf(" - Number of nodes: %s\n - Number of links: %s\n",
                length(res$nodeList), length(res$linkList)))

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
