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
#' setSimulationPath()
#'
#' @export
#'
setSimulationPath <- function(path, trace=1) {
  if (missing(path)) {
    # /!\ NE FONCTIONNERAIT QUE SUR WINDOWS
    path <- choose.dir(getwd(), "Select an Antares simulation directory")
  }

  oldwd <- getwd()
  setwd(path)

  # Vérifier que c'est bien une simulation Antares
  if (!file.exists("info.antares-output")) stop("Not an Antares simulation")

  # Récupérer les infos basiques sur la simulation
  info <- readIniFile("info.antares-output")$general

  # Où se trouvent les résultats ?
  opath <- switch(as.character(info$mode),
                  "draft" = "adequacy-draft",
                  "Economy" = "economy",
                  "Adequacy" = "adequacy")

  # Quels résultats sont disponibles ? Synthèse ? Années Monte-Carlo ?
  synthesis <- file.exists(file.path(opath, "mc-all"))
  yearByYear <- file.exists(file.path(opath, "mc-ind"))
  scenarios <- file.exists("ts-numbers")

  mcYears <- ifelse(yearByYear,
                    length(list.files(file.path(opath, "mc-ind"))),
                    0)

  if (!synthesis & !yearByYear) stop("No results found")

  # Liste des noeuds et des liens
  opath2 <- file.path(opath, ifelse(synthesis, "mc-all", "mc-ind/00001"))
  nodeList <- list.files(file.path(opath2, "areas"))
  linkList <- list.files(file.path(opath2, "links"))

  res <- list(
    path = path,
    opath = opath,
    name = info$name,
    mode = info$mode,
    synthesis = synthesis,
    yearByYear = yearByYear,
    scenarios = scenarios,
    mcYears = mcYears,
    antaresVersion = info$version,
    start = as.POSIXct(Sys.time()),
    nodeList = nodeList,
    linkList = linkList
  )

  options(antares=res)

  printInfo(res, trace)

  setwd(oldwd)

  invisible(res)
}

# Fonction privée affichant des informations sur le projet
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
}
