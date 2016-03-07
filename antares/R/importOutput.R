#' Import the output of an Antares simulation
#'
#' This function imports the output of a antares simulation for some nodes,
#' links and Monte-Carlo years specified by the user.
#'
#' @param nodes
#' vector containing the name of nodes of interest. NULL if you do not want to
#' import results for any node.
#' @param links
#' vector containing the name of links of interest. NULL if you do not want to
#' import results for any link.
#' @param synthesis
#' TRUE if you want to import the synthetic results. FALSE if you prefer to import
#' year by year results
#' @param mcYears
#' Index of the Monte-Carlo years to import. Used only if synthesis is FALSE.
#' @param timeStep
#' Resolution of the data to import: hourly, daily, weekly, monthly or annual
#' @param inputs
#' Should inputs be imported?
#' @param misc
#' Should misc be imported?
#'
#' @return
#' An object of class "antaresOutput". It is a list with the following elements:
#' \itemize{
#'    \item{nodes: }{data.table containing}
#' }
#'
#' @export
#'
importOutput <- function(nodes = getOption("antares")$nodeList,
                         links = getOption("antares")$linkList,
                         synthesis = getOption("antares")$synthesis,
                         mcYears = 1:getOption("antares")$mcYears,
                         timeStep = c("hourly", "daily", "weekly", "monthly", "annual"),
                         clusters = FALSE, inputs = FALSE, misc = FALSE) {

  timeStep <- timeStep[1]
  res <- list()

  # Importation des résultats pour les noeuds
  if (!is.null(nodes)) {
    res$nodes <- llply(nodes, .importOutputForNode,
                       synthesis = synthesis, mcYears=mcYears, timeStep=timeStep,
                       .progress="text")

    res$nodes <- rbindlist(res$nodes)
  }

  res
}

.getOutputHeader <- function(path) {
  colname <- read.table(path, header=F, skip = 4, nrows = 3, sep = "\t")
  colname <- apply(colname[c(1,3),], 2, paste, collapse = "_")
  colname[1:2] <- c("node", "timeId")
  colname <- gsub("^_|_EXP$|_values$|_$", "", colname)
  colname
}

# Importation des résultats pour un seul noeud. La fonction importOutput s'occupe
# d'itérer sur tous les noeuds
.importOutputForNode <- function(node, synthesis, mcYears, timeStep) {
  opts <- getOption("antares")

  if (synthesis) { # Récupération résultats synthétiques

    path <- sprintf("%s/%s/mc-all/areas/%s/values-%s.txt",
                    opts$path, opts$opath, node, timeStep)
    if (!file.exists(path)) {
      message("Data not found for node", node)
      return(NULL)
    }

    colname <- .getOutputHeader(path)

    res <- fread(path, sep = "\t", header = F, skip = 7, stringsAsFactors = TRUE,
                 integer64 = "numeric")

  } else { # Récupération des années Monte-Carlo

    path <- sprintf("%s/%s/mc-ind/%%05.0f/areas/%s/values-%s.txt",
                    opts$path, opts$opath, node, timeStep)

    if (!file.exists(sprintf(path, 1))) {
      message("Data not found for node", node)
      return(NULL)
    }

    colname <- c(.getOutputHeader(sprintf(path, 1)), "mcYear")

    # Importation des données
    res <- llply(mcYears, function(i) {
      x <- fread(sprintf(path,i), sep = "\t", header = F, skip = 7,
                 stringsAsFactors = TRUE, integer64 = "numeric")
      x$mcYear <- i
      x
    })

    res <- rbindlist(res)

  }

  setnames(res, names(res), colname)
  res$node <- as.factor(rep(node, nrow(res)))

  res
}


.importOutputForClusters <- function(node, synthesis, mcYears, timeStep) {
  opts <- getOption("antares")

  if (synthesis) {

  }
}

.importOutputForLink <- function(link, synthesis, mcYears, timeStep) {

}
