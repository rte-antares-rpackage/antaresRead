#Copyright © 2016 RTE Réseau de transport d’électricité
#' Read output for a list of clusters
#'
#' @param clusters vector of thermal clusters to be imported
#' @param selected vector of thematic trimming
#' @inheritParams readAntares
#'
#' @return data.table of results for thermal clusters
#'
#' @export
readAntaresClusters <- function(clusters, selected = c("production", "NP Cost", "NODU", "profit"),
                                timeStep = c("hourly", "daily", "weekly", "monthly", "annual"),
                                opts = simOptions(), parallel = FALSE, showProgress = TRUE) {
  
  if (missing(clusters)) 
    stop("The function 'readAntaresClusters' expects a vector of cluster names as argument.")
  if ("Input" %in% opts$mode)
    stop("Cannot use 'readAntaresClusters' in 'Input' mode.")
  
  ##Add check control for all
  allClusters <- readClusterDesc()[, c("area","cluster")]
  ind_cluster <- which(tolower(allClusters$cluster) %in% .checkArg(tolower(clusters), 
                                                                   tolower(unique(allClusters$cluster)), 
                                                                   "clusters %s do not exist in the simulation."))
  clusters <- allClusters$cluster[ind_cluster]
  
  ind_cluster <- which(tolower(allClusters$cluster) %in% .checkArg(tolower(clusters), 
                                                                   tolower(unique(allClusters[area %in% opts$areasWithClusters]$cluster)), 
                                                                   "clusters %s have no output."))
  clusters <- allClusters$cluster[ind_cluster]
  
  areas <- unique(allClusters[cluster %in% clusters]$area)
  
  res <- readAntares(clusters = areas, timeStep = timeStep, opts = opts, 
                     parallel = parallel, showProgress = showProgress)
  
  subset(res, cluster %in% clusters, select = c(setdiff(colnames(res),c("production", "NP Cost", "NODU", "profit")),
                                                intersect(colnames(res),selected))) #support for up to v8.4
}