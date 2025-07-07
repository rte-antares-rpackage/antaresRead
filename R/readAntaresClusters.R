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
  allClusters <- readClusterDesc(opts = opts)[, c("area","cluster")]
  ind_cluster <- which(tolower(allClusters$cluster) %in% .checkArg(tolower(clusters), 
                                                                   tolower(unique(allClusters$cluster)), 
                                                                   "clusters %s do not exist in the simulation."))
  clusters <- allClusters$cluster[ind_cluster]
  
  ind_cluster <- which(tolower(allClusters$cluster) %in% .checkArg(tolower(clusters), 
                                                                   tolower(unique(allClusters[area %in% opts$areasWithClusters]$cluster)), 
                                                                   "clusters %s have no output."))
  clusters <- unique(allClusters$cluster[ind_cluster])
  
  areas <- unique(allClusters[cluster %in% clusters]$area)
  
  res <- readAntares(clusters = areas, timeStep = timeStep, opts = opts, 
                     parallel = parallel, showProgress = showProgress)
  
  subset(res, cluster %in% clusters, select = c(setdiff(colnames(res),c("production", "NP Cost", "NODU", "profit")),
                                                intersect(colnames(res),selected))) #support for up to v8.4
}


#' Read output for a list of short-term storage clusters
#'
#' @param clustersST vector of short-term storage clusters to be imported
#' @param selected vector of thematic trimming
#' @inheritParams readAntares
#'
#' @return data.table of results for short-term storage clusters
#'
#' @export
readAntaresSTClusters <- function(clustersST, selected = c("P.injection", "levels", "P.withdrawal"),
                                  timeStep = c("hourly", "daily", "weekly", "monthly", "annual"),
                                  opts = simOptions(), parallel = FALSE, showProgress = TRUE) {
  
  if (missing(clustersST)) { 
    stop("The function 'readAntaresSTClusters' expects a vector of short-term storage clusters names as argument.")
  }
  if ("Input" %in% opts$mode) {
    stop("Cannot use 'readAntaresSTClusters' in 'Input' mode.")
  }
  if (opts$antaresVersion < 860) {
    stop("Cannot use 'readAntaresSTClusters' for a study version < 860.")
  }
  
  ##Add check control for all
  allSTClusters <- readClusterSTDesc(opts = opts)[, c("area","cluster")]
  allSTClusters$lower_cluster <- tolower(allSTClusters$cluster)
  ind_cluster <- which(allSTClusters$lower_cluster %in% .checkArg(tolower(clustersST), 
                                                                  tolower(unique(allSTClusters$cluster)), 
                                                                  "short-term storage clusters %s do not exist in the simulation."))
  clustersST <- allSTClusters$cluster[ind_cluster]
  
  ind_cluster <- which(allSTClusters$lower_cluster %in% .checkArg(tolower(clustersST), 
                                                                  tolower(unique(allSTClusters[area %in% opts$areasWithSTClusters]$cluster)), 
                                                                  "short-term storage clusters %s have no output."))
  clustersST <- unique(allSTClusters$cluster[ind_cluster])
  
  output_st_clusters <- data.table()
  if (length(clustersST) > 0) {  
    areas <- unique(allSTClusters[cluster %in% clustersST]$area)
  
    res <- readAntares(clustersST = areas, timeStep = timeStep, opts = opts, 
                       parallel = parallel, showProgress = showProgress)  

    output_st_clusters <- subset(res, cluster %in% clustersST, select = c(setdiff(colnames(res),c("P.injection", "levels", "P.withdrawal")),
                                                                    intersect(colnames(res),selected))
                                                                    )
  }
  
  return(output_st_clusters)
}
