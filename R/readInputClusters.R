#Copyright © 2016 RTE Réseau de transport d’électricité

#' Read Input thermal time series
#' 
#' @description 
#' \code{readInputThermal} is a function that reads thermal time series from an antares 
#' project. But contrary to \code{\link{readAntares}}, it only reads time series
#' stored in the input folder, so it can work in "input" mode. 
#' 
#' @param areas vector of areas names for which thermal time series must be read.
#' @param clusters vector of clusters names for which thermal time series must be read.
#' @param thermalAvailabilities if TRUE, return thermalAvailabilities data
#' @param thermalModulation if TRUE, return thermalModulation data
#' @param thermalData if TRUE, return thermalData from prepro
#' @inheritParams readAntares
#' 
#' @return 
#' If thermalModulation or thermalData is TRUE, an object of class "antaresDataList" is returned. It is a list of
#' data.tables for selected input
#' 
#' Else the result is a data.table with class "antaresDataTable".
#' 
#' @note 
#' the clusters parameter can also accept the special value "all". 
#' It indicates the function to read the desired time series for all clusters.
#' 
#' @seealso 
#' \code{\link{setSimulationPath}}, \code{\link{readAntares}}, 
#' \code{\link{getAreas}}, \code{\link{getLinks}}
#' 
#' @export
readInputThermal <- function(areas = "all",
                             clusters,
                             thermalAvailabilities = TRUE,
                             thermalModulation = FALSE,
                             thermalData = FALSE,
                             opts = simOptions(),
                             timeStep = c("hourly", "daily", "weekly", "monthly", "annual"),
                             simplify = TRUE,
                             parallel = FALSE,
                             showProgress = TRUE) {
  
  if(!any(thermalAvailabilities, thermalModulation, thermalData)){
    stop("At least one type of data should be selected")
  }
  
  timeStep <- match.arg(timeStep)
  areas <- tolower(unique(areas))
  clusters <- tolower(unique(clusters))
  
  # Can the importation be parallelized ?
  if (parallel) {
    if(!requireNamespace("foreach")) stop("Parallelized importation impossible. Please install the 'foreach' package and a parallel backend provider like 'doParallel'.")
    if (!foreach::getDoParRegistered()) stop("Parallelized importation impossible. Please register a parallel backend, for instance with function 'registerDoParallel'")
  }
  
  allAreasClusters <- readClusterDesc(opts = opts)[area %in% opts$areasWithClusters, c("area", "cluster")]
  
  #Check if areas and clusters input correspond to study
  lst_areas_clusters <- .check_areas_clusters(allAreasClusters, areas, clusters)
  
  #Get areas, clusters and areas/clusters pairs
  areas <- lst_areas_clusters$areas
  clusters <- lst_areas_clusters$clusters
  areas_clusters_table <- lst_areas_clusters$areas_clusters_table
  
  res <- list() # Object the function will return
  
  # ThermalAvailabilities processing
  if (thermalAvailabilities){
    thermalTS <- as.data.table(ldply(clusters, function(cl) {
      areas <- areas_clusters_table[cluster == cl]$area
      resCl <- ldply(areas, function(x){
        filePattern <- sprintf("%s/%s/%%s/series.txt", "thermal/series", x)
        mid <- .importInputTS(cl, timeStep, opts, filePattern, "ThermalAvailabilities",
                              inputTimeStep = "hourly", type = "matrix")
        
        if (is.null(mid)){
          timeId_value <- 1:8736
          tsId_value <- replicate(8736,1)
          ThermalAvailabilities_value <- replicate(8736,0)
          mid <- data.table("timeId" = timeId_value, "tsId" = tsId_value, "ThermalAvailabilities" = ThermalAvailabilities_value)
        }
        mid$area <- x
        mid$cluster <- cl
        mid
      })
      resCl <- dcast(as.data.table(resCl), area + cluster + timeId ~ tsId, value.var = "ThermalAvailabilities")
    }))
    
    tsCols <- setdiff(colnames(thermalTS), c("area", "cluster", "timeId"))
    setnames(thermalTS, tsCols, paste0("ts",tsCols))
    setcolorder(thermalTS, c("area", "cluster", "timeId", setdiff(names(thermalTS), c("area", "cluster", "timeId"))))
    
    if (nrow(thermalTS) > 0) res$thermalAvailabilities <- thermalTS
  }
  
  
  # thermalModulation processing
  if (thermalModulation){
    thermalMod <- as.data.table(ldply(areas, .importThermalModulation, opts = opts, timeStep = timeStep))
    thermalMod <- thermalMod[cluster %in% clusters]
    setcolorder(thermalMod, c("area", "cluster", "timeId", setdiff(names(thermalMod), c("area", "cluster", "timeId"))))
    
    if (nrow(thermalMod) > 0) res$thermalModulation <- thermalMod
  }
  
  # thermalData processing
  if (thermalData){
    thermalDat <- as.data.table(ldply(areas, .importThermalData, opts = opts, timeStep = timeStep))
    thermalDat <- thermalDat[cluster %in% clusters]
    setcolorder(thermalDat, c("area", "cluster", "timeId", setdiff(names(thermalDat), c("area", "cluster", "timeId"))))
    
    if (nrow(thermalDat) > 0) res$thermalData <- thermalDat
  }
  
  if (length(res) == 0) stop("At least one argument of readInputTS has to be defined.")
  
  # Class and attributes
  res <- .addClassAndAttributes(res, NULL, timeStep, opts, simplify)
  addDateTimeColumns(res)
}




#' Read Input RES time series
#' 
#' @description 
#' \code{readInputRes} is a function that reads renewable time series from an antares 
#' project. But contrary to \code{\link{readAntares}}, it only reads time series
#' stored in the input folder, so it can work in "input" mode. 
#' 
#' @param areas vector of RES areas names for which renewable time series must be read.
#' @param clusters vector of RES clusters names for which renewable time series must be read.
#' @inheritParams readAntares
#' 
#' @return 
#' data.table with class "antaresDataTable".
#' 
#' @seealso 
#' \code{\link{setSimulationPath}}, \code{\link{readAntares}}, 
#' \code{\link{getAreas}}, \code{\link{getLinks}}
#' 
#' @export
readInputRES <- function(areas = "all",
                         clusters,
                         opts = simOptions(),
                         timeStep = c("hourly", "daily", "weekly", "monthly", "annual"),
                         simplify = TRUE,
                         parallel = FALSE,
                         showProgress = TRUE) {
  
  timeStep <- match.arg(timeStep)
  areas <- tolower(unique(areas))
  clusters <- tolower(unique(clusters))
  
  # Can the importation be parallelized ?
  if (parallel) {
    if(!requireNamespace("foreach")) stop("Parallelized importation impossible. Please install the 'foreach' package and a parallel backend provider like 'doParallel'.")
    if (!foreach::getDoParRegistered()) stop("Parallelized importation impossible. Please register a parallel backend, for instance with function 'registerDoParallel'")
  }
  
  allAreasClusters <- readClusterResDesc(opts = opts)[area %in% opts$areasWithResClusters, c("area", "cluster")]
  
  #Check if areas and clusters input correspond to study
  lst_areas_clusters <- .check_areas_clusters(allAreasClusters, areas, clusters)
  
  #Get areas, clusters and areas/clusters pairs
  areas <- lst_areas_clusters$areas
  clusters <- lst_areas_clusters$clusters
  areas_clusters_table <- lst_areas_clusters$areas_clusters_table
  
  res <- list() # Object the function will return
  
  ResTS <- as.data.table(ldply(clusters, function(cl) {
    
    areas <- areas_clusters_table[cluster == cl]$area
    resCl <- ldply(areas, function(x){
      filePattern <- sprintf("%s/%s/%%s/series.txt", "renewables/series", x)
      mid <- .importInputTS(cl, timeStep, opts, filePattern, "production",
                            inputTimeStep = "hourly", type = "matrix")
      if (is.null(mid)){
        timeId_value <- 1:8736
        tsId_value <- replicate(8736,1)
        production_value <- replicate(8736,0)
        mid <- data.table("timeId" = timeId_value, "tsId" = tsId_value, "production" = production_value)
      }
      mid$area <- x
      mid$cluster <- cl
      mid
    })
    
    resCl <- dcast(as.data.table(resCl), area + cluster + timeId ~ tsId, value.var = "production")
  }))
  
  tsCols <- setdiff(colnames(ResTS), c("area", "cluster", "timeId"))
  setnames(ResTS, tsCols, paste0("ts",tsCols))
  setcolorder(ResTS, c("area", "cluster", "timeId", setdiff(names(ResTS), c("area", "cluster", "timeId"))))
  
  if (nrow(ResTS) > 0) res$ResProduction <- ResTS
  
  if (length(res) == 0) stop("At least one argument of readInputRes has to be defined.")
  
  # Class and attributes
  res <- .addClassAndAttributes(res, NULL, timeStep, opts, simplify)
  addDateTimeColumns(res)
}


.check_areas_clusters <- function(allAreasClusters, areas, clusters) {
  allAreas <- allAreasClusters$area
  allClusters <- allAreasClusters$cluster
  
  all_areas_clusters_table <- data.table("area" = tolower(allAreas), "cluster" = tolower(allClusters))
  
  # Check for "all" values
  is_areas_all <- identical(areas, "all")
  is_clusters_all <- identical(clusters, "all")
  
  # Filter areas and clusters based on selections
  if (is_areas_all & is_clusters_all) {
    areas <- allAreas
    clusters <- allClusters
  } else if (is_areas_all & !is_clusters_all) {
    areas <- all_areas_clusters_table[cluster %in% tolower(clusters)]$area
    
    # Check for unavailable clusters
    diff_clusters <- setdiff(clusters, all_areas_clusters_table$cluster)
    if (length(diff_clusters) > 0) {
      stop(paste0("the following clusters are not available:", diff_clusters))
    }
    clusters <- all_areas_clusters_table[cluster %in% tolower(clusters)]$cluster
  } else if (!is_areas_all & is_clusters_all) {
    clusters <- all_areas_clusters_table[area %in% tolower(areas)]$cluster
    
    # Check for unavailable areas
    diff_areas <- setdiff(areas, all_areas_clusters_table$area)
    if (length(diff_areas) > 0) {
      stop(paste0("the following areas are not available:", diff_areas))
    }
    areas <- all_areas_clusters_table[area %in% tolower(areas)]$area
  }
  
  #Get all areas/clusters pairs
  areas_clusters_table <- data.table("area" = areas, "cluster" = clusters)
  
  # Check for unavailable area/cluster pairs
  diff_areas_cluster <- fsetdiff(areas_clusters_table, all_areas_clusters_table)
  if (nrow(diff_areas_cluster) > 0) {
    pairs_not_available <- sapply(1:nrow(diff_areas_cluster), function(i) {
      paste(diff_areas_cluster[i, ], collapse = "/")
    })
    stop(paste0("the following pairs area/cluster are not available:", pairs_not_available))
  }
  
  # Return filtered areas, clusters, and table
  return(list(areas = areas, clusters = unique(clusters), areas_clusters_table = areas_clusters_table))
}