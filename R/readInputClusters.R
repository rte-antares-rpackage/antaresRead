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
  
  allAreasClusters <- readClusterDesc(opts = opts)[, c("area", "cluster")]
  
  #To compare with area and cluster selected
  allAreasClusters$lower_area <- tolower(allAreasClusters$area)
  allAreasClusters$lower_cluster <- tolower(allAreasClusters$cluster)
  
  if (identical(areas, "all")) {
    areas <- allAreasClusters$area
  }else{
    # Check for unavailable areas
    diff_areas <- setdiff(areas, allAreasClusters$lower_area)
    if (length(diff_areas) > 0) {
      stop(paste0("the following areas are not available:", diff_areas))
    }
  }
  # All areas selected with corresponding clusters
  allAreasClusters_filtered_area <- allAreasClusters[area %in% areas]
  
  if (identical(clusters, "all")) {
    clusters <- allAreasClusters_filtered_area$cluster
  }else{
    # Check for unavailable clusters
    diff_clusters <- setdiff(clusters, allAreasClusters_filtered_area$lower_cluster)
    if (length(diff_clusters) > 0) {
      stop(paste0("the following clusters are not available:", diff_clusters))
    }
  }
  # Couple areas/clusters of interest.
  allAreasClusters_filtered <- allAreasClusters_filtered_area[cluster %in% clusters]
  
  # To loop
  clusters <- unique(allAreasClusters_filtered$cluster)
  
  res <- list() # Object the function will return
  
  # ThermalAvailabilities processing (/series)
  if (thermalAvailabilities){
    thermalTS <- as.data.table(ldply(clusters, function(cl) {
      areas <- allAreasClusters_filtered[cluster == cl]$area
      resCl <- ldply(areas, function(x){
        filePattern <- sprintf("%s/%s/%%s/series.txt", "thermal/series", x)
        mid <- .importInputTS(cl, timeStep, opts, filePattern, "ThermalAvailabilities",
                              inputTimeStep = "hourly", type = "matrix")
        
        if (is.null(mid)){
          nb_rows_ts <- opts$timeIdMax
          timeId_value <- seq(1,nb_rows_ts)
          tsId_value <- replicate(nb_rows_ts,1)
          ThermalAvailabilities_value <- replicate(nb_rows_ts,0)
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
  
  # thermalModulation processing (/prepro/.../.../modulation.txt)
  if (thermalModulation){
    thermalMod <- as.data.table(ldply(areas, .importThermalModulation, opts = opts, timeStep = timeStep))
    thermalMod <- thermalMod[cluster %in% clusters]
    setcolorder(thermalMod, c("area", "cluster", "timeId", setdiff(names(thermalMod), c("area", "cluster", "timeId"))))
    
    if (nrow(thermalMod) > 0) res$thermalModulation <- thermalMod
  }
  
  # thermalData processing (/prepro/.../.../data.txt)
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
  allAreasClusters$lower_area <- tolower(allAreasClusters$area)
  allAreasClusters$lower_cluster <- tolower(allAreasClusters$cluster)
  
  if (identical(areas, "all")) {
    areas <- allAreasClusters$area
  }else{
    # Check for unavailable areas
    diff_areas <- setdiff(areas, allAreasClusters$lower_area)
    if (length(diff_areas) > 0) {
      stop(paste0("the following areas are not available:", diff_areas))
    }
  }
  allAreasClusters_filtered_area <- allAreasClusters[area %in% areas]
  
  if (identical(clusters, "all")) {
    clusters <- allAreasClusters_filtered_area$cluster
  }else{
    # Check for unavailable clusters
    diff_clusters <- setdiff(clusters, allAreasClusters_filtered_area$lower_cluster)
    if (length(diff_clusters) > 0) {
      stop(paste0("the following clusters are not available:", diff_clusters))
    }
  }
  allAreasClusters_filtered <- allAreasClusters_filtered_area[cluster %in% clusters]
  clusters <- unique(allAreasClusters_filtered$cluster)
  
  res <- list() # Object the function will return
  
  ResTS <- as.data.table(ldply(clusters, function(cl) {
    
    areas <- allAreasClusters_filtered[cluster == cl]$area
    resCl <- ldply(areas, function(x){
      filePattern <- sprintf("%s/%s/%%s/series.txt", "renewables/series", x)
      mid <- .importInputTS(cl, timeStep, opts, filePattern, "production",
                            inputTimeStep = "hourly", type = "matrix")
      if (is.null(mid)){
        nb_rows_ts <- opts$timeIdMax
        timeId_value <- seq(1,nb_rows_ts)
        tsId_value <- replicate(nb_rows_ts,1)
        production_value <- replicate(nb_rows_ts,0)
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