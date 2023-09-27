#Copyright © 2016 RTE Réseau de transport d’électricité

#' Read Input thermal time series
#' 
#' @description 
#' \code{readInputThermal} is a function that reads thermal time series from an antares 
#' project. But contrary to \code{\link{readAntares}}, it only reads time series
#' stored in the input folder, so it can work in "input" mode. 
#' 
#' @param clusters vector of clusters names for which thermal time series must be read.
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
readInputThermal <- function(clusters = NULL, thermalModulation = FALSE, thermalData = FALSE,
                             opts = simOptions(),
                             timeStep = c("hourly", "daily", "weekly", "monthly", "annual"),
                             simplify = TRUE, parallel = FALSE,
                             showProgress = TRUE) {
  
  timeStep <- match.arg(timeStep)
  
  # Can the importation be parallelized ?
  if (parallel) {
    if(!requireNamespace("foreach")) stop("Parallelized importation impossible. Please install the 'foreach' package and a parallel backend provider like 'doParallel'.")
    if (!foreach::getDoParRegistered()) stop("Parallelized importation impossible. Please register a parallel backend, for instance with function 'registerDoParallel'")
  }
  
  allAreasClusters <- readClusterDesc(opts = opts)[area %in% opts$areasWithClusters, c("area", "cluster")]
  allClusters <- unique(allAreasClusters$cluster)
  # Manage special value "all"
  if(identical(clusters, "all")) clusters <- allClusters

  if (length(setdiff(tolower(clusters), tolower(allClusters))) > 0){
    cat(c("the following clusters are not available : ",setdiff(tolower(clusters), tolower(allClusters))))
    stop("Some clusters are not available in the areas specified")
  }
  
  ind_cluster <- which(tolower(allClusters) %in% tolower(clusters))
  clusters <- unique(allClusters[ind_cluster])
  res <- list() # Object the function will return
  
  thermalTS <- as.data.table(ldply(clusters, function(cl) {
    
    area <- unique(allAreasClusters[cluster == cl]$area)
    if (length(area) > 1) warning(cl," is in more than one area")
    resCl <- ldply(area, function(x){
      filePattern <- sprintf("%s/%s/%%s/series.txt", "thermal/series", x)
      mid <- .importInputTS(cl, timeStep, opts, filePattern, "ThermalAvailabilities",
                            inputTimeStep = "hourly", type = "matrix")
      if (is.null(mid)) return (data.table())
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
  
  # thermalModulation processing
  if (thermalModulation){
    areas <- unique(allAreasClusters[cluster %in% clusters]$area)
    thermalMod <- as.data.table(ldply(areas, .importThermalModulation, opts = opts, timeStep = timeStep))
    thermalMod <- thermalMod[cluster %in% clusters]
    setcolorder(thermalMod, c("area", "cluster", "timeId", setdiff(names(thermalMod), c("area", "cluster", "timeId"))))
    
    if (nrow(thermalMod) > 0) res$thermalModulation <- thermalMod
  }
  
  # thermalData processing
  if (thermalData){
    areas <- unique(allAreasClusters[cluster %in% clusters]$area)
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
readInputRES <- function(clusters = NULL, opts = simOptions(),
                         timeStep = c("hourly", "daily", "weekly", "monthly", "annual"),
                         simplify = TRUE, parallel = FALSE,
                         showProgress = TRUE) {
  
  timeStep <- match.arg(timeStep)
  
  # Can the importation be parallelized ?
  if (parallel) {
    if(!requireNamespace("foreach")) stop("Parallelized importation impossible. Please install the 'foreach' package and a parallel backend provider like 'doParallel'.")
    if (!foreach::getDoParRegistered()) stop("Parallelized importation impossible. Please register a parallel backend, for instance with function 'registerDoParallel'")
  }
  
  allAreasClusters <- readClusterResDesc(opts = opts)[area %in% opts$areasWithResClusters, c("area", "cluster")]
  allClusters <- unique(allAreasClusters$cluster)
  # Manage special value "all"
  if(identical(clusters, "all")) clusters <- allClusters
  
  if (length(setdiff(tolower(clusters), tolower(allClusters))) > 0){
    cat(c("the following clusters are not available : ",setdiff(tolower(clusters), tolower(allClusters))))
    stop("Some clusters are not available in the areas specified")
  }
  
  ind_cluster <- which(tolower(allClusters) %in% tolower(clusters))
  clusters <- unique(allClusters[ind_cluster])
  res <- list() # Object the function will return
  
  ResTS <- as.data.table(ldply(clusters, function(cl) {
    
    area <- unique(allAreasClusters[cluster == cl]$area)
    if (length(area) > 1) warning(cl," is in more than one area")
    resCl <- ldply(area, function(x){
      filePattern <- sprintf("%s/%s/%%s/series.txt", "renewables/series", x)
      mid <- .importInputTS(cl, timeStep, opts, filePattern, "production",
                            inputTimeStep = "hourly", type = "matrix")
      if (is.null(mid)) return (data.table())
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