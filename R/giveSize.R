#Copyright © 2016 RTE Réseau de transport d’électricité



#' Specify RAM limit
#' 
#' This function specify RAM limit (in Go) of the value returned by \link[antaresRead]{readAntares}.
#' 
#' @param x \code{numeric} RAM limit in Go
#' 
#' @examples
#' \dontrun{
#' #Set maximum ram to used to 50 Go
#' setRam(50)
#' }
#' 
#' @export
setRam <- function(x){
  options(maxSizeLoadOnComp = x)
  options(maxSizeLoad = x)
}




#' Give an estimation of size of data load
#'
#' @noRd
.giveSize <- function(opts, areas = NULL, links = NULL,
                      clusters = NULL, districts = NULL, select = NULL, mcYears = NULL,
                      timeStep = "hourly", misc = FALSE, thermalAvailabilities = FALSE,
                      hydroStorage = FALSE, hydroStorageMaxPower = FALSE, reserve = FALSE,
                      linkCapacity = FALSE, mustRun = FALSE, thermalModulation = FALSE){
  
  if(length(select) > 0)
  {
    select <- unlist(llply(select, function(x) {
      for (alias in names(pkgEnv$varAliases)) {
        if (tolower(alias) %in% tolower(x)) x <- append(x, pkgEnv$varAliases[[alias]]$select)
      }
      x
    }))
  }
  
  sizeObject <- utils::object.size(1.10)
  sizeObject <- 4.6
  sizeObjectCl <- 5.5
  #Nombre id time
  nbTid <- opts$timeIdMax - opts$timeIdMin
  nbIdCols <- 6
  if(timeStep == "daily"){
    nbTid <- nbTid/24
    nbIdCols <- 5
  }
  if(timeStep == "weekly"){
    nbTid <- nbTid/24/7
    nbIdCols <- 3
  }
  if(timeStep == "monthly"){
    nbTid <- nbTid/24/30.5
    nbIdCols <- 4
  }
  if(timeStep == "annual"){
    nbTid <- nbTid/24/365
    nbIdCols = 3
  }
  
  nbTid <- nbTid
  if(is.null(areas) & is.null(links) & is.null(clusters) & is.null(districts)){
    areas <- "all"
  }
  
  if(is.null(mcYears)){
    nbMc <- 1
  }else{
    nbIdCols <- nbIdCols + 1
    if("all" %in% mcYears){
      nbMc <- length(opts$mcYears)
    }else{
      nbMc <- length(mcYears)
    }
  }
  
  ##Areas size
  if("all" %in% areas){
    nbAreas <- length(opts$areaList)
  }else{
    nbAreas <- length(areas)
  }
  
  if(is.null(select)){
    if(is.null(mcYears)){
      selectAreas <- opts$variables$areas
    }else{
      selectAreas <- opts$variables$areas
      selectAreas <- selectAreas[-grep("_max" , selectAreas)]
      selectAreas <- selectAreas[-grep("_min" , selectAreas)]
      selectAreas <- selectAreas[-grep("_std" , selectAreas)]
    }
  }else{
    selectAreas <- select
  }
  
  
  nbVarsAreas <- length(selectAreas) + nbIdCols
  if(hydroStorage){
    nbVarsAreas <- nbVarsAreas + 1
  }
  
  if(misc){
    nbVarsAreas <- nbVarsAreas + 8
  }
  if(hydroStorageMaxPower){
    nbVarsAreas <- nbVarsAreas + 3
  }
  if(reserve){
    nbVarsAreas <- nbVarsAreas + 4 
  }
  
  if(mustRun){
    nbVarsAreas <- nbVarsAreas + 4
  }
  
  nbRowAreas <- nbTid * nbMc * nbAreas * nbVarsAreas
  areasSize <- sizeObject * nbRowAreas / 1024 ^2
  
  ##Link size
  
  if("all" %in% links){
    nbLinks <- length(opts$linkList)
  }else{
    nbLinks <- length(links)
  }
  
  if(is.null(select)){
    if(is.null(mcYears)){
      selectLinks <- opts$variables$links
    }else{
      selectLinks <- opts$variables$links
      selectLinks <- selectLinks[-grep("_max" , selectLinks)]
      selectLinks <- selectLinks[-grep("_min" , selectLinks)]
      selectLinks <- selectLinks[-grep("_std" , selectLinks)]
    }
  }else{
    selectLinks <- select
  }
  nbVarsLinks <- length(selectLinks) + nbIdCols
  
  if(linkCapacity){
    nbVarsLinks <- nbVarsLinks + 5
  }
  
  nbRowLinks <- nbTid * nbMc * nbLinks * nbVarsLinks
  linksSize <- sizeObject * nbRowLinks / 1024 ^2
  
  
  
  ##Districts size
  if(!is.null(districts))
  {
    if("all" %in% districts){
      nbDis <- length(opts$districtList)
    }else{
      nbDis <- length(districts)
    }
  }else{
    nbDis <- 0
  }
  
  nbRowDistricts <- nbTid * nbMc * nbDis * nbVarsAreas
  disSize <- sizeObject * nbRowDistricts / 1024 ^2
  
  clusWithData <- data.table()
  if(!is.null(clusters))
  {
    clusWithData  <- readClusterDesc()
    if("all" %in% clusters){
      enabled <- TRUE
      if("enabled" %in% names(clusWithData))
      {
        clusWithData <- clusWithData[is.na(enabled)]
      }
    }else{
      clustersS <- clusters
      if("enabled" %in% names(clusWithData))
      {
        clusWithData <- clusWithData[is.na(enabled)]
      }
      if("area" %in% names(clusWithData))
      {
      clusWithData <- clusWithData[ area %in% clustersS]
      }
    }
  }
  
  nbColcl <- 4
  
  if(thermalAvailabilities){
    nbColcl <- nbColcl + 2
  }
  if(thermalModulation){
    nbColcl <- nbColcl + 4
  }
  nbRowClusters <- nbTid * nbMc * nrow(clusWithData) * ( nbColcl + nbIdCols)
  clSize <- sizeObjectCl * nbRowClusters / 1024 ^2
  
  linksSize + areasSize + disSize + clSize
}

