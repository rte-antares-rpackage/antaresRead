#Copyright © 2016 RTE Réseau de transport d’électricité

#' hvdc straitement
#'
#' @description usage for hvdc
#' 
#' @param data \code{antaresDataList} data to apply straitement
#' @param removeHvdcAreas \code{boolean} remove HVDC areas.
#' @param reafectLinks \code{boolean} .
#' 
#' @return  Object of class "antaresDataList" is returned. 
#' It is a list of data.tables, each element representing one type of element (areas, links, clusters)
#' 
#' @examples 
#' \dontrun{
#' 
#' data <- readAntares(areas = 'all', links = 'all')
#' data <- setHvdcAreas(data, "psp in")
#' data <- hvdcModification(data)
#' 
#' }
#' 
#' @export
hvdcModification <- function(data, removeHvdcAreas = TRUE, reafectLinks = FALSE){
  if(removeHvdcAreas == TRUE){
    if("antaresDataList" %in% class(data)){
      if(is.null(attributes(data$areas)$hvdcAreas)){
        stop("You must indicate hvdcAreas with setHvdcAreas function")
      }
      data$areas <- .removeAreasInList(data$areas, attributes(data$areas)$hvdcAreas)
    }
    
    if("antaresDataTable" %in% class(data)){
      
      if(is.null(attributes(data)$hvdcAreas)){
        stop("You must indicate hvdcAreas with setHvdcAreas function")
      }
      data <- .removeAreasInList(data, attributes(data)$hvdcAreas)
    }
  }
  
  if(reafectLinks == TRUE){
    if(!"antaresDataList" %in% class(data) || !(all(c('areas', 'links') %in% names(data)))){
      stop("You mush haveantaresDataList with areas and links data")
    }
    data <-  .reafectLinks(data)
    areasHvdc <- attributes(data$areas)$hvdcAreas
    lkSupr <- getLinks(areasHvdc)
    data$links <- data$links[!link %in% lkSupr]
  }
  
  data
}


.reafectLinks <- function(data){
  areasHvdc <- attributes(data$areas)$hvdcAreas
  areaHvdc <- areasHvdc[1]
  for (areaHvdc in areasHvdc){
    linksHvdcin <- getLinks(areaHvdc)
    linksHvdc <- strsplit(linksHvdcin, " - ")
    noHvdc <- unlist(linksHvdc)[unlist(linksHvdc) != areaHvdc]
    
    possibleLinks <- c(paste(noHvdc, collapse = " - "), paste(rev(noHvdc), collapse = " - "))
    avialableLink <- possibleLinks[possibleLinks %in% unique(data$links$link)]
    if(length(avialableLink) == 0){
      ll <- unlist(strsplit(linksHvdcin, ' - '))
      avialableLink <- paste(sort(ll[ll!=areaHvdc]), collapse = " - ")
    }
    
    
    IDcols <- getIdCols(data$links)
    
    selLk <- data$links[link %in% linksHvdcin, .SD, .SDcols = c(IDcols, "FLOW LIN.")]
    
    lktp <- strsplit(avialableLink, " - ")[[1]]
    inLink <- lktp[1]
    outLink <- lktp[2]
    toRev <- unlist(lapply(linksHvdc, function(X){
      if(X[1] == inLink){
        return(FALSE)
      }else{
        if(X[2] == outLink){
          return(FALSE)
        }else{
          return(TRUE)
        }
      }
    }))
    selLk[link %in% linksHvdcin[toRev]]$`FLOW LIN.` <- - selLk[link %in% linksHvdcin[toRev]]$`FLOW LIN.`
    
    if(nrow( data$links[link == avialableLink]) == 0){
      lk <- data.table(data$links[, .SD, .SDcols = getIdCols(data$links)])
      lk <- lk[link == link[1]]
      lk$link <- avialableLink
      lk$`FLOW LIN.` <-  data$links[link == linksHvdcin[1]]$`FLOW LIN.`
      data$links <- rbindlist(list(data$links, lk), fill=TRUE)
    }else{
      data$links[link == avialableLink]$`FLOW LIN.` <- data$links[link == avialableLink]$`FLOW LIN.` + data$links[link == linksHvdcin[1]]$`FLOW LIN.`
      data$links <- data$links[!link %in% linksHvdcin]
    }
    NULL
    
  }
  data
}


.removeAreasInList <- function(data, removeArea){
  data[!area%in%removeArea]
}


