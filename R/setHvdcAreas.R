#Copyright © 2016 RTE Réseau de transport d’électricité

#' Set hvdc areas
#' 
#'
#' @description This function add hvdc attribute 
#' 
#' @param data \code{antaresData} or \code{antaresDatalist} data.
#' @param areas \code{character} hvdc areas list.
#' 
#' 
#' 
#' @return  Object of class "antaresDataList" is returned. 
#' It is a list of data.tables, each element representing one type of element (areas, links, clusters)
#' 
#' 
#' @examples
#' \dontrun{
#' 
#' library(antaresRead)
#' opts <- setSimulationPath('mypath', 1)
#' myAreaOutput <- readAntares(areas = "all", links = "all")
#' myAreaOutput <- setHvdcAreas(myAreaOutput, "y_dsr")
#' 
#' 
#' }
#' 
#' @export
setHvdcAreas <- function(data, areas){
  if('antaresDataList' %in% class(data)){
    if(is.null(data$areas)){
      stop("You must import areas to use setHvdcAreas")
    }
   if(any(!areas%in% unique(data$areas$area) )){
     stop(paste0("All areas specify must be in data areas, missing :", 
                 paste0(areas[!areas%in% unique(data$areas$area)],
                        collapse = " ; ")))
   }
     attr(data$areas, "hvdcAreas") <- areas
  }else{
  if(attributes(data)$type != "areas"){
    stop("You must import areas to use setHvdcAreas")
  }
  
  if(any(!areas%in% unique(data$area) )){
    stop(paste0("All areas specify must be in data areas, missing :", 
                paste0(areas[!areas%in% unique(data$area)],
                                                                             collapse = " ; ")))
  }
  attr(data, "hvdcAreas") <- areas
  }
  data
}


