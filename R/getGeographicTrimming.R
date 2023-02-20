#Copyright © 2016 RTE Réseau de transport d’électricité

#' Read geographic trimming (filtering) options
#'
#' @param areas Character. vector of areas
#' @param links Logical. if TRUE, return filtering options for all links starting from selected areas
#' @param opts List. simulation options
#'
#' @return list of filtering options for areas and links
#'
#' @export
getGeographicTrimming <- function(areas = NULL, links = TRUE, opts = simOptions()){
  if (is.null(areas)) stop("You need to select at least one area.")
  if (length(areas) == 1 && areas == "all") areas <- opts$areaList

  res <- list()
  
  if(!is.null(areas)){
    areaData <- lapply(as.list(areas), .readPropertiesFunction, opts2 = opts, type = "areas")
    names(areaData) <- areas
    res$areas <- areaData
  }
  
  if (links){
    linkData <- lapply(as.list(areas), .readPropertiesFunction, opts2 = opts, type = "links")
    names(linkData) <- areas
    linkData <- unlist(linkData, recursive = F)
    names(linkData) <- gsub("\\.", " - ", names(linkData))
    res$links <- linkData
  }

  res
}

.readPropertiesFunction <- function(x, opts2, type){
  if (type == "areas"){
    if (opts2$typeLoad != "api"){
      inputPath <- file.path(opts2$inputPath, "areas", x, "optimization.ini")
      readIniFile(inputPath)$filtering
    } else readIni(file.path("input", "areas", x, "optimization"))$filtering
    
  } else if (type == "links"){
    if (opts2$typeLoad != "api"){
      inputPath <- file.path(opts2$inputPath, "links", x, "properties.ini")
      lapply(as.list(readIniFile(inputPath)), function(x){x[grep("filter", names(x))]})
    } else lapply(as.list(readIni(file.path("input", "links", x, "properties"))), function(x){x[grep("filter", names(x))]})
  }
} 

