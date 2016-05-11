#' Retrieve links connected to a set of areas
#'
#' This function find the name of the links connected to a set of areas.
#'
#' @param internalOnly
#' if TRUE, only links that connect two areas from the list are returned. 
#' If not, the function may return links that connect a area from the list with 
#' a area outside the list.
#' @inheritParams getAreas
#' @inheritParams readAntares
#'
#' @return
#' character vector containing link names.
#'
#' @export
#'
getLinks <- function(select = NULL, exclude = NULL, regexpSelect = TRUE, 
                     regexpExclude = TRUE, internalOnly=FALSE, 
                     opts = simOptions(), ignore.case = TRUE) {
  l <- opts$linkList
  lsplit <- tstrsplit(l, " - ")

  areas <- getAreas(select, exclude, regexpSelect, regexpExclude, opts)
  
  if(internalOnly) idx <- lsplit[[1]] %in% areas & lsplit[[2]] %in% areas
  else idx <- lsplit[[1]] %in% areas | lsplit[[2]] %in% areas

  l[idx]
}
