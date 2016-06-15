#' Retrieve links connected to a set of areas
#'
#' This function find the name of the links connected to a set of areas. 
#'
#' @param ... 
#'   parameters passed to \code{\link{getAreas}} to construct a set of nodes.
#'   These parameters are used only if argument \code{areas} is \code{NULL}.
#' @param internalOnly
#'   If TRUE, only links that connect two areas from the list are returned. 
#'   If not, the function may return links that connect a area from the list with 
#'   a area outside the list.
#' @param areas
#'   Vector containing area names. It represents the set of areas we interested
#'   in. If \code{NULL}, \code{\link{getAreas}} to create the set of areas.
#' @inheritParams readAntares
#'
#' @return
#' character vector containing link names.
#'
#' @export
#'
getLinks <- function(..., opts = simOptions(), internalOnly=FALSE, areas = NULL) {
  l <- opts$linkList
  lsplit <- tstrsplit(l, " - ")

  if(is.null(areas)) areas <- getAreas(..., opts = opts)
  
  if(internalOnly) idx <- lsplit[[1]] %in% areas & lsplit[[2]] %in% areas
  else idx <- lsplit[[1]] %in% areas | lsplit[[2]] %in% areas

  l[idx]
}
