#' Retrieve links connected to a set of areas
#'
#' This function find the name of the links connected to a set of areas. 
#'
#' @param areas
#'   Vector containing area names. It represents the set of areas we are interested
#'   in. If \code{NULL}, all areas of the study are used.
#' @param exclude
#'   Vector containing area names. If not \code{NULL}, all links connected to 
#'   one of these areas are omited.
#' @param internalOnly
#'   If TRUE, only links that connect two areas from parameter \code{area} are returned. 
#'   If not, the function may return links that connect an area from the list with 
#'   an area outside the list.
#' @inheritParams readAntares
#'
#' @return
#' character vector containing link names.
#' 
#' @examples 
#' \dontrun{
#' # Get all links of a study
#' getLinks()
#' 
#' # Get all links connected to french areas (assuming their name contains "fr")
#' getLinks(getAreas("fr"))
#' 
#' # Same but with only links connecting two french areas
#' getLinks(getAreas("fr"), internalOnly = TRUE)
#' 
#' # Exclude links connecting real areas with pumped storage virtual areas
#' # (assuming their name contains "psp")
#' getLinks(getAreas("fr"), exclude = getAreas("psp"))
#' 
#' }
#'
#' @export
#'
getLinks <- function(areas = NULL, exclude = NULL, opts = simOptions(), 
                     internalOnly=FALSE) {
  l <- opts$linkList
  lsplit <- tstrsplit(l, " - ")

  if(is.null(areas)) areas <- getAreas(opts = opts)
  
  if(internalOnly) idx <- lsplit[[1]] %in% areas & lsplit[[2]] %in% areas
  else idx <- lsplit[[1]] %in% areas | lsplit[[2]] %in% areas

  idx <- idx & !lsplit[[1]] %in% exclude & !lsplit[[2]] %in% exclude
  l[idx]
}
