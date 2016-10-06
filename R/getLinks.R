#Copyright © 2016 RTE Réseau de transport d’électricité

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
#' @param namesOnly
#'   If \code{TRUE}, the function returns a vector with link names, else it
#'   returns a table containing the name, the origin and the destination of each
#'   selected link.
#' @param withDirection
#'   Used only if \code{namesOnly = TRUE}. If \code{FALSE}, then the function
#'   returns a table with one line per link, containing the link name the 
#'   origin and the destination of the link. If \code{FALSE}, then it returns a
#'   table with columns area, link, to and direction which is equal is equal to
#'   1 if the link connects "area" to "to" and -1 if it connects "to" to "area".
#'   The column area contains only areas that are compatible with parameters 
#'   \code{areas} and \code{exclude}. Note that the same link can appear twice 
#'   in the table with different directions.  
#'   
#' @inheritParams readAntares
#'
#' @return
#' If \code{namesOnly = TRUE} the function returns a vector containing link names 
#' 
#' If \code{namesOnly = FALSE} and \code{withDirection = FALSE}, it returns a
#' \code{data.table} with exactly one line per link and with three columns:
#' \item{link}{link name}
#' \item{from}{first area connected to the link}
#' \item{to}{second area connected to the link}
#' 
#' If \code{namesOnly = FALSE} and \code{withDirection = FALSE}, it returns a
#' \code{data.table} with one or two lines per link and with four columns:
#' \item{area}{Area name}
#' \item{link}{Link name}
#' \item{to}{Area connected to "area" by "link"}
#' \item{direction}{1 if the link connects "area" to "to" else -1}
#' 
#' @examples 
#' \dontrun{
#' # Get all links of a study
#' getLinks()
#' 
#' # Get all links with their origin and their destination
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
                     internalOnly=FALSE, namesOnly = TRUE, 
                     withDirection = FALSE) {

  if(is.null(areas)) areas <- getAreas(opts = opts)
  
  if (internalOnly) links <- opts$linksDef[from %in% areas & to %in% areas]
  else links <- opts$linksDef[from %in% areas | to %in% areas]
  
  links <- links[!from %in% exclude & !to %in% exclude]
  
  if (namesOnly) return(links$link)
  if (!withDirection) return(links)
  
  links <- rbind(links[, .(area = from, link, to = to, direction = 1)], 
                 links[, .(area = to, link, to = from, direction = -1)])
  
  links[area %in% areas]
}
