#Copyright © 2016 RTE Réseau de transport d’électricité

#' Retrieve links connected to a set of areas
#'
#' This function finds the names of the links connected to a set of areas. 
#'
#' @param areas
#'   Vector containing area names. It represents the set of areas we are interested
#'   in. If \code{NULL}, all areas of the study are used.
#' @param exclude
#'   Vector containing area names. If not \code{NULL}, all links connected to 
#'   one of these areas are omitted.
#' @param internalOnly
#'   If \code{TRUE}, only links that connect two areas from parameter \code{areas} are returned. 
#'   If not, the function also returns all the links that connect an area from the list with 
#'   an area outside the list.
#' @param namesOnly
#'   If \code{TRUE}, the function returns a vector with link names, else it
#'   returns a table containing the name, the origin and the destination of each
#'   selected link.
#' @param withDirection
#'   Used only if \code{namesOnly = FALSE}. If \code{FALSE}, then the function
#'   returns a table with one line per link, containing the link name, the 
#'   origin and the destination of the link. If \code{TRUE}, then it returns a
#'   table with columns \code{area}, \code{link}, \code{to} and \code{direction}
#'   which is equal is equal to
#'   1 if the link connects \code{area} to \code{to} and -1 if it connects
#'   \code{to} to \code{area}.
#'   The column \code{area} contains only areas that are compatible with parameters 
#'   \code{areas} and \code{exclude}. Note that the same link can appear twice 
#'   in the table with different directions.  
#'   
#' @inheritParams readAntares
#'
#' @return
#' If \code{namesOnly = TRUE} the function returns a vector containing link names 
#' 
#' If \code{namesOnly = FALSE} and \code{withDirection = FALSE}, it returns a
#' \code{data.table} with \strong{exactly one line per link} and with three columns:
#' \item{link}{Link name}
#' \item{from}{First area connected to the link}
#' \item{to}{Second area connected to the link}
#' 
#' If \code{namesOnly = FALSE} and \code{withDirection = TRUE}, it returns a
#' \code{data.table} with \strong{one or two lines per link} and with four columns:
#' \item{area}{Area name}
#' \item{link}{Link name}
#' \item{to}{Area connected to \code{area} by \code{link}}
#' \item{direction}{1 if the link connects \code{area} to \code{to} else -1}
#' 
#' @examples 
#' \dontrun{
#' 
#' # Get all links of a study
#' getLinks()
#' 
#' # Get all links with their origins and destinations
#' getLinks(namesOnly = FALSE)
#' 
#' # Get all links connected to French areas (assuming their names contain "fr")
#' getLinks(getAreas("fr"))
#' 
#' # Same but with only links connecting two French areas
#' getLinks(getAreas("fr"), internalOnly = TRUE)
#' 
#' # Exclude links connecting real areas with pumped storage virtual areas
#' # (assuming their names contain "psp")
#' getLinks(getAreas("fr"), exclude = getAreas("psp"))
#' 
#' }
#'
#' @export
#'
getLinks <- function(areas = NULL, exclude = NULL, opts = simOptions(), 
                     internalOnly=FALSE, namesOnly = TRUE, 
                     withDirection = FALSE) {

  if (is.null(areas)) areas <- getAreas(opts = opts)
  
  # There are no links -> return NULL
  if (is.null(opts$linksDef$from)) {
    return(NULL)
  }
  
  if (internalOnly) links <- opts$linksDef[from %in% areas & to %in% areas]
  else links <- opts$linksDef[from %in% areas | to %in% areas]
  
  links <- links[!from %in% exclude & !to %in% exclude]
  
  if (namesOnly) return(links$link)
  if (!withDirection) return(links)

  outward_links <- links[, .(area = from, link, to = to)]
  outward_links[, direction := 1]

  inward_links <- links[, .(area = to, link, to = from)]
  inward_links[, direction := -1]

  links <- rbind(outward_links, inward_links)
  
  links[area %in% areas]
}
