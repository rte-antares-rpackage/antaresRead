#' Retrieve links connected to a set of nodes
#'
#' This function find the name of the links connected to a set of node.
#'
#' @param nodes
#' vector containing node names.
#' @param internalOnly
#' if TRUE, only links that connect two nodes from the list are returned. 
#' If not, the function may return links that connect a node from the list with 
#' a node outside the list.
#'
#' @return
#' character vector containing link names.
#'
#' @export
#'
getLinks <- function(nodes, internalOnly=FALSE) {
  l <- getOption("antares")$linkList
  lsplit <- tstrsplit(l, " - ")

  if(internalOnly) idx <- lsplit[[1]] %in% nodes & lsplit[[2]] %in% nodes
  else idx <- lsplit[[1]] %in% nodes | lsplit[[2]] %in% nodes

  l[idx]
}
