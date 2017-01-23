#Copyright © 2016 RTE Réseau de transport d’électricité

#' get Id columns
#' 
#' \code{getIdCols} return the id columns of an AntaresDataTable
#' 
#' @param x 
#'   an AntaresDataTable.
#'   
#' @return 
#' A character vector containing the name of the id columns of an antaresDataTable
#' 
#' @export
#' 
getIdCols <- function(x = NULL) {
  
  if (!is(x, "antaresDataTable") || is.null(x))
    stop("x has to be an 'antaresDataTable' object")
  
  .idCols(x)
  
}
