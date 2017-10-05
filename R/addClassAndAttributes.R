#Copyright © 2016 RTE Réseau de transport d’électricité

#' Transform a table or a list of table in an antaresData object
#' 
#' This function modifies the class and the attributes of its input to transform
#' it in a antaresDataObject.
#' 
#' @param x
#'   data.table or list of data.tables
#' @param synthesis
#'   Does x contain synthetic results ?
#' @param timeStep
#'   time step of x
#' @param opts
#'   antares options used to create 'x'
#' @param simplify
#'   if x is a list with only one element, should the function return only this
#'   element or a list ?
#' @param type
#'   only if x is a data.table: type of data contained in the data.table (areas,
#'   links, etc.)
#' 
#' @return 
#' If x is a data.table, an object of class "antaresDataTable". If x is a list
#' of data.table, an "antaresDataList".
#' 
#' @details 
#' If x is a data.table, the function will add attributes type, timeStep, synthesis
#' and opts and add the class "antaresDataTable" to it.
#' 
#' If x is a list of data.tables, the function first converts each element
#' in antaresDataTable. The type of each element is equal to its name, unless it
#' has already an attribute "type". This is to ensure that if an element is
#' already an antaresDataTable its type is not overwritten.
#' 
#' @noRd
#' 
.addClassAndAttributes <- function(x, synthesis, timeStep, opts, simplify = TRUE, type) {
  
  if (is(x, "data.table")) {
    setattr(x, "class", c("antaresDataTable", "antaresData", "data.table", "data.frame"))
    setattr(x, "type", type)
    setattr(x, "timeStep", timeStep)
    setattr(x, "synthesis", synthesis)
    setattr(x, "opts", opts)
    
    # Order columns: id columns first
    .reorderCols(x)
    
    return(x)
  }
  
  for (n in names(x)) {
    # If an element has already a type, do not overwrite it.
    eltype <- attr(x[[n]], "type")
    if (is.null(eltype)) eltype <- n
    
    x[[n]] <- .addClassAndAttributes(x[[n]], synthesis, timeStep, opts, type = eltype)
    
  }
  
  class(x) <- append(c("antaresDataList", "antaresData"), class(x))
  attr(x, "timeStep") <- timeStep
  attr(x, "synthesis") <- synthesis
  attr(x, "opts") <- opts
  
  # Simplify the result if possible
  if (simplify & length(x) == 1) x <- x[[1]]
  
  x
}
