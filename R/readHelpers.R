# Add the class "antaresDataList" to a list of tables and attach attributes synthesis,
# timeStep and opts.
.addClassAndAttributes <- function(x, synthesis, timeStep, opts, simplify) {
  for (n in names(x)) {
    class(x[[n]]) <- append(c("antaresDataTable", "antaresData"), class(x[[n]]))
    attr(x[[n]], "type") <- n
    attr(x[[n]], "timeStep") <- timeStep
    attr(x[[n]], "synthesis") <- synthesis
    attr(x[[n]], "opts") <- opts
    
    # Order columns: id columns first
    .setcolorder(x[[n]], .idCols(x[[n]]))
    
  }
  
  class(x) <- append(c("antaresDataList", "antaresData"), class(x))
  attr(x, "timeStep") <- timeStep
  attr(x, "synthesis") <- synthesis
  attr(x, "opts") <- opts
  
  # Simplify the result if possible
  if (simplify & length(x) == 1) x <- x[[1]]
  
  x
}

# Private function that returns the name of de id columns of a table
.idCols <- function(x) {
  intersect(pkgEnv$idVars, names(x))
}

#' Function for preprocessing arguments areas, links, clusters and districts
#' of readAntares.
#'
#' @param list
#'   value of the argument areas, links, clusters or districts
#' @param reference
#'   vector containing the reference list of elements. For "areas", it is the list
#'   of areas from the simulation, etc.
#' @param msg
#'   warning message to display when an element does not exist in the reference
#'   list
#' @return
#' If the argument is empty it returns NULL.
#' If it contains "all", it returns the reference list
#' Else it returns the parameter "list" without the non-existent elements
#' 
#' @noRd
#' 
.checkArg <- function(list, reference, msg) {
  if (is.null(list) || length(list) == 0) return(NULL)
  if (any(list == "all")) return(reference)
  
  res <- intersect(list, reference)
  if (length(res) < length(list)) {
    missingElements <- setdiff(list, reference)
    warning(sprintf(msg, paste(missingElements, collapse = ", ")), call. = FALSE)
    if (length(res) == 0) return(NULL)
  }
  
  res
}

# Private function that reorder columns of a data.table. Contrary to 
# setcolorder, the function does not need the name of all columns.
.setcolorder <- function(x, neworder) {
  neworder <- c(neworder, setdiff(names(x), neworder))
  setcolorder(x, neworder)
}
