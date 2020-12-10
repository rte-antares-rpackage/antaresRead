#' Mcyear aggregation weigthed by wd
#' 
#' @param x \code{antaresData} data import with antaresRead
#' @param fun \code{function} function to use
#' @param ... \code{args} others args pass to fun
#' 
#' @examples
#' \dontrun{
#'   data <- readAntares(areas = 'all', mcYears = 'all')
#'   ponderateMcAggregation(data, fun = weighted.mean, w = c(.1, .9))
#' 
#'   
#' }
#' 
#' @export
#' 
ponderateMcAggregation <- function(x, fun = weighted.mean, ...) {
  
  if(!is.null(w)){
    
    if(length(w) != length(unique(x$mcYear))){
      stop("You must have identical length for wd and mcYear")
    }
  }
  attrs <- attributes(x)
  idVars <- setdiff(.idCols(x), "mcYear")
  x[, mcYear := NULL]
  x <- x[,lapply(.SD,fun, ... = ...),by=idVars]
  #reset attributes
  .addClassAndAttributes(x, TRUE, attrs$timeStep, attrs$opts, type = attrs$type)
}
