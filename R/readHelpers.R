.addClassAndAttributes <- function(x, synthesis, timeStep, opts, simplify) {
  for (n in names(x)) {
    class(x[[n]]) <- append(c("antaresDataTable", "antaresData"), class(x[[n]]))
    attr(x[[n]], "type") <- n
    attr(x[[n]], "timeStep") <- timeStep
    attr(x[[n]], "synthesis") <- synthesis
    attr(x[[n]], "opts") <- opts
    
    # Order columns: id columns first
    idCols <- intersect(pkgEnv$idVars, names(x[[n]]))
    .setcolorder(x[[n]], idCols)
    
  }
  
  class(x) <- append(c("antaresDataList", "antaresData"), class(x))
  attr(x, "timeStep") <- timeStep
  attr(x, "synthesis") <- synthesis
  attr(x, "opts") <- opts
  
  # Simplify the result if possible
  if (simplify & length(x) == 1) x <- x[[1]]
  
  x
}

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
