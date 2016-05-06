.addClassAndAttributes <- function(x, synthesis, timeStep, opts, simplify) {
  for (n in names(x)) {
    class(x[[n]]) <- append(c("antaresDataTable", "antaresData"), class(x[[n]]))
    attr(x[[n]], "type") <- n
    attr(x[[n]], "timeStep") <- timeStep
    attr(x[[n]], "synthesis") <- synthesis
    attr(x[[n]], "opts") <- opts
  }
  
  class(x) <- append(c("antaresDataList", "antaresData"), class(x))
  attr(x, "timeStep") <- timeStep
  attr(x, "synthesis") <- synthesis
  attr(x, "opts") <- opts
  
  # Simplify the result if possible
  if (simplify & length(x) == 1) x <- x[[1]]
  
  x
}
