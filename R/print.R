#' @export
print.antaresDataTable <- function(x, ...) {
  cat(sprintf("'antaresDataTable' object with dimension %s x %s\n", nrow(x), ncol(x)))
  cat(sprintf("Type: %s\nTimeStep: %s\nSynthesis: %s\n",
              attr(x, "type"), attr(x, "timeStep"), attr(x, "synthesis")))
  data.table:::print.data.table(x)
}

#' @export
print.antaresDataList <- function(x, ...) {
  elements <- names(x)
  N <- length(elements)
  if (N == 1) mytext <- paste("element", elements)
  else {
    mytext <- paste("elements", paste(elements[-N], collapse = ", "), "and", elements[N])
  }
  
  cat(sprintf("'antaresDataList' object with %s\n",mytext))
  cat(sprintf("TimeStep: %s\nSynthesis: %s\n",
              attr(x, "type"), attr(x, "timeStep"), attr(x, "synthesis")))
  
  for (n in names(x)) {
    cat(paste0("\n.$", n, "\n"))
    data.table:::print.data.table(x[[n]])
  }
  
}

#' @export
print.simOptions <- function(x, ...) {

  cat(sprintf("Antares simulation '%s'\nMode %s\n", x$name, x$mode))
  cat(sprintf("\nContent:\n - synthesis: %s\n - year by year: %s\n - MC Scenarios: %s\n",
              x$synthesis, x$yearByYear, x$scenarios))
  cat(sprintf(" - Number of nodes: %s\n - Number of districts: %s\n - Number of links: %s\n",
              length(x$nodeList), length(x$setList), length(x$linkList)))
  
  if (x$yearByYear) cat(sprintf(" - Number of Monte-Carlo years: %s\n", x$mcYears))

}