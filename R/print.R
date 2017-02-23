#Copyright © 2016 RTE Réseau de transport d’électricité

#' @export
print.antaresDataTable <- function(x, ...) {
  cat(sprintf("'antaresDataTable' object with dimension %s x %s\n", nrow(x), ncol(x)))
  cat(sprintf("Type: %s\nTimeStep: %s\nSynthesis: %s\n",
              attr(x, "type"), attr(x, "timeStep"), attr(x, "synthesis")))
  NextMethod()
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
  
  # Overwrite print.antaresDataTable so that informations like synthesis and 
  # timestep are not printed again.
  print.antaresDataTable <- function(x, ...) NextMethod()
  
  for (n in names(x)) {
    cat(sprintf("\n.$%s (%s x %s)\n", n, nrow(x[[n]]), ncol(x[[n]])))
    print(x[[n]])
  }
  
}

#' @export
print.simOptions <- function(x, ...) {
  cat(sprintf("Antares project '%s' (%s)\n", x$studyName, x$studyPath))
  cat(sprintf("Simulation '%s'\n", x$name))
  cat(sprintf("Mode %s\n\nContent:\n", x$mode))
  cat(sprintf(" - synthesis: %s\n - year by year: %s\n - MC Scenarios: %s\n",
              x$synthesis, x$yearByYear, x$scenarios))
  cat(sprintf(" - Number of areas: %s\n - Number of districts: %s\n - Number of links: %s\n",
              length(x$areaList), length(x$districtList), length(x$linkList)))
  
  if (!is.null(x$mcYears)) cat(sprintf(" - Number of Monte-Carlo years: %s\n", length(x$mcYears)))

}
