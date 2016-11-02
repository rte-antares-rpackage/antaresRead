#Copyright © 2016 RTE Réseau de transport d’électricité

# Private function that returns the name of de id columns of a table
.idCols <- function(x) {
  intersect(pkgEnv$idVars, names(x))
}

#' reorder the columns of a data table
#' 
#' This function puts the id columns in the order defined by variable pkgEnv$idVars,
#' and then the other columns in the same order as in 'x'
#' 
#' @noRd
#' 
.reorderCols <- function(x) {
  idCols <- .idCols(x)
  neworder <- c(idCols, setdiff(names(x), idCols))
  setcolorder(x, neworder)
  invisible(x)
}
