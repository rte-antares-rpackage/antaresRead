# Private function that returns the name of de id columns of a table
.idCols <- function(x) {
  intersect(pkgEnv$idVars, names(x))
}

# Private function that reorder columns of a data.table. Contrary to 
# setcolorder, the function does not need the name of all columns.
.setcolorder <- function(x, neworder) {
  neworder <- c(neworder, setdiff(names(x), neworder))
  setcolorder(x, neworder)
}
