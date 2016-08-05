#' show aliases for variables
#'
#' This function shows aliases for groups of variables.These aliases can be used
#' in the \code{\link{readAntares}} function in the \code{select} argument
#' 
#' @return
#' A named list. The names of the list correspond to the aliases and each element
#' is a vector of variable names.
#' 
#' @examples 
#' showAliases()
#' 
#' @export
#'
showAliases <- function() {
  l_ply(names(pkgEnv$varAliases), function(x) {
    cat(sprintf("\"%s\":\n   ", x))
    cat(paste(pkgEnv$varAliases[[x]], collapse = ", "), "\n")
  })
  cat('"allArea" : all area variables\n')
  cat('"allLink" : all link variables\n')

  invisible(pkgEnv$varAliases)
}
