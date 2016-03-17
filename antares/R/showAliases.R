#' show aliases for variables
#'
#' This function shows aliases for groups of variables.These aliases can be used
#' in the \code{\link{readOutput}} function in the \code{select} argument
#'
#' @export
#'
showAliases <- function() {
  l_ply(names(pkgEnv$varAliases), function(x) {
    cat(sprintf("\"%s\":\n   ", x))
    cat(paste(pkgEnv$varAliases[[x]], collapse = ", "), "\n")
  })
  cat('"allNode" : all node variables\n')
  cat('"allLink" : all link variables\n')

  invisible(pkgEnv$varAliases)
}
