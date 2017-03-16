#Copyright © 2016 RTE Réseau de transport d’électricité

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
showAliases <- function(names = NULL) {
  desc <- vapply(pkgEnv$varAliases, function(x) x$desc, character(1))
  select <- vapply(pkgEnv$varAliases, FUN.VALUE = character(1), function(x) {
    paste(x$select, collapse = ", ")
  })
  
  res <- data.frame(
    name = names(pkgEnv$varAliases),
    desc = desc,
    select = select,
    row.names = NULL
  )
  
  if (is.null(names)) {
    print(res[, c("name", "desc")])
  } else {
    res <- res[res$name %in% names, ]
    print(res)
  }

  invisible(res)
}


addAlias <- function(name, desc, select) {
  if (!exists("varAliases", envir = pkgEnv)) {
    assign("pkgEnv", list(), envir = pkgEnv)
  }
  pkgEnv$varAliases[[name]] <- list(desc = desc, select = select)
}
