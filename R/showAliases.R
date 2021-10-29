#Copyright © 2016 RTE Réseau de transport d’électricité

#' show aliases for variables
#' 
#' @param names optional vector of alias names. If provided, the full list of 
#'   columns selected by these aliases is displayed. Else only the name and a 
#'   short description of all aliases is displayed.
#' 
#' @export
#' @rdname setAlias
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
    res <- res[tolower(res$name) %in% tolower(names), ]
    print(res)
  }

  invisible(res)
}

#' Add an "alias" for readAntares
#' 
#' @description 
#' Aliases are short names that can be used in the \code{select} parameter in 
#' function \code{\link{readAntares}} to tell the function which columns and/or
#' type of data to import. 
#' 
#' \code{setAlias} can be used to create a new alias. It can be especially
#' useful for package developers to help their users select the data required
#' by their packages.
#'
#' \code{getAlias} return character vector containing columns and/or types of data
#' 
#' \code{showAliases} lists available aliases
#' 
#' @param name Alias name
#' @param desc Short description indicating why the new alias is interesting
#' @param select character vector containing columns and/or types of data to 
#'   import.
#' 
#' @return
#' \code{setAlias} is only used for its side effects. A data.frame with columns 
#' 'name', 'desc' and 'select'. \code{showAliases} invisibly returns a 
#' data.frame with columns "name", "desc" and "select".
#' 
#' 
#' @examples 
#' 
#' # Display the short description of an alias
#' showAliases()
#' 
#' # Display the full description of an alias
#' showAliases("renewable")
#' 
#' getAlias("renewable")
#' 
#' \dontrun{
#' # Create a new alias that imports flows
#' setAlias("test", "short description", c("links", "FLOW LIN.")) 
#' showAliases()
#' }
#' 
#' @export
setAlias <- function(name, desc, select) {
  if (!exists("varAliases", envir = pkgEnv)) {
    assign("pkgEnv", list(), envir = pkgEnv)
  }
  pkgEnv$varAliases[[name]] <- list(desc = desc, select = select)
  
  invisible(TRUE)
}

#' @export
#' @rdname setAlias
getAlias <- function(name) {
  if (!exists("varAliases", envir = pkgEnv)) {
    stop("No aliases defined")
  }
  
  if(!name %in% names(pkgEnv$varAliases)){
    warning("Cannot find '", name, "' alias")
    return(NULL)
  }
  pkgEnv$varAliases[[name]]$select
}
