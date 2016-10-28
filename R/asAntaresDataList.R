#' Convert objects to antaresDataTable
#' 
#' @description 
#' This function converts a list of tables or table into an
#' \code{antaresDataList} object.
#' 
#' An \code{antaresDataList} is a list of tables of class\code{antaresDataTable}.
#' It also has attributes that store the time step, the type of data and the 
#' simulation options.
#' 
#' 
#' @param x
#'   Data.frame or data.table to convert to a an antaresDataTable.
#' @param ...
#'   Arguments to be passed to methods.
#'   
#' @return
#'   \code{antaresDataList} object.
#' 
#' @export
as.antaresDataList <- function(x, ...) {
  UseMethod("as.antaresDataList", x)
}

#' @export
as.antaresDataList.antaresDataList <- function(x, ...) {
  x
}


#' @rdname as.antaresDataList
#' @param name
#'   name of the table in the final object. If \code{NULL}, the type of the data
#'   is used.
#' @export
as.antaresDataList.antaresDataTable <- function(x, name = NULL, ...) {
  attrs <- attributes(x)
  
  if (is.null(name)) name <- attrs$type
  x <- list(x)
  names(x) <- name
  
  .addClassAndAttributes(x, attrs$synthesis, attrs$timeStep, attrs$opts, FALSE)
}

#' @rdname as.antaresDataList
#' @param synthesis
#'   Does the table contain synthetic results ?
#' @param timeStep
#'   Time step of the data. One of "hourly", "daily", "weekly", "monthly" or "annual".
#' @param type
#'   type of data: for instance "areas", "links", "clusters", etc.
#' @param opts
#'   Simulation options.
#' @export
as.antaresDataList.data.frame <- function(x, synthesis, timeStep, type, 
                                          opts = simOptions(), name = type, ...) {
  x <- as.antaresDataTable(x, synthesis, timeStep, type, opts)
  as.antaresDataList(x, name)
}

#' @export
as.antaresDataList.list <- function(x, synthesis, timeStep, opts = simOptions(), ...) {
  .addClassAndAttributes(x, synthesis, timeStep, opts, FALSE)
}

#' @export
as.antaresDataList.default <- function(x, ...) {
  stop("Cannot convert this object to an 'antaresDataList'.")
}
