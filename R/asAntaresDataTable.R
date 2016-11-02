#' Convert objects to antaresDataTable
#' 
#' @description 
#' This function converts a \code{data.frame} or a \code{data.table} into an
#' \code{antaresDataTable} object.
#' 
#' An \code{antaresDataTable} is simply a \code{data.table} with additional
#' attributes recording the time step, the type of data and the simulation 
#' options.
#'
#' @param x
#'   object to convert to a an \code{antaresDataList}.
#' @param ...
#'   Arguments to be passed to methods.
#'   
#' @return
#'   \code{antaresDataTable} object.
#' 
#' @export
as.antaresDataTable <- function(x, ...) {
  UseMethod("as.antaresDataTable", x)
}

#' @export
as.antaresDataTable.antaresDataTable <- function(x, ...) {
  x
}

#' @rdname as.antaresDataTable
#' 
#' @param synthesis
#'   Does the table contain synthetic results ?
#' @param timeStep
#'   Time step of the data. One of "hourly", "daily", "weekly", "monthly" or "annual".
#' @param type
#'   type of data: for instance "areas", "links", "clusters", etc.
#' @param opts
#'   Simulation options.
#' 
#' @method as.antaresDataTable data.frame
#' @export
as.antaresDataTable.data.frame <- function(x, synthesis, timeStep, type, opts = simOptions(), ...) {
  x <- as.data.table(x)
  timeStep <- match.arg(timeStep, c("hourly", "daily", "weekly", "monthly", "annual"))
  .addClassAndAttributes(x, synthesis, timeStep, opts, type = type)
}

#' @export
as.antaresDataTable.default <- function(x, ...) {
  stop("Cannot convert this object to an 'antaresDataList' object.")
}
