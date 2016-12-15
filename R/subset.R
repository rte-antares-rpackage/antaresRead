#' Subset an antaresDataList
#' 
#' Subset method for \code{antaresDataList}.
#' 
#' @param x Object of class \code{antaresDataList} created with 
#'   \code{\link{antaresDataList}}.
#' @param y A table containing at least one of the columns "area", "timeId" or 
#'   "mcYear". If it is not \code{NULL}, then only tuples \code{(area, timeId,
#'   mcYear)} present in this table are kept.
#' @param areas Vector of area names to keep in the result. If \code{NULL}, all 
#'   areas are kept.
#' @param timeIds Vector of time ids to keep. If \code{NULL}, all time ids are 
#'   kept.
#' @param mcYears Vector of monte-carlo years to keep. If \code{NULL}, all time 
#'   ids are kept.
#' @param ... Currently unused.
#' 
#' @return 
#' A filtered \code{antaresDataList}.
#' 
#' @export
#' 
subset.antaresDataList <- function(x, y = NULL, areas = NULL, timeIds = NULL, mcYears = NULL, ...) {
  for (n in names(x)) {
    filter <- TRUE
    if (!is.null(areas)) {
      if (!is.null(x[[n]]$area)) {
        filter <- filter & x[[n]]$area %in% areas
      } else if (!is.null(x[[n]]$link)) {
        filter <- filter & x[[n]]$link %in% getLinks(areas, opts = simOptions(x))
      }
    }
    if (!is.null(timeIds)) {
      filter <- filter & x[[n]]$timeId %in% timeIds
    }
    if (!is.null(mcYears)) {
      filter <- filter & x[[n]]$mcYear %in% mcYears
    }
    x[[n]] <- x[[n]][filter, ]
  }
  
  x
}
