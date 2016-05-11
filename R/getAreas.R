#' Select and exclude areas
#' 
#' \code{getAreas} and \code{getDistricts} are utility functions that builds 
#' list of areas or districts by using regular expressions to select and/or 
#' exclude areas/districts
#' 
#' @param select 
#'   Character vector. If \code{regexpSelect} is TRUE, this vector is
#'   interpreted as a list of regular expressions. Else it is interpreted as a
#'   list of area names. If \code{NULL}, all areas are selected
#' @param exclude
#'   Character vector. If \code{regexpExclude} is TRUE, this vector is
#'   interpreted as a list of regular expressions and each area validating one
#'   of them is excluded. Else it is interpreted as list of area names to 
#'   exclude. If \code{NULL}, not any area is excluded.
#' @param regexpSelect
#'   Is \code{select} a list of regular expressions ?
#' @param regexpExclude
#'   Is \code{exclude} a list of regular expressions ?
#' @param ignore.case Should the case be ignored when evaluating the regular
#' expressions ?
#' @inheritParams readAntares
#'   
#' @return 
#' A character vector containg the name of the areas/districts satisfying the 
#' rules defined by the parameters.
#' 
#' @seealso \code{\link{getLinks}}
#' 
#' @export
#' 
getAreas <- function(select = NULL, exclude = NULL, withClustersOnly = FALSE,
                     regexpSelect = TRUE, 
                     regexpExclude = TRUE, opts = simOptions(),
                     ignore.case = TRUE) {
  
  allAreas <- if(withClustersOnly) opts$areasWithClusters else opts$areaList
  
  .getAreas(select, exclude, regexpSelect, regexpExclude, ignore.case, allAreas)
  
}

#' @rdname getAreas
#' @export
getDistricts <- function(select = NULL, exclude = NULL, regexpSelect = TRUE, 
                     regexpExclude = TRUE, opts = simOptions(),
                     ignore.case = TRUE) {
  
  .getAreas(select, exclude, regexpSelect, regexpExclude, ignore.case, opts$setList)
  
}

.getAreas <- function(select, exclude, regexpSelect, regexpExclude,
                      ignore.case, allAreas) {
  areas <- c()
  
  if (is.null(select) | identical(select, "all")) {
    areas <- allAreas
  } else {
    if (regexpSelect) {
      for (regexp in select) {
        selectedAreas <- allAreas[grepl(regexp, allAreas, ignore.case = ignore.case)]
        areas <- union(areas, selectedAreas)
      }
    } else {
      areas <- allAreas[allAreas %in% select]
    }
  }
  
  if (!is.null(exclude)) {
    if (regexpExclude) {
      for (regexp in exclude) {
        areas <- areas[!grepl(regexp, areas, ignore.case = ignore.case)]
      }
    } else {
      areas <- areas[!areas %in% exclude]
    }
  }
  
  areas
}
