#' Select and exclude nodes
#' 
#' \code{getNodes} is a utility function that builds a list of nodes by using 
#' regular expressions to select and/or exclude nodes
#' 
#' @param select 
#'   Character vector. If \code{regexpSelect} is TRUE, this vector is
#'   interpreted as a list of regular expressions. Else it is interpreted as a
#'   list of node names. If \code{NULL}, all nodes are selected
#' @param exclude
#'   Character vector. If \code{regexpExclude} is TRUE, this vector is
#'   interpreted as a list of regular expressions and each node validating one
#'   of them is excluded. Else it is interpreted as list of node names to 
#'   exclude. If \code{NULL}, not any node is excluded.
#' @param regexpSelect
#'   Is \code{select} a list of regular expressions ?
#' @param regexpExclude
#'   Is \code{exclude} a list of regular expressions ?
#' @param ignore.case Should the case be ignored when evaluating the regular
#' expressions ?
#' @inheritParams readAntares
#'   
#' @return 
#' A character vector containg the name of the nodes satisfying the rules
#' defined by the parameters.
#' 
#' @export
#' 
getNodes <- function(select = NULL, exclude = NULL, regexpSelect = TRUE, 
                     regexpExclude = TRUE, opts = getOption("antares"),
                     ignore.case = TRUE) {
  
  allNodes <- opts$nodeList
  nodes <- c()
  
  if (is.null(select) | identical(select, "all")) {
    nodes <- allNodes
  } else {
    if (regexpSelect) {
      for (regexp in select) {
        selectedNodes <- allNodes[grepl(regexp, allNodes, ignore.case = ignore.case)]
        nodes <- union(nodes, selectedNodes)
      }
    } else {
      nodes <- allNodes[allNodes %in% select]
    }
  }
  
  if (!is.null(exclude)) {
    if (regexpExclude) {
      for (regexp in exclude) {
        nodes <- nodes[!grepl(regexp, nodes, ignore.case = ignore.case)]
      }
    } else {
      nodes <- nodes[!nodes %in% exclude]
    }
  }
  
  nodes
}
