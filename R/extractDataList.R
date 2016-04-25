#' Format data PPSE-style
#'
#' This function converts an "readAntares" object in the data structure used
#' by PPSE : instead of having one table for nodes, one for links and one for
#' clusters, the function creates a list with one element per node. Each element
#' is a data.table containing the data about the node and one column per cluster
#' of the node containing the production of this cluster.
#'
#' @param x
#'   object of class "antaresData" or "antaresTable" created by the function
#'   \code{\link{readAntares}}
#' @param nodes
#'   character vector containing the name of nodes to keep in the
#'   final object. If \code{NULL}, all nodes are kept in the final object.
#' @inheritParams readAntares
#'
#' @return a list of data.tables with one element per node. The list also
#' contains an element named "nodeList" containing the name of nodes in the
#' object and a table called "infos" that contains for each node the number
#' of variables of différent type (values, details, link).
#'
#' @export
#'
extractDataList <- function(x, nodes=NULL) {
  # Check arguments
  opts <- simOptions(x)
  
  if (is(x, "antaresTable") && !is.null(x$node)) x <- list(nodes = x)

  if (is.null(x$nodes)) stop("'x' does not contain nodes data.")
  
  
  
  if (!is.null(nodes)) {
    missingNodes <- nodes[! nodes %in% x$nodes$node]
    for (m in missingNodes) warning("Node '", m, "' missing in 'x'")
    
    x$nodes <- x$nodes[node %in% nodes]
    
    if (nrow(x$nodes) == 0) {
      stop("Argument 'nodes' does not contain any node name present in 'x'")
    }
  }

  # Create variable Scenario equal to 0 if synthesis or mcYear if not.
  if (is.null(x$nodes$mcYear)) {
    x$nodes$Scenario <- 0
    if (!is.null(x$clusters)) x$clusters$Scenario <- 0
    if (!is.null(x$links)) x$links$Scenario <- 0
  } else {
    x$nodes$Scenario <- x$nodes$mcYear
    if (!is.null(x$clusters)) x$clusters$Scenario <- x$clusters$mcYear
    if (!is.null(x$links)) x$links$Scenario <- x$links$mcYear
    x$nodes$mcYear <- NULL
  }
  
  # Add time variable
  x$nodes$Dtime <- .timeIdToDate(x$nodes$timeId, attr(x, "timeStep"), opts)

  # Base structure: one table per node
  dataList <- dlply(x$nodes, .(node), data.table)

  # Additional elements: list of nodes and info about the content of each table
  nodeList <- names(dataList)
  info <- data.table(node = nodeList,
                     valuesLength = ncol(x$nodes),
                     detailsLength = 0,
                     linkLength = 0)

  # Add cluster production and flows if available to each element of dataList
  for (n in nodeList) {

    # Clusters
    if (!is.null(x$clusters)) {
      cl <- x$clusters[node == n]

      if (nrow(cl) > 0) {
        tmp <- dcast(cl, Scenario + timeId ~ cluster, value.var = "MWh")
        dataList[[n]] <- merge(dataList[[n]], tmp, by = c("Scenario", "timeId"))

        info[node == n, detailsLength := ncol(tmp) - 2]
      }
    }

    # Flows
    if (!is.null(x$links) && !is.null(x$links$`FLOW LIN.`)) {
      links <- x$links[link %in% getLinks(n)]

      if(nrow(links) > 0) {
        tmp <- dcast(links, Scenario + timeId ~ link, value.var = "FLOW LIN.")
        dataList[[n]] <- merge(dataList[[n]], tmp, by = c("Scenario", "timeId"))

        info[node == n, linkLength := ncol(tmp) - 2]
      }
    }
  }

  dataList$nodeList <- nodeList
  dataList$infos <- info

  dataList
}
