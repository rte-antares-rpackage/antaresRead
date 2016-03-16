#' Format data PPE-style
#'
#' This function converts an "antaresOutput" object in the data structure used
#' by PPE : instead of having one table for nodes, one for links and one for
#' clusters, the function creates a list with one element per node. Each element
#' is a data.table containing the data about the node and one column per cluster
#' of the node containing the production of this cluster.
#'
#' @param x
#'   object of class "antaresOutput" or data.table created by the function
#'   \code{\link{importOutput}}
#' @param nodes
#'   (optional) character vector containing the name of nodes to keep in the
#'   final object
#'
#' @returns a list of data.tables with one element per node
#'
#' @export
#'
extractDataList <- function(x, nodes) {
  if (is.data.frame(x) && !is.null(x$node)) x <- list(nodes = x)

  if (is.null(x$nodes)) stop("Object does not contain nodes data.")

  if (!missing(nodes)) x$nodes <- x$nodes[node %in% nodes]

  dataList <- dlply(x$nodes, .(node), data.table)
  nodeList <- names(dataList)

  # Add clusters production
  if (!is.null(x$clusters)) {
    clusters <- dlply(x$clusters, .(node), data.table)

    for (n in names(clusters)) {
      if(!is.null(dataList[[n]])) {
        tmp <- dcast(clusters[[n]], timeId ~ cluster, value.var = "MWh")
        dataList[[n]] <- merge(dataList[[n]], tmp, by = "timeId")
      }
    }
  }

  # Add flows
  if (!is.null(x$links)) {
    for (n in nodeList) {
      links <- x$links[link %in% getLinks(n)]

      if(nrow(links) > 0) {
        tmp <- dcast(links, timeId ~ link, value.var = "FLOW LIN.")
        dataList[[n]] <- merge(dataList[[n]], tmp, by = "timeId")
      }
    }
  }

  dataList$nodeList <- nodeList

  dataList
}
