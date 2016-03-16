#' Import clusters description
#'
#' This function creates a table containing description of the clusters of an
#' antares study: type, capacity, marginal cost, etc.
#'
#' @return
#' A data.table with one line per cluster. The columns of the data.table may
#' change between different projects, but there will always be a column
#' "cluster" containing the name of the cluster and a column "node" containing the
#' name of the node it belongs to.
#'
#' @examples
#' readClusterDesc()
#'
#' @export
#'
readClusterDesc <- function() {
  opts <- getOption("antares")
  path <- file.path(opts$path, "../../input/thermal/clusters")

  nodes <- list.files(path)

  res <- ldply(nodes, function(x) {
    clusters <- readIniFile(file.path(path, x, "list.ini"))

    if (length(clusters) == 0) return(NULL)

    clusters <- ldply(clusters, as.data.frame)
    clusters$.id <- NULL
    clusters$node <- x

    clusters[, c(ncol(clusters), 1:(ncol(clusters) - 1))]
  })

  res <- as.data.table(res)
  setnames(res, "name", "cluster")

  res
}
