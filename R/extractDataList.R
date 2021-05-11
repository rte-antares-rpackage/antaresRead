#Copyright © 2016 RTE Réseau de transport d’électricité

#' Format data PPSE-style
#'
#' This function converts an "readAntares" object in the data structure used
#' by PPSE : instead of having one table for areas, one for links and one for
#' clusters, the function creates a list with one element per area. Each element
#' is a data.table containing the data about the area and one column per cluster
#' of the area containing the production of this cluster.
#'
#' @param x
#'   object of class "antaresData" or "antaresTable" created by the function
#'   \code{\link{readAntares}}
#' @param areas
#'   character vector containing the name of areas to keep in the
#'   final object. If \code{NULL}, all areas are kept in the final object.
#'
#' @return a list of data.tables with one element per area. The list also
#' contains an element named "areaList" containing the name of areas in the
#' object and a table called "infos" that contains for each area the number
#' of variables of different type (values, details, link).
#'
#' @export
#'
extractDataList <- function(x, areas = NULL) {
  # Check arguments
  opts <- simOptions(x)
  
  if (is(x, "antaresDataTable") && !is.null(x$area)) x <- list(areas = x)

  if (is.null(x$areas)) stop("'x' does not contain areas data.")
  
  
  
  if (!is.null(areas)) {
    missingAreas <- areas[! areas %in% x$areas$area]
    for (m in missingAreas) warning("Area '", m, "' missing in 'x'")
    
    x$areas <- x$areas[area %in% areas]
    
    if (nrow(x$areas) == 0) {
      stop("Argument 'areas' does not contain any area name present in 'x'")
    }
  }

  # Create variable Scenario equal to 0 if synthesis or mcYear if not.
  if (is.null(x$areas$mcYear)) {
    x$areas$Scenario <- 0
    if (!is.null(x$clusters)) x$clusters$Scenario <- 0
    if (!is.null(x$links)) x$links$Scenario <- 0
  } else {
    x$areas$Scenario <- x$areas$mcYear
    if (!is.null(x$clusters)) x$clusters$Scenario <- x$clusters$mcYear
    if (!is.null(x$links)) x$links$Scenario <- x$links$mcYear
    x$areas$mcYear <- NULL
  }
  
  # Add time variable
  x$areas$Dtime <- .timeIdToDate(x$areas$timeId, attr(x, "timeStep"), opts)

  # Base structure: one table per area
  dataList <- dlply(x$areas, .(area), data.table)

  # Additional elements: list of areas and info about the content of each table
  areaList <- names(dataList)
  info <- data.table(area = areaList,
                     valuesLength = ncol(x$areas),
                     detailsLength = 0,
                     linkLength = 0)

  # Add cluster production and flows if available to each element of dataList
  for (n in areaList) {

    # Clusters
    if (!is.null(x$clusters)) {
      cl <- x$clusters[area == n]

      if (nrow(cl) > 0) {
        tmp <- dcast(cl, Scenario + timeId ~ cluster, value.var = "production")
        dataList[[n]] <- merge(dataList[[n]], tmp, by = c("Scenario", "timeId"))

        info[area == n, detailsLength := ncol(tmp) - 2]
      }
    }

    # Flows
    if (!is.null(x$links) && !is.null(x$links$`FLOW LIN.`)) {
      links <- x$links[link %in% getLinks(n)]

      if(nrow(links) > 0) {
        tmp <- dcast(links, Scenario + timeId ~ link, value.var = "FLOW LIN.")
        dataList[[n]] <- merge(dataList[[n]], tmp, by = c("Scenario", "timeId"))

        info[area == n, linkLength := ncol(tmp) - 2]
      }
    }
  }

  dataList$areaList <- areaList
  dataList$infos <- info

  dataList
}
