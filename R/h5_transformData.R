#' Transform a \code{antaresDataList} object to be writable by \link{writeAntaresH5}
#'
#' @param data \code{antaresDataList}
#' @param areasKey \code{character} organization key for areas, define h5 group and subgroup
#' @param linksKey \code{character} organization key for links, define h5 group and subgroup
#' @param districtKey \code{character} organization key for districts, define h5 group and subgroup
#' @param clustersKey \code{character} organization key for clusters, define h5 group and subgroup
#'
#' @import data.table
#'
#' @noRd
#' 
transformH5 <- function(data,
                        areasKey = c("area"),
                        linksKey = c("link"),
                        districtKey = c("district"),
                        clustersKey = c("area", "cluster")){

  if("areas"%in%names(data))
  {
    data$areas <- data$areas[, list(list(.SD)), by = areasKey]
  }
  if("links"%in%names(data))
  {
    data$links <- data$links[, list(list(.SD)), by = linksKey]
  }
  if("districts"%in%names(data))
  {
    data$districts <- data$districts[, list(list(.SD)), by = districtKey]
  }
  if("clusters"%in%names(data))
  {
    data$clusters <- data$clusters[, list(list(.SD)), by = clustersKey]
  }
  data
}
