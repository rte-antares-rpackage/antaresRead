#Copyright © 2016 RTE Réseau de transport d’électricité

#' aggregate a data.table by district
#'
#' @param x
#'   data.table containing at least a column area.
#' @param opts
#'   simulation options returned with simOptions()
#' @param fun
#'   vector of functions with size equal to the number of columns to aggregate
#'   (number of columns - number of id columns). If it is of length 1, the
#'   function is used for all columns.
#'
#' @return
#' A data.table with the same columns as the input except that the column area
#' is replaced by column district.
#'
#' @note
#' This is a private function that is used in functions like surplus that require
#' detailed data by area, but user may want aggregated output.
#'
#' @noRd
#'
.groupByDistrict <- function(x, opts, fun = c(sum)) {
  x <- merge(x, opts$districtsDef, by = "area", allow.cartesian = TRUE)

  # Check that all nodes from a district are in the data
  areasInData <- unique(x$area)
  districts <- intersect(x$district, opts$districtsDef$district)
  districtsDef <- split(opts$districtsDef$area, opts$districtsDef$district)

  for (d in districts) {
    missingAreas <- setdiff(districtsDef[[d]], areasInData)

    if (length(missingAreas) > 0) warning("The following areas belongs to district ", d, " but are not in 'x': ",
                                          paste(missingAreas, collapse = ", "))
  }

  # Aggregation by district
  x[, area := NULL]
  idVars <- .idCols(x)

  if (length(fun) == 1) fun <- rep(fun, ncol(x) - length(idVars))

  x[, mapply(function(x, f) {f(x)}, x = .SD, f = fun, SIMPLIFY=FALSE),
    by = idVars]

}

.merge_Col_Area_D <- function(x = NULL, colMerge = NULL, opts=NULL, allX = TRUE){
  if (!is.null(x$districts)){
    for(oneCol in colMerge){
      byarea <- .get_by_area(x)
      bydistrict <- .get_by_district(x)
      ColAreas <- x$areas[, mget(oneCol), by = byarea]
      ColDistricts <- .groupByDistrict(ColAreas, opts)
      if(!(oneCol %in% names(x$districts))){
        x$districts <- merge(x$districts, 
                             ColDistricts, 
                             by = bydistrict)
      }else{
        x$districts <- merge(x$districts, 
                             ColDistricts, 
                             by = bydistrict,
                             all.x = allX,
                             all.y = !allX)
      }
      
      if(paste0(oneCol,".x") %in% names(x$districts)){
        
        if(!(oneCol %in% names(x$districts))){
          if(allX){
            x$districts[, c(oneCol):= get(paste0(oneCol,".x"))]
          }else{
            x$districts[, c(oneCol):= get(paste0(oneCol,".y"))]
          }
        }
        x$districts[, c(paste0(oneCol,".x")) := NULL]
        x$districts[, c(paste0(oneCol,".y")) := NULL]
      }
    }
  }
  return(x)
}
