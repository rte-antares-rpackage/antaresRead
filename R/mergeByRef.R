#Copyright © 2016 RTE Réseau de transport d’électricité

.mergeByRef <- function(dt1, dt2, on = intersect(names(dt1), names(dt2)), 
                        colsToAdd = setdiff(names(dt2), on)) {
  setkeyv(dt1, on)
  setkeyv(dt2, on)
  dt1[dt2, c(colsToAdd) := mget(paste0("i.", colsToAdd))]
  
  dt1
}
