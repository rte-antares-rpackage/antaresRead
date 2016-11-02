#Copyright © 2016 RTE Réseau de transport d’électricité

# When doing some simple modifications
#' @export
"[.antaresDataTable" <- function(x, ...) {
  attrs <- attributes(x)
  idVars <- .idCols(x)
  
  x <- NextMethod("[", x)
  
  if (all(idVars%in% names(x))) {
    .addClassAndAttributes(x, attrs$synthesis, attrs$timeStep, attrs$opts, 
                           type = attrs$type)
  }
  
  x
}

#' @export
merge.antaresDataTable <- function(x, ...) {
  attrs <- attributes(x)
  idVars <- .idCols(x)
  
  x <- NextMethod(merge, x)
  
  .addClassAndAttributes(x, attrs$synthesis, attrs$timeStep, attrs$opts, 
                         type = attrs$type)
  
  .reorderCols(x)
}
