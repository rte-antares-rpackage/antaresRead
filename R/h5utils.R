#Copyright © 2016 RTE Réseau de transport d’électricité

#' Test if opts is h5
#'
#' @description Test if opts is h5
#' 
#' @param opts , opts
#' @export
isH5Opts <- function(opts){
  v <- FALSE
  if(!is.null(opts$h5)){
    if(opts$h5){
      v <- TRUE
    }
  }
  v
}
