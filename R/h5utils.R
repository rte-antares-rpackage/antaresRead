#Copyright © 2016 RTE Réseau de transport d’électricité

#' Test if opts is h5
#'
#' @description Test if the value returned by setSimulationPath() is referring to an h5 file
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


.getTimeStep <- function(fid){
  .requireRhdf5_Antares()
  timeSteps <- sapply(c("hourly", "daily", "weekly", "monthly", "annual"), function(X){
    rhdf5::H5Lexists(fid, X)
  })
  names(timeSteps[which(timeSteps == TRUE)])
}
