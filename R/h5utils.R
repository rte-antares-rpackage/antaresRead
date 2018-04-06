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

# .writeAttributes <- function(res = NULL, path = NULL, timeStep = "hourly", fid = NULL, attributes = NULL)
# {
#   .requireRhdf5_Antares()
#   
#   if(is.null(attributes))
#   {
#     attrib <- attributes(res)
#   }else{
#     attrib <- attributes
#   }
#   s <- serialize(attrib, NULL, ascii = TRUE)
#   if(!is.null(path))
#   {
#     rhdf5::h5write.default(rawToChar(s), path, paste0(timeStep, "/attrib"))
#   }else{
#     did <- rhdf5::H5Dopen(fid,  paste0(timeStep, "/attrib"))
#     rhdf5::H5Dwrite(did, rawToChar(s))
#   }
# }
