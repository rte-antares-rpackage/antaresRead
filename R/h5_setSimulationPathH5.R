#' Set simulation path for h5 file
#'
#' @param path \code{character} path of a .h5 file or a repertory with .h5 file(s)
#' @param simulation \code{character} simulation number or name
#'
#' @noRd
#' 
setSimulationPathH5 <- function(path, simulation = NULL){
  if(dir.exists(path)){
    allFiles <- list.files(path)
    avaliableFile <- allFiles[grep(".h5$", allFiles)]

    if(length(avaliableFile) == 0){
      stop("Not available .h5 file in your directory")
    }
    if (!is.null(simulation)) {
    if(simulation == 0) simulation <- NULL
    }
    if (is.null(simulation)) {
      if (length(avaliableFile) == 1) { # Case 2
        simulation <- 1
      } else { # Case 3
        cat("Please, choose a simulation\n")
        for (i in 1:length(avaliableFile)) {
          cat(sprintf("   %s - %s\n", i, avaliableFile[i]))
        }
        simulation <- utils::type.convert(scan(what = character(), nmax = 1), as.is = TRUE)
      }
    }

    if(simulation == -1){
      simulation <- length(avaliableFile)
    }

    if(is.character(simulation)){
      if(length(which(simulation == avaliableFile)) == 0){
        stop("Simulation not in your study")
      }
    }

    if(is.numeric(simulation)){
      simulation <- avaliableFile[simulation]
    }

    path <- paste0(path, "/", simulation)
  } else if(!file.exists(path) & !grepl(".h5$", path)){
    stop("Invalid path argument. File not found. Must be a .h5 file or a repertory with .h5 file(s)")
  } else if(file.exists(path) & !grepl(".h5$", path)){
    stop("Invalid path argument. Must be a .h5 file or a repertory with .h5 file(s)")
  }

  attributes <- .getOptionsH5(path)
  options(antares=attributes)
  attributes
}

#' Get H5 options
#'
#' @param path \code{character} path of h5 file
#'
#' @noRd
#' @export
# Need to be export for antaresViz
.getOptionsH5 <- function(path){
  
  .requireRhdf5_Antares()
  fid <- rhdf5::H5Fopen(path)
  attributes <- .loadAttributes(fid, "hourly")
  attributes <- attributes$opts
  attributes$h5 <- TRUE
  attributes$h5path <- normalizePath(path)
  attributes$studyPath <- NULL
  attributes$simPath <- NULL
  attributes$inputPath <- NULL
  attributes$simDataPath <- NULL
  attributes
}
