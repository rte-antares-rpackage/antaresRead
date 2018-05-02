#' Import binding constraints
#'
#' @description
#' This function imports the binding constraints of an Antares project form an h5 file see also \link[antaresRead]{readBindingConstraints}.
#'
#' @param opts \code{list} of simulation parameters returned by the function \link[antaresRead]{setSimulationPath}.
#'
#' @noRd
h5ReadBindingConstraints <- function(opts){
  .requireRhdf5_Antares()
  fid <- rhdf5::H5Fopen(opts$h5path)
  timestep <- .getTimeStep(fid)[1]
  out <- unserialize(charToRaw(rhdf5::h5read(fid, paste0(timestep , "/inputs/buildingcte"))))
  rhdf5::H5close()
  out
}

#' Import areas layout
#'
#' @description
#' This function imports the position of the areas from an h5 file. It may be useful for plotting the
#' network see also \link[antaresRead]{readLayout}.
#'
#' Be aware that the layout is read in the input files so they may have changed since a simulation has been run.
#' @param opts \code{list} of simulation parameters returned by the function \link[antaresRead]{setSimulationPath}.
#'
#' @noRd
h5ReadLayout <- function(opts){
  .requireRhdf5_Antares()
  fid <- rhdf5::H5Fopen(opts$h5path)
  timestep <- .getTimeStep(fid)[1]
  out <- unserialize(charToRaw(rhdf5::h5read(fid, paste0(timestep , "/inputs/layout"))))
  rhdf5::H5close()
  out
}

#' Import cluster description
#'
#' @description
#' This function imports the characteristics of each cluster from an h5 file see also \link[antaresRead]{readClusterDesc}.
#'
#' Be aware that clusters descriptions are read in the input files so they may have changed since a simulation has been run.
#' @param opts \code{list} of simulation parameters returned by the function \link[antaresRead]{setSimulationPath}.
#'
#' @noRd
#' 
h5ReadClusterDesc <- function(opts){
  .requireRhdf5_Antares()
  fid <- rhdf5::H5Fopen(opts$h5path)
  timestep <- .getTimeStep(fid)[1]
  out <- unserialize(charToRaw(rhdf5::h5read(fid, paste0(timestep , "/inputs/cldesc"))))
  rhdf5::H5close()
  out
}
