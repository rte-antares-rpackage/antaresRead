#' Read binding constraints
#'
#' @description
#' This function reads the binding constraints of an Antares project \link[antaresRead]{readBindingConstraints}.
#' @param opts \link[antaresRead]{readBindingConstraints}
#'
#' @noRd
h5ReadBindingConstraints <- function(opts){
  if(!requireNamespace("rhdf5", versionCheck = list(op = ">=", version = "2.20.0"))) stop(rhdf5_message)
  fid <- rhdf5::H5Fopen(opts$h5path)
  timestep <- .getTimeStep(fid)[1]
  out <- unserialize(charToRaw(rhdf5::h5read(fid, paste0(timestep , "/inputs/buildingcte"))))
  rhdf5::H5close()
  out
}

#' Import clusters description
#'
#' @description
#' This function reads in the input files of an antares study the
#' characteristics of each cluster \link[antaresRead]{readBindingConstraints}.
#'
#' @param opts \link[antaresRead]{readBindingConstraints}
#'
#' @noRd
h5ReadLayout <- function(opts){
  if(!requireNamespace("rhdf5", versionCheck = list(op = ">=", version = "2.20.0"))) stop(rhdf5_message)
  fid <- rhdf5::H5Fopen(opts$h5path)
  timestep <- .getTimeStep(fid)[1]
  out <- unserialize(charToRaw(rhdf5::h5read(fid, paste0(timestep , "/inputs/layout"))))
  rhdf5::H5close()
  out
}

#' Read areas layout
#'
#' @description
#' This function reads in the input files of an antares study the current areas
#' layout, ie. the position of the areas It may be useful for plotting the
#' network.
#'
#' Be aware that the layout is read in the input files so they may have
#' changed since a simulation has been run \link[antaresRead]{readBindingConstraints}.
#' 
#' @param opts \link[antaresRead]{readBindingConstraints}
#'
#' @noRd
#' 
h5ReadClusterDesc <- function(opts){
  if(!requireNamespace("rhdf5", versionCheck = list(op = ">=", version = "2.20.0"))) stop(rhdf5_message)
  fid <- rhdf5::H5Fopen(opts$h5path)
  timestep <- .getTimeStep(fid)[1]
  out <- unserialize(charToRaw(rhdf5::h5read(fid, paste0(timestep , "/inputs/cldesc"))))
  rhdf5::H5close()
  out
}
