#Copyright © 2016 RTE Réseau de transport d’électricité

#' Extract simulation options
#' 
#' The function \code{\link{readAntares}} stores in its output the options used
#' to read some data (path of the study, area list, link list, start date,
#' etc.). 
#' 
#' \code{simOptions} extracts these options from an object of class 
#' \code{antaresTable} or \code{antaresOutput}. It can be useful when working on
#' multiple simulations, either to check how some object has been created or to
#' use it in some functions like \code{\link{getAreas}} or
#' \code{\link{getLinks}}
#' 
#' If the parameter of the function is \code{NULL}, it returns the default
#' simulation options, that is the options set by \code{\link{setSimulationPath}}
#' the last time it was run.
#' 
#' @param x
#'   object of class \code{antaresTable} or \code{antaresData}
#'   
#' @return
#' list of options used to read the data contained in an object or the last
#' simulation options read by \code{\link{setSimulationPath}} if \code{x} is
#' \code{NULL}
#' 
#' @examples 
#' \dontrun{
#'   setSimulationPath(study1)
#'   
#'   simOptions() # returns the options for study 1
#'   
#'   data <- readAntares()
#'   
#'   # Choose a different study
#'   setSimulationPath(study2)
#'   
#'   simOptions() # returns the options for study 2
#'   
#'   getAreas() # returns the areas of the secund study
#'   getAreas(opts = simOptions(data)) # returns the areas of the first study
#'   
#' }
#' 
#' @export
#' 
simOptions <- function(x = NULL) {
  if (is.null(x)) {
    opts <- getOption("antares")
    if (is.null(opts)) stop("Default antares options are not set. You need to run 'setSimulationPath()' to set them.")
    else return(opts)
  }
  
  if (!is(x, "antaresTable") & !is(x, "antaresData"))
    stop ("x should be an object of class 'antaresTable' or 'antaresData'")
  
  attr(x, "opts")
}

