#' Extract simulation options
#' 
#' The function \code{\link{readAntares}} stores in its output the options used
#' to read some data (path of the study, node list, link list, start date,
#' etc.). 
#' 
#' \code{simOptions} extracts these options from an object of class 
#' \code{antaresTable} or \code{antaresOutput}. It can be useful when working on
#' multiple simulations, either to check how some object has been created or to
#' use it in some functions like \code{\link{getNodes}} or
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
#'   data <- readAntares()
#'   
#'   # Choose a different study
#'   setSimulationPath(study2)
#'   
#'   getNodes() # returns the nodes of the secund study
#'   getNodes(opts = simOptions(data)) # returns the nodes of the first study
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
