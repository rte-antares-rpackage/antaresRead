#Copyright © 2016 RTE Réseau de transport d’électricité

#' Read binding constraints
#' 
#' @description 
#' This function reads the binding constraints of an Antares project. 
#' 
#' Be aware
#' that binding constraints are read in the input files of a study. So they may
#' have changed since a simulation has been run.
#' 
#' @inheritParams readAntares
#' 
#' @return 
#' A list containing one element per constraint. Each element is a list with the
#' following content:
#' \item{name}{name of the constraint}
#' \item{id}{id of the constraint}
#' \item{enabled}{is the constraint enabled ?}
#' \item{type}{time step the constraint applies to}
#' \item{operator}{type of constraint: equality, inequality on one side or both sides}
#' \item{coefficients}{elements containing the coefficients used by the constraint}
#' \item{values}{values used by the constraint. It contains one line per time step
#'   and three columns "less", "greate" and "equal"}
#' 
#' @examples 
#' \dontrun{
#' setSimulationPath()
#' 
#' # Read the constraints of the default antares study
#' readBindingConstraints()
#' 
#' #Equivalent to:
#' readBindingConstraints(simOptions())
#' 
#' # Read the constraints of a given antares study
#' areasSimulation1 <- readAntares()
#' 
#' # [... code that change the default antares study]
#' 
#' readBindingConstraints(simOptions(areasSimulation1))
#' 
#' }
#' 
#' @export
readBindingConstraints <- function(opts=simOptions()) {
  
  path <- file.path(opts$inputPath, "bindingConstraints/bindingconstraints.ini")
  bindingConstraints <- readIniFile(path, stringsAsFactors = FALSE)
  
  if(length(bindingConstraints) == 0) {
    warning("It looks like there is no binding constraints is this study.")
    return(NULL)
  }
  
  for (i in 1:length(bindingConstraints)) {
    path <- file.path(opts$inputPath, sprintf("bindingConstraints/%s.txt",
                                         bindingConstraints[[i]]$id))
    
    if (file.size(path) == 0) {
      nrows <- switch(bindingConstraints[[i]]$type,
                      hourly = 24*7*52,
                      daily = 7 * 52,
                      weekly = 52,
                      monthly = 12,
                      annual = 1)
      
      bindingConstraints[[i]]$values <- as.data.table(matrix(0L, nrow = nrows, 3))
    } else {
      bindingConstraints[[i]]$values <- fread(path)
    }
    
    setnames(bindingConstraints[[i]]$values, 
             names(bindingConstraints[[i]]$values),
             c("less", "greater", "equal"))
    
  }
  
  unname(bindingConstraints)
  
}
