#' Read binding constraints
#' 
#' This function reads the binding constraints of an Antares project.
#' 
#' @inheritParams readAntares
#' 
#' @return 
#' A list with an element per binding constraint. 
#' 
#' @export
readBindingContraints <- function(opts=simOptions()) {
  
  path <- file.path(opts$path, "../../input/bindingConstraints/bindingconstraints.ini")
  bindingConstraints <- readIniFile(path, stringsAsFactors = FALSE)
  
  for (i in 1:length(bindingConstraints)) {
    path <- file.path(opts$path, sprintf("../../input/bindingConstraints/%s.txt",
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
