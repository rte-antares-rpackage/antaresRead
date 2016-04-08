readBindingContraints <- function(opts=getOption("antares")) {
  
  path <- file.path(opts$path, "../../input/bindingConstraints/bindingconstraints.ini")
  bindingConstraints <- readIniFile(path, stringsAsFactors = FALSE)
  
  for (i in 1:length(bindingConstraints)) {
    path <- file.path(opts$path, sprintf("../../input/bindingConstraints/%s.txt",
                                         bindingConstraints[[i]]$id))
    
    if (file.size(path) == 0) {
      bindingConstraints[[i]]$values <- matrix(0L)
    } else {
      bindingConstraints[[i]]$values <- fread(path)
    }
    
  }
  
  bindingConstraints
  
}
