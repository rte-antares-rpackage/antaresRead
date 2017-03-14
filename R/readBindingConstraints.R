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
  
  res <- unname(bindingConstraints)
  
  constraintNames <- vapply(res, function(x) x$name, character(1))
  
  res <- lapply(res, function(x) {
    coefs <- x
    for (v in c("name", "id", "enabled", "type", "operator", "values")) {
      coefs[[v]] <- NULL
    }
    
    list(
      enabled = x$enabled,
      type = x$type,
      operator = x$operator,
      coefs = unlist(coefs),
      values = x$values
    )
  })
  
  names(res) <- constraintNames
  class(res) <- "bindingConstraints"
  res
}


summary.bindingConstraints <- function(x) {
  equations <- vapply(x, FUN.VALUE = character(1), function(x) {
    coefs <- sprintf(
      "%s %s x %s",
      ifelse(sign(x$coefs < 0), " -", " +"),
      abs(x$coefs),
      names(x$coefs)
    )
    
    lhs <- paste(coefs, collapse = "")
    lhs <- gsub("^ (\\+ )?", "", lhs)
    lhs <- gsub("1 x ", "", lhs)
    
    if (x$operator == "both") {
      # Left inequality
      rhs <- mean(x$values$greater)
      range <- range(x$values$greater)
      if(range[1] == range[2]) {
        res <- sprintf("%s < %s", rhs, lhs)
      } else {
        res <- sprintf("[%s, %s] < %s", range[1], range[2], lhs)
      }
      # right inequality
      rhs <- mean(x$values$less)
      range <- range(x$values$less)
      if(range[1] == range[2]) {
        res <- sprintf("%s < %s", res, rhs)
      } else {
        res <- sprintf("%s < [%s, %s]", res, range[1], range[2])
      }
    } else {
      operator <- switch(x$operator, equal = "=", less = "<", greater = ">")
      rhs <- mean(x$values[[x$operator]])
      range <- range(x$values[[x$operator]])
      if(range[1] == range[2]) {
        res <- sprintf("%s %s %s", lhs, operator, rhs)
      } else {
        res <- sprintf("%s %s [%s, %s]", lhs, operator, range[1], range[2])
      }
    }
    
    res
  })
  
  type <- vapply(x, function(x) x$type, character(1))
  enabled <- vapply(x, function(x) x$enabled, logical(1))
  
  data.frame(
    enabled = enabled, 
    type = type, 
    equation = equations
  )
}
