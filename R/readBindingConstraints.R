#Copyright © 2016 RTE Réseau de transport d’électricité

#' Read binding constraints
#' 
#' @description 
#' This function reads the binding constraints of an Antares project. 
#' 
#' Be aware that binding constraints are read in the input files of a study. So
#' they may have changed since a simulation has been run.
#' 
#' @inheritParams readAntares
#' 
#' @return 
#' \code{readBindingConstraints} returns an object of class \code{bindingConstraints}.
#' It is a named list with one element per read constraint. Each element is itself
#' a list with the following elements: 
#' \item{enabled}{is the constraint enabled ?}
#' \item{timeStep}{time step the constraint applies to}
#' \item{operator}{type of constraint: equality, inequality on one side or both sides}
#' \item{coefficients}{elements containing the coefficients used by the constraint}
#' \item{values}{values used by the constraint. It contains one line per time step
#'   and three columns "less", "greater" and "equal"}
#' 
#' The \code{summary} method returns a data.frame with one line per constraint.
#' 
#' 
#' @examples 
#' \dontrun{
#' setSimulationPath()
#'
#' constraints <- readBindingConstraints()
#' summary(constraints)
#' 
#' }
#' 
#' @export
readBindingConstraints <- function(opts=simOptions()) {
  
  if(isH5Opts(opts)){
    if(requireNamespace("rhdf5", versionCheck = list(op = ">=", version = rhdf5_version))){
      return(h5ReadBindingConstraints(opts))
    } else {
      stop(rhdf5_message)
    }
  }
  
  path <- file.path(opts$inputPath, "bindingconstraints/bindingconstraints.ini")
  bindingConstraints <- readIniFile(path, stringsAsFactors = FALSE)
  
  if(length(bindingConstraints) == 0) {
    warning("It looks like there is no binding constraints is this study.")
    return(NULL)
  }
  
  for (i in 1:length(bindingConstraints)) {
    path <- file.path(opts$inputPath, sprintf("bindingconstraints/%s.txt",
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
      timeStep = x$type,
      operator = x$operator,
      coefs = unlist(coefs),
      values = x$values
    )
  })
  
  names(res) <- constraintNames
  class(res) <- "bindingConstraints"
  res
}

#' @param object Object returned by readBindingConstraints
#' @param ... Unused
#' 
#' @export
#' @rdname readBindingConstraints
summary.bindingConstraints <- function(object, ...) {
  equations <- vapply(object, FUN.VALUE = character(1), function(x) {
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
  
  timeStep <- vapply(object, function(x) x$timeStep, character(1))
  enabled <- vapply(object, function(x) x$enabled, logical(1))
  
  data.frame(
    enabled = enabled, 
    timeStep = timeStep, 
    equation = equations
  )
}
