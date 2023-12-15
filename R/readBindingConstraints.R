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
readBindingConstraints <- function(opts = simOptions()) {
  
  if(isH5Opts(opts)){
    if(.requireRhdf5_Antares(stopP = FALSE)){
      return(h5ReadBindingConstraints(opts))
    } else {
      stop(rhdf5_message)
    }
  }
  
  ##
  # API BLOC
  ##
  
  if(opts$typeLoad == 'api'){
    bindingConstraints <- read_secure_json(file.path(opts$inputPath, "bindingconstraints", "bindingconstraints"), 
                                           opts$token, timeout = opts$timeout, config = opts$httr_config)
  }else{
    path <- file.path(opts$inputPath, "bindingconstraints/bindingconstraints.ini")
    bindingConstraints <- readIniFile(path, stringsAsFactors = FALSE)
  }
  
  ##
  # Exception if no properties
  ##
  
  if(length(bindingConstraints) == 0) {
    warning("It looks like there is no binding constraints is this study.")
    return(NULL)
  }
  
  # read txt files
  for (i in 1:length(bindingConstraints)) {
    # check to return 0 values if empty file (only for < v870)
    nrows <- switch(bindingConstraints[[i]]$type,
                    hourly = 24*7*52,
                    daily = 7 * 52,
                    weekly = 52,
                    monthly = 12,
                    annual = 1)
    
    # v870
    if(opts$antaresVersion>=870){
      
      parse_type <- switch(bindingConstraints[[i]]$operator,
                      less = "lt",
                      greater = "gt",
                      equal = "eq",
                      both = c("lt", "gt")) # "both" case ? 
      
      path_file_value <- file.path(opts$inputPath, 
                           sprintf("bindingconstraints/%s.txt", 
                                   paste0(bindingConstraints[[i]]$id, 
                                          "_",
                                          parse_type)))
      
      
      suppressWarnings(
        tmp_values <- lapply(path_file_value, 
                             fread_antares, 
                             opts = opts)
      )
      
      names(tmp_values) <- parse_type
      
      # this test do nothing => tmp_values never NULL
        # return 0 row/col for empty file or error if file does not exist
      if(is.null(tmp_values)){
        tmp_values <- list(`lt`= matrix(0L, nrow = nrows),
                           `gt`= matrix(0L, nrow = nrows),
                           `eq`= matrix(0L, nrow = nrows))
      }
      bindingConstraints[[i]]$values <- tmp_values
      
    }else{
      path <- file.path(opts$inputPath, 
                        sprintf("bindingconstraints/%s.txt", 
                                bindingConstraints[[i]]$id))
      
      # why return 0 if  file.size(path) == 0 ? 
      if(opts$typeLoad != "api" && file.size(path) == 0){
        bindingConstraints[[i]]$values <- as.data.table(matrix(0L, nrow = nrows, 3))
        setnames(bindingConstraints[[i]]$values, 
                 names(bindingConstraints[[i]]$values),
                 c("less", "greater", "equal"))
      }
      else{
        # bindingConstraints[[i]]$values <- fread(path)
        tmp_values <- fread_antares(opts = opts, file = path)
        
        # this test do nothing => tmp_values never NULL
          # return 0 row/col for empty file or error if file does not exist
        if(is.null(tmp_values)){
          tmp_values <- as.data.table(matrix(0L, nrow = nrows, 3))
        }
        
        bindingConstraints[[i]]$values <- tmp_values
        setnames(bindingConstraints[[i]]$values, 
                 names(bindingConstraints[[i]]$values),
                 c("less", "greater", "equal"))
      }
    }
  }  
  
  res <- unname(bindingConstraints)
  
  constraintNames <- vapply(res, function(x) x$name, character(1))
  
  # re structure list
  res <- lapply(res, function(x) {
    coefs <- x
    # default names of parameters
    names_elements <- c("name", "id", "enabled", "type", "operator", "values")
    
    # v832
    if (opts$antaresVersion>=832)
      names_elements <- append(names_elements, 
                               c("filter-year-by-year", "filter-synthesis"))

    # v870
    if(opts$antaresVersion>=870)
      names_elements <- append(names_elements, "group")
      
    for (v in names_elements) {
      coefs[[v]] <- NULL
    }
    
    # update list 
    
    # v870
    if(opts$antaresVersion>=870)
      list(
        enabled = x$enabled,
        timeStep = x$type,
        operator = x$operator,
        `filter-year-by-year` = x$`filter-year-by-year`,
        `filter-synthesis` = x$`filter-synthesis`,
        group = x$group,
        coefs = unlist(coefs),
        values = x$values)
    # v832
    else if(opts$antaresVersion>=832)
      list(
        enabled = x$enabled,
        timeStep = x$type,
        operator = x$operator,
        `filter-year-by-year` = x$`filter-year-by-year`,
        `filter-synthesis` = x$`filter-synthesis`,
        coefs = unlist(coefs),
        values = x$values)
    else
      list(
      enabled = x$enabled,
      timeStep = x$type,
      operator = x$operator,
      coefs = unlist(coefs),
      values = x$values)
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
