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
  
  ##
  # read txt files
  ##
  for (i in 1:length(bindingConstraints)) {
    
    # dimension according to parameter "type" to return default value (TS file)
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
      
      # check if "both" case
      both_case <- bindingConstraints[[i]]$operator %in% "both"
      
      # check path file
        # multiple path for "both" case
      if(!all(file.exists(path_file_value)))
        stop("Time series file for binding constraint ", 
                bindingConstraints[[i]]$id, 
                " not exist", 
                call. = FALSE)
        
      # Read files
        # both case 
      if(both_case){
        tmp_values <- lapply(path_file_value, 
                             fread_antares, 
                             opts = opts)
        names(tmp_values) <- c("less", "greater")
       }
      else
        tmp_values <- fread_antares(opts = opts, 
                                    file = path_file_value)
        
        
      # check empty values to return default values
        # both case
      default_scenarised_values <- as.data.table(
        matrix(0L, nrow = nrows, ncol = 1))
      
      if(both_case){
        check_nrow <- unlist(lapply(tmp_values, nrow))
        if(any(check_nrow %in% 0)){
          warning("Time series files for binding constraint ", 
                  bindingConstraints[[i]]$id, 
                  " are empty", 
                  call. = FALSE)
          
          tmp_values[["less"]] <- default_scenarised_values
          tmp_values[["greater"]] <- default_scenarised_values
          # tmp_values <- lapply(tmp_values, 
          #        function(x){
          #          x[["less"]] <- default_scenarised_values
          #          x[["greater"]] <- default_scenarised_values
          #        })
         }
      }
      else
        if(nrow(tmp_values)==0){
        warning("Time series file for binding constraint ", 
                bindingConstraints[[i]]$id, 
                " is empty", 
                call. = FALSE)
        tmp_values <- default_scenarised_values
        }
        
      # # [return] default values 
      # if(is.null(tmp_values)){
      #   default_scenarised_values <- as.data.table(
      #     matrix(0L, nrow = nrows, ncol = 1))
      #   tmp_values <- default_scenarised_values
      # }
      
      # return
      bindingConstraints[[i]]$values <- tmp_values
      
    }else{ # <870 (legacy)
      path <- file.path(opts$inputPath, 
                        sprintf("bindingconstraints/%s.txt", 
                                bindingConstraints[[i]]$id))
      
      # why return 0 if  file.size(path) == 0 ? 
      if(opts$typeLoad != "api" && file.size(path) == 0){
        bindingConstraints[[i]]$values <- as.data.table(
          matrix(0L, nrow = nrows, 3))
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
  
  ##
  # manage full list object
  ##
  
  # to return named list
  constraintNames <- sapply(bindingConstraints, 
                            `[[`, 
                            "name")
  
  # re structure list ($properties, $coefs, $values)
    # [breaking changes] add "$properties" for all version
  bindingConstraints <- lapply(bindingConstraints, function(x) {
    # default names of parameters (core parameters)
    names_elements <- c("name", "id", "enabled", "type", "operator", "values")
    
    # get links information from list
    coefs_elements <- setdiff(names(x), names_elements)
    coefs_values <- x[which(names(x)%in%coefs_elements)]
    
    ##
    # manage properties with version (filter)
    ##
      # filter on parameters to keep only links information
    
    # v832
    if (opts$antaresVersion>=832){
      names_elements_832 <- c("filter-year-by-year", 
                              "filter-synthesis")
      elements_832 <- x[which(names(x)%in%names_elements_832)]
      
      coefs_values[names_elements_832] <- NULL
    }
    
    # v870
    if(opts$antaresVersion>=870){
      names_elements_870 <- "group"
      elements_870 <- x[which(names(x)%in%names_elements_870)]
      
      coefs_values[names_elements_870] <- NULL
    }
      
    ##
    # update list 
    ##
    
    # core elements list
    core_list <- list(
      properties = list(
        name = x$name,
        id = x$id,
        enabled = x$enabled,
        timeStep = x$type,
        operator = x$operator),
      coefs = unlist(coefs_values),
      values = x$values)
    
    # add properties according to version
      # decreasing approach
    
    # v870
    if(opts$antaresVersion>=870){
      list_870 <- list()
      list_870$properties = append(core_list$properties, 
                                   c(
                                     unlist(elements_832),
                                     unlist(elements_870)))
      list_870 <- append(list_870, 
                         core_list[c(2,3)])
      return(list_870)
    }
    # v832
    if(opts$antaresVersion>=832){
      list_832 <- list()
      list_832$properties = append(core_list$properties, 
                                   unlist(elements_832))
      list_832 <- append(list_832, 
                         core_list[c(2,3)])
      return(list_832)
    }

    return(core_list)
  })
  
  names(bindingConstraints) <- constraintNames
  class(bindingConstraints) <- "bindingConstraints"
  bindingConstraints
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
    
    if (x$properties$operator == "both") {
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
      operator <- switch(x$properties$operator, equal = "=", less = "<", greater = ">")
      rhs <- mean(x$values[[x$properties$operator]])
      range <- range(x$values[[x$properties$operator]])
      if(range[1] == range[2]) {
        res <- sprintf("%s %s %s", lhs, operator, rhs)
      } else {
        res <- sprintf("%s %s [%s, %s]", lhs, operator, range[1], range[2])
      }
    }
    
    res
  })
  
  timeStep <- vapply(object, function(x) x$properties$timeStep, character(1))
  enabled <- vapply(object, function(x) x$properties$enabled, logical(1))
  
  data.frame(
    enabled = enabled, 
    timeStep = timeStep, 
    equation = equations
  )
}
