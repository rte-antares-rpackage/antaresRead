#Copyright © 2016 RTE Réseau de transport d’électricité

#' Read binding constraints
#' 
#' @description 
#' `r antaresRead:::badge_api_ok()`
#' `r lifecycle::badge("experimental")`  
#' 
#' This function reads the binding constraints of an Antares project. 
#' 
#' Be aware that binding constraints are read in the input files of a study. So
#' they may have changed since a simulation has been run.
#' 
#' @inheritParams readAntares
#' @param with_time_series \code{boolean} if TRUE, the second member time series are read
#' @param constraint_names \code{str} constraint names to filter on
#'
#' @importFrom assertthat assert_that
#' 
#' @return 
#' An object of class \code{bindingConstraints}. This object is also a named 
#' list with 3 sections per read constraint.
#' 
#' @section Warning:
#' Since `release 2.7.0` the structure of the returned object has evolved for 
#' all versions of study Antares:  
#'  - .ini parameters are in section `properties`
#'  - Coefficients links or thermal are in section `coefs`  
#'  - Values are already in section `values`
#'  
#' @note 
#' For an study Antares **version >=8.7.0**. Now contains `data.frame` with 
#' one line per time step and \eqn{p} colums according to "scenarized RHS".  
#' 
#' For "both" case, you will find in section `values` two `data.frame` :  
#'  - One `data.frame` for `less` 
#'  - One `data.frame` for `greater`
#'  
#' For an study Antares **version <8.7.0**.  
#' 
#' Section \code{values} contains one line 
#' per time step and three columns "less", "greater" and "equal"
#' 
#' 
#' 
#' @examples 
#' \dontrun{
#' setSimulationPath()
#'
#' constraints <- readBindingConstraints()
#' 
#' # read properties
#' constraints$properties
#' 
#' # read coefs
#' constraints$coefs
#' 
#' # read values
#' constraints$values
#'   # both case ( study Antares >=8.7.0)
#' constraints$values$less
#' constraints$values$greater
#' 
#' # display equation (only for study Antares <8.7.0)
#' summary(constraints)
#' 
#' # read binding constraints without the time series
#' readBindingConstraints(opts = simOptions(), with_time_series = FALSE)
#' }
#' 
#' @export
readBindingConstraints <- function(opts = simOptions(), with_time_series = TRUE, constraint_names = NULL) {
  
  assert_that(inherits(with_time_series, what = "logical"))
  
  ##
  # API BLOC
  ##
  
  if(opts$typeLoad == 'api'){
    bindingConstraints <- read_secure_json(file.path(opts$inputPath, 
                                                     "bindingconstraints", 
                                                     "bindingconstraints"), 
                                           opts$token, 
                                           timeout = opts$timeout, 
                                           config = opts$httr_config)
  }else{
    path <- file.path(opts$inputPath, 
                      "bindingconstraints/bindingconstraints.ini")
    bindingConstraints <- readIniFile(path, 
                                      stringsAsFactors = FALSE)
  }
  
  ##
  # Exception if no properties
  ##
  
  if(length(bindingConstraints) == 0) {
    warning("It looks like there is no binding constraints is this study.")
    return(NULL)
  }
  
  # Filter on constraint_names
  if (!is.null(constraint_names)) {
    bindingConstraints <- .filter_bindingConstraints_by_names(bindingConstraints = bindingConstraints,
                                                              constraint_names = constraint_names)
  }
  ##
  # read values txt files
  ##
  if (with_time_series) {
    bindingConstraints <- lapply(bindingConstraints, 
                                 FUN = .read_binding_values, 
                                 opts = opts)
  }
  
  ##
  # manage full list object
  ##
  
  # to return named list
  constraintNames <- sapply(bindingConstraints, 
                            `[[`, 
                            "id")
  
  # re structure list ($properties, $coefs, $values)
    # [breaking changes] add "$properties" for all version
  bindingConstraints <- lapply(bindingConstraints, 
                               .manage_list_structure, 
                               opts = opts)
  
  if (!with_time_series) {
    bindingConstraints <- lapply(bindingConstraints,
                                 FUN = function(bc) {bc[!names(bc)=="values"]}
                                 )
  }
  
  names(bindingConstraints) <- constraintNames
  class(bindingConstraints) <- "bindingConstraints"
  
  return(bindingConstraints)
}


# read values files for every binding of study
.read_binding_values <- function(binding_object, 
                                 opts = simOptions()){
  # dimension according to parameter "type" to return default value (TS file)
  nrows <- switch(binding_object$type,
                  hourly = 24*7*52,
                  daily = 7 * 52,
                  weekly = 52,
                  monthly = 12,
                  annual = 1)
  
  # v870
  if(opts$antaresVersion>=870){
    
    parse_type <- switch(binding_object$operator,
                         less = "lt",
                         greater = "gt",
                         equal = "eq",
                         both = c("lt", "gt")) # "both" case ? 
    
    path_file_value <- file.path(opts$inputPath, 
                                 sprintf("bindingconstraints/%s.txt", 
                                         paste0(binding_object$id, 
                                                "_",
                                                parse_type)))
    
    # check if "both" case
    both_case <- binding_object$operator %in% "both"
    
    # check path file
    # multiple path for "both" case
    if(opts$typeLoad != "api"){
      if(!all(file.exists(path_file_value)))
        stop("Time series file for binding constraint ", 
             binding_object$id, 
             " not exist", 
             call. = FALSE)
    }
    
    
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
        tmp_values[["less"]] <- default_scenarised_values
        tmp_values[["greater"]] <- default_scenarised_values
      }
    }
    else
      if(nrow(tmp_values)==0)
        tmp_values <- default_scenarised_values
    # return
    binding_object$values <- tmp_values
    return(binding_object)
    
  }else{ # <870 (legacy)
    path <- file.path(opts$inputPath, 
                      sprintf("bindingconstraints/%s.txt", 
                              binding_object$id))
    
    # why return 0 if  file.size(path) == 0 ? 
    if(opts$typeLoad != "api" && file.size(path) == 0){
      binding_object$values <- as.data.table(
        matrix(0L, nrow = nrows, 3))
      setnames(binding_object$values, 
               names(binding_object$values),
               c("less", "greater", "equal"))
      return(binding_object)
    }
    else{
      # binding_object$values <- fread(path)
      tmp_values <- fread_antares(opts = opts, file = path)
      
      # this test do nothing => tmp_values never NULL
      # return 0 row/col for empty file or error if file does not exist
      if(is.null(tmp_values)){
        tmp_values <- as.data.table(matrix(0L, nrow = nrows, 3))
      }
      
      binding_object$values <- tmp_values
      setnames(binding_object$values, 
               names(binding_object$values),
               c("less", "greater", "equal"))
      return(binding_object)
    }
  }
}


# build list structure according to antares version
.manage_list_structure <- function(binding_object, 
                                   opts = simOptions()){
  
  # default names of parameters (core parameters)
  names_elements <- c("name", "id", "enabled", "type", "operator", "comments", "values")
  
  # get links information from list
  coefs_elements <- setdiff(names(binding_object), 
                            names_elements)
  coefs_values <- binding_object[which(names(binding_object) %in%
                                         coefs_elements)]
  
  ##
  # manage properties with version (filter)
  ##
  # filter on parameters to keep only links information
  
  is_v832 <- opts[["antaresVersion"]] >= 832
  is_v870 <- opts[["antaresVersion"]] >= 870
  
  # v832
  if (is_v832) {
    names_elements_832 <- c("filter-year-by-year", 
                            "filter-synthesis")
    elements_832 <- binding_object[which(names(binding_object) %in% 
                                           names_elements_832)]
    
    coefs_values[names_elements_832] <- NULL
  }
  
  # v870
  if (is_v870) {
    names_elements_870 <- "group"
    elements_870 <- binding_object[which(names(binding_object) %in% 
                                           names_elements_870)]
    
    coefs_values[names_elements_870] <- NULL
  }
  
  ##
  # update list 
  ##
  
  # core elements list
  core_list <- list(
    "properties" = list(
      "name" = binding_object[["name"]],
      "id" = binding_object[["id"]],
      "enabled" = binding_object[["enabled"]],
      "timeStep" = binding_object[["type"]],
      "operator" = binding_object[["operator"]],
      "comments" = binding_object[["comments"]]
      ),
    "coefs" = unlist(coefs_values),
    "values" = binding_object[["values"]]
  )
  
  # add properties according to version
  # decreasing approach
  
  # v870
  if (is_v870) {
    list_870 <- list()
    list_870[["properties"]] = append(core_list[["properties"]], 
                                 c(
                                   unlist(elements_832),
                                   unlist(elements_870)))
    list_870 <- append(list_870, 
                       core_list[c(2,3)])
    return(list_870)
  }
  # v832
  if (is_v832) {
    list_832 <- list()
    list_832[["properties"]] = append(core_list[["properties"]], 
                                 unlist(elements_832))
    list_832 <- append(list_832, 
                       core_list[c(2,3)])
    return(list_832)
  }
  
  return(core_list)
}


#' @title Display equation of binding constraint
#' @description 
#' `r lifecycle::badge("deprecated")`
#' This function cannot be used for a study `>= 8.7.0`
#' @param object Object returned by readBindingConstraints
#' @param ... Unused
#' 
#' @return A data.frame with one line per constraint.
#' @export
summary.bindingConstraints <- function(object, ...) {
  lifecycle::deprecate_warn(">= 2.7.0", "antaresRead::summary.bindingConstraints()")
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

#' @title Filter a list of binding constraints by names by a exact match
#' @param bindingConstraints \code{list} a list of binding constraints
#' @param constraint_names \code{str} constraint names to filter on
#' @return A list of binding constraints containing the constraints which have their name in constraint_names.
.filter_bindingConstraints_by_names <- function(bindingConstraints, constraint_names) {
  
  constraint_names <- tolower(constraint_names)
  existing_constraint_names <- sapply(bindingConstraints, "[[", "name", USE.NAMES = FALSE, simplify = TRUE)
  existing_constraint_names <- tolower(existing_constraint_names)
  constraint_names <- intersect(existing_constraint_names, constraint_names)
  
  if (length(constraint_names) == 0) {
    warning("No binding constraints with one of the names you provide as argument.")
    return(NULL)
  }
  
  # index starts at 0 in input/bindingconstraints/bindingconstraints.ini
  idx_constraint_names <- which(existing_constraint_names %in% constraint_names) - 1
  
  return(bindingConstraints[names(bindingConstraints) %in% idx_constraint_names])
}
