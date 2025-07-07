
#' Read configuration options from file or API
#'
#' @param pathIni Path to config/ini file to read.
#' @template opts-arg
#' @param default_ext Default extension used for config files.
#'
#'
#' @return A list with an element for each section of the .ini file.
#'
#' @name read-ini
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(antaresRead)
#' library(antaresEditObject)
#'
#' # With physical study:
#' setSimulationPath("../tests-studies/Study_V8.2/", simulation = "input")
#' readIni("settings/generaldata")
#'
#' # With API
#' setSimulationPathAPI(
#'   host = "http://localhost:8080",
#'   study_id = "73427ae1-be83-44e0-b04f-d5127e53424c",
#'   token = NULL,
#'   simulation = "input"
#' )
#' readIni("settings/generaldata")
#'
#' }
readIni <- function(pathIni, opts = antaresRead::simOptions(), default_ext = ".ini") {
  stopifnot(inherits(opts, "simOptions"))
  if (is_api_study(opts)) {
    if (endsWith(pathIni, default_ext))
      pathIni <- sub(sprintf("\\%s$", default_ext), "", x = pathIni)
    readIniAPI(
      study_id = opts$study_id,
      path = pathIni,
      host = opts$host,
      token = opts$token
    )
  } else {
    if (!endsWith(pathIni, default_ext))
      pathIni <- paste0(pathIni, default_ext)
    readIniFile(
      file = file.path(opts$studyPath, pathIni),
      stringsAsFactors = FALSE
    )
  }
}

#' @param file File path.
#' @param stringsAsFactors logical: should character vectors be converted to factors?
#' @importFrom utils type.convert
#' @export
#' @rdname read-ini
readIniFile <- function(file, stringsAsFactors = FALSE) {
  
  X <- readLines(file)
  sections <- grep("^\\[.*\\]$", X)
  starts <- sections + 1
  ends <- c(sections[-1] - 1, length(X))
  L <- vector(mode = "list", length = length(sections))
  names(L) <- gsub("\\[|\\]", "", X[sections])
  
  for(i in seq(along = sections)) {
  
    if (starts[i] >= ends[i]) next
    
    pairs <- X[seq(starts[i], ends[i])]
    pairs <- pairs[pairs != ""]
    pairs <- strsplit(pairs, "=")
    
    key <- lapply(pairs, `[`, 1)
    key <- unlist(key)
    key <- trimws(key)
    
    value <- lapply(pairs, `[`, 2)
    value <- as.list(trimws(unlist(value)))
    value <- lapply(value, function(x){
      if (tolower(x) %in% c("true", "false")) {
        tolower(x) == "true"
      } else if(!identical(grep(pattern = "^[0-9]+(e|i)$", x = x), integer(0))) {
        # Not convert those type of values : 789e or 789i (complex number)
        as.character(x)
      } else {
        utils::type.convert(x, as.is = TRUE)
      }
    }
  )
    
    L[[i]] <- value
    names(L[[i]]) <- key
  }
  L
}


#' @param study_id Study's identifier.
#' @param path Path of configuration object to read.
#' @param host Host of AntaREST server API.
#' @param token API personnal access token.
#' @export
#' @rdname read-ini
readIniAPI <- function(study_id, path, host, token = NULL) {
  api_get_ini_file <- api_get(
    opts = list(host = host, token = token),
    endpoint = paste0(study_id, "/raw"),
    query = list(
      path = path,
      formatted = TRUE
    )
  )
  # reformat list contains unnamed list
  .format_list(api_get_ini_file)
}

# reformat list from JSON format 
.format_list <- function(list_x){
  # check list to find sub list
  check_class_list <- lapply(list_x, function(x) 
    lapply(x, class) %in% "list")
  
  is_true_list <- lapply(check_class_list, function(x) 
    any(x %in% TRUE))
  
  index_true <- which(is_true_list %in% TRUE)
  
  # reformat sub list
  list_to_reformat <- list_x[index_true]
  
  list_to_reformat <- lapply(list_to_reformat, function(x){
    index_list <- which(
      lapply(x, class) %in% "list")
    
    # reformat only unnamed list
    if(is.name(x[index_list]))
      return(x)
    else{
      for (sub_list in index_list){
        elements <- unlist(x[sub_list], use.names = FALSE)
        if(class(elements)%in%"character"){
          elements <- paste("'", elements, "'", sep="", collapse=",")
          elements <- paste0("[", elements, "]")
        }else{
          elements <- paste0(elements, collapse= ",")
          elements <- paste0("[", elements, "]")
        }
        x[sub_list] <- elements
      }
      x
    }
  })
  
  # return original list 
  if(identical(index_true, integer(0)))
    list_x
  else
    # return list reformated
    append(list_x[-index_true], 
           list_to_reformat)
}
