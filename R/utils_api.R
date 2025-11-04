#' @importFrom utils URLencode
#' @importFrom shiny isRunning
#' @importFrom data.table fread
fread_antares <- function(opts, file, ...) {
  if (identical(opts$typeLoad, "api")) {
    file <- gsub("\\.txt$", "", file)
    response <- api_get(
      opts = opts,
      endpoint = I(file),
      query = list(formatted = FALSE),
      parse_result = "text"
    )
    suppressWarnings(
      tryCatch(fread(response, ...), error = function(e){
        if(isRunning())
          e <- as.character(e)
        message(file)
        message(e)
      }))
  } else {
    suppressWarnings(
      fread(file, ...))
  }
}


empty_strings_as_NA <- function(x) {
  if (identical(x, ""))
    return(NA)
  if (is.character(x))
    return(x)
  rapply(object = x, f = function(y) {
    if (identical(y, "")) {
      return(NA)
    }
    return(y)
  }, how = "replace")
}


#' @importFrom utils URLencode
read_secure_json <- function(url, token = NULL, timeout = 600, config = list()) {
  result <- api_get(
    opts = list(token = token, timeout = timeout, httr_config = config),
    endpoint = I(url)
  )
  empty_strings_as_NA(result)
}


#' @importFrom httr GET timeout add_headers http_status
.getSuccess <- function(path, token, timeout = 600, config = list()) {
  if (!is.null(token) && token != "") {
    response <- GET(
      URLencode(path), timeout(timeout),
      add_headers(Authorization = paste0("Bearer ", token)),
      config = config
    )
  } else {
    response <- GET(path, timeout(timeout), config = config)
  }
  http_status(response)$category == "Success"
}


#' Change API Timeout
#'
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{\link{setSimulationPathAPI}}
#' @param timeout \code{numeric} API timeout (seconds). Default to 600.
#'
#' @return
#' Object of class `simOptions`, list of options used to read the data contained in the last
#' simulation read by \code{\link{setSimulationPathAPI}}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' opts <- setTimeoutAPI(opts, timeout = 45)
#' }
#'
setTimeoutAPI <- function(opts, timeout){
  if(opts$typeLoad == 'api'){
    opts$timeout <- timeout
  } else {
    warning("setTimeoutAPI can only be use for API Simulation")
  }
  return(opts)
}


is_api_study <- function(opts) {
  isTRUE(opts$typeLoad == "api")
}


split_vector_in_equal_parts <- function(x, n) {

  return(split(x, sort(seq(x)%%n)))
}
