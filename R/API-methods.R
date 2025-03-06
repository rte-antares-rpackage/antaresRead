
#' API methods
#'
#' @param endpoint API endpoint to interrogate, it will be added after `default_endpoint`.
#'  Can be a full URL (by wrapping ìn [I()]), in that case `default_endpoint` is ignored.
#' @param ... Additional arguments passed to API method.
#' @param default_endpoint Default endpoint to use.
#' @param parse_result `character` options for parameter `as` of function [httr::content()]
#' @param encoding argument to pass as argument to the function [httr::content()]
#' @param opts Antares simulation options or a `list` with an `host = ` slot.
#'
#' @return Response from the API.
#' @export
#'
#' @name API-methods
#'
#' @importFrom httr GET accept_json stop_for_status warn_for_status content add_headers timeout
#'
#' @examples
#' \dontrun{
#'
#' # List studies with local API
#' # default result content in R object (auto parsed)
#' api_get(opts = list(host = "http://0.0.0.0:8080"),
#'         endpoint = NULL, 
#'         parse_result = NULL)
#'
#' # you can force parse options as text and encoding to UTF-8
#' api_get(opts = list(host = "http://0.0.0.0:8080"),
#'         endpoint = NULL, 
#'         parse_result = "text",
#'         encoding = "UTF-8")
#'
#' }
api_get <- function(opts, 
                    endpoint, ..., 
                    default_endpoint = "v1/studies", 
                    parse_result = NULL,
                    encoding = NULL) {
  if (inherits(endpoint, "AsIs")) {
    opts$host <- endpoint
    endpoint <- NULL
    default_endpoint <- NULL
  }
  if (is.null(opts$host))
    stop("No host provided in `opts`: use a valid simulation options object or explicitly provide a host with opts = list(host = ...)")
  config <- c(
    opts$httr_config,
    list(
      accept_json()
    )
  )
  if (!is.null(opts$token) && opts$token != "") {
    config <- c(
      config,
      add_headers(Authorization = paste("Bearer ", opts$token))
    )
  }
  if (is.null(opts$timeout))
    opts$timeout <- 600
  result <- GET(
    url = URLencode(.generate_endpoint_url(opts$host, default_endpoint, endpoint)),
    config = config,
    timeout(opts$timeout),
    ...
  )
  #fix for skipping 404 when some output is missing
  url_elements <- strsplit(result$url, "%2F")[[1]]
  condition_status_check <- !(!is.na(url_elements[4]) & url_elements[4] %in% c("economy","adequacy") & result$status_code == 404)
  if(condition_status_check){
    mess_error <- content(result)
    if(!is.null(names(mess_error)))
      mess_error <- paste0("\n[Description] : ", mess_error$description,
                           "\n[Exception] : ", mess_error$exception)
    else
      mess_error <- NULL
    stop_for_status(result, task = mess_error)
    }else 
      warn_for_status(result)
  content(result, as = parse_result, encoding = encoding)
}

#' @export
#'
#' @rdname API-methods
#'
#' @importFrom httr POST accept_json content_type_json stop_for_status content add_headers
api_post <- function(opts, endpoint, ..., default_endpoint = "v1/studies") {
  if (inherits(endpoint, "AsIs")) {
    opts$host <- endpoint
    endpoint <- NULL
    default_endpoint <- NULL
  }
  if (is.null(opts$host))
    stop("No host provided in `opts`: use a valid simulation options object or explicitly provide a host with opts = list(host = ...)")
  config <- c(
    opts$httr_config,
    list(
      accept_json(),
      content_type_json()
    )
  )
  if (!is.null(opts$token) && opts$token != "") {
    config <- c(
      config,
      add_headers(Authorization = paste("Bearer ", opts$token))
    )
  }
  result <- POST(
    url = URLencode(.generate_endpoint_url(opts$host, default_endpoint, endpoint)),
    config = config,
    ...
  )
  api_content <- content(result)
  if(!is.null(names(api_content)))
    api_content <- paste0("\n[Description] : ", api_content$description,
                         "\n[Exception] : ", api_content$exception)
  else
    api_content <- NULL
  stop_for_status(result, task = api_content)
  content(result)
}

#' @export
#'
#' @rdname API-methods
#'
#' @importFrom httr PUT accept_json stop_for_status content add_headers
api_put <- function(opts, endpoint, ..., default_endpoint = "v1/studies") {
  if (inherits(endpoint, "AsIs")) {
    opts$host <- endpoint
    endpoint <- NULL
    default_endpoint <- NULL
  }
  if (is.null(opts$host))
    stop("No host provided in `opts`: use a valid simulation options object or explicitly provide a host with opts = list(host = ...)")
  if (!is.null(opts$token) && opts$token != "") {
    config <- add_headers(Authorization = paste("Bearer ", opts$token), Accept = "application/json")
  } else {
    config <- add_headers(Accept = "application/json")
  }
  result <- PUT(
    url = URLencode(.generate_endpoint_url(opts$host, default_endpoint, endpoint)),
    config,
    ...
  )
  api_content <- content(result)
  if(!is.null(names(api_content)))
    api_content <- paste0("\n[Description] : ", api_content$description,
                          "\n[Exception] : ", api_content$exception)
  else
    api_content <- NULL
  stop_for_status(result, task = api_content)
  content(result)
}

#' @export
#'
#' @rdname API-methods
#'
#' @importFrom httr DELETE accept_json stop_for_status content
api_delete <- function(opts, endpoint, ..., default_endpoint = "v1/studies") {
  if (inherits(endpoint, "AsIs")) {
    opts$host <- endpoint
    endpoint <- NULL
    default_endpoint <- NULL
  }
  if (is.null(opts$host))
    stop("No host provided in `opts`: use a valid simulation options object or explicitly provide a host with opts = list(host = ...)")
  config <- c(
    opts$httr_config,
    list(
      accept_json()
    )
  )
  if (!is.null(opts$token) && opts$token != "") {
    config <- c(
      config,
      add_headers(Authorization = paste("Bearer ", opts$token))
    )
  }
  result <- DELETE(
    url = URLencode(.generate_endpoint_url(opts$host, default_endpoint, endpoint)),
    config = config,
    ...
  )
  api_content <- content(result)
  if(!is.null(names(api_content)))
    api_content <- paste0("\n[Description] : ", api_content$description,
                          "\n[Exception] : ", api_content$exception)
  else
    api_content <- NULL
  stop_for_status(result, task = api_content)
  content(result)
}
