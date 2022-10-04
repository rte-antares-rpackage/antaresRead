
#' API methods
#'
#' @param endpoint API endpoint to interrogate, it will be added after `default_endpoint`.
#'  Can be a full URL (by wrapping ìn [I()]), in that case `default_endpoint` is ignored.
#' @param ... Additional arguments passed to API method.
#' @param default_endpoint Default endpoint to use.
#' @param opts Antares simulation options or a `list` with an `host = ` slot.
#'
#' @return Response from the API.
#' @export
#'
#' @name API-methods
#'
#' @importFrom httr GET accept_json stop_for_status content add_headers timeout
#'
#' @examples
#' \dontrun{
#'
#' # List studies with local API
#' api_get(
#'   opts = list(host = "http://0.0.0.0:8080"),
#'   endpoint = NULL
#' )
#'
#' }
api_get <- function(opts, endpoint, ..., default_endpoint = "v1/studies") {
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
    opts$timeout <- 60
  result <- GET(
    url = paste(c(opts$host, default_endpoint, endpoint), collapse = "/"),
    config = config,
    timeout(opts$timeout),
    ...
  )
  stop_for_status(result)
  content(result)
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
    url = paste(c(opts$host, default_endpoint, endpoint), collapse = "/"),
    config = config,
    ...
  )
  stop_for_status(result)
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
    url = paste(c(opts$host, default_endpoint, endpoint), collapse = "/"),
    config,
    ...
  )
  stop_for_status(result)
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
    url = paste(c(opts$host, default_endpoint, endpoint), collapse = "/"),
    config = config,
    ...
  )
  stop_for_status(result)
  content(result)
}
