# API methods

API methods

## Usage

``` r
api_get(
  opts,
  endpoint,
  ...,
  default_endpoint = "v1/studies",
  parse_result = NULL,
  encoding = NULL
)

api_post(opts, endpoint, ..., default_endpoint = "v1/studies")

api_put(opts, endpoint, ..., default_endpoint = "v1/studies")

api_delete(opts, endpoint, ..., default_endpoint = "v1/studies")
```

## Arguments

- opts:

  Antares simulation options or a `list` with an `host = ` slot.

- endpoint:

  API endpoint to interrogate, it will be added after
  `default_endpoint`. Can be a full URL (by wrapping ìn
  [`I()`](https://rdrr.io/r/base/AsIs.html)), in that case
  `default_endpoint` is ignored.

- ...:

  Additional arguments passed to API method.

- default_endpoint:

  Default endpoint to use.

- parse_result:

  `character` options for parameter `as` of function
  [`httr::content()`](https://httr.r-lib.org/reference/content.html)

- encoding:

  argument to pass as argument to the function
  [`httr::content()`](https://httr.r-lib.org/reference/content.html)

## Value

Response from the API.

## Examples

``` r
if (FALSE) { # \dontrun{

# List studies with local API
# default result content in R object (auto parsed)
api_get(opts = list(host = "http://0.0.0.0:8080"),
        endpoint = NULL, 
        parse_result = NULL)

# You can force parse options as text and encoding to UTF-8
api_get(opts = list(host = "http://0.0.0.0:8080"),
        endpoint = NULL, 
        parse_result = "text",
        encoding = "UTF-8")
        
# You can change or delete `default_endpoint` 

# no use `default_endpoint`
api_get(opts = list(host = "http://0.0.0.0:8080"),
        endpoint = NULL, 
        default_endpoint = NULL,
        parse_result = "text",
        encoding = "UTF-8")
        
# replace `default_endpoint`
api_get(opts = list(host = "http://0.0.0.0:8080"),
        endpoint = NULL, 
        default_endpoint = "myfolder/myfolder",
        parse_result = "text",
        encoding = "UTF-8")       
} # }
```
