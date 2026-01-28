# Read configuration options from file or API

Read configuration options from file or API

## Usage

``` r
readIni(pathIni, opts = antaresRead::simOptions(), default_ext = ".ini")

readIniFile(file, stringsAsFactors = FALSE)

readIniAPI(study_id, path, host, token = NULL)
```

## Arguments

- pathIni:

  Path to config/ini file to read.

- opts:

  List of simulation parameters returned by the function
  [`setSimulationPath()`](setSimulationPath.md)

- default_ext:

  Default extension used for config files.

- file:

  File path.

- stringsAsFactors:

  logical: should character vectors be converted to factors?

- study_id:

  Study's identifier.

- path:

  Path of configuration object to read.

- host:

  Host of AntaREST server API.

- token:

  API personnal access token.

## Value

A list with an element for each section of the .ini file.

## Examples

``` r
if (FALSE) { # \dontrun{
library(antaresRead)
library(antaresEditObject)

# With physical study:
setSimulationPath("../tests-studies/Study_V8.2/", simulation = "input")
readIni("settings/generaldata")

# With API
setSimulationPathAPI(
  host = "http://localhost:8080",
  study_id = "73427ae1-be83-44e0-b04f-d5127e53424c",
  token = NULL,
  simulation = "input"
)
readIni("settings/generaldata")

} # }
```
