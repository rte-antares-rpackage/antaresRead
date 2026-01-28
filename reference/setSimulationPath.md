# Set Path to an Antares simulation

This function has to be used before the `read` functions. It sets the
path to the Antares simulation to work on and other useful options (list
of areas, links, areas with clusters, variables, etc.). On local disk
with `setSimulationPath` or on an AntaREST API with
`setSimulationPathAPI`

## Usage

``` r
setSimulationPath(path, simulation = NULL)

setSimulationPathAPI(
  host,
  study_id,
  token,
  simulation = NULL,
  timeout = 600,
  httr_config = list()
)
```

## Arguments

- path:

  (optional) Path to the simulation. It can either be the path to a
  directory containing an antares project or directly to the directory
  containing the output of a simulation. If missing, a window opens and
  lets the user choose the directory of the simulation interactively.
  Can also choose .h5 file, if `rhdf5` is installed.

- simulation:

  (optional) Only used if "path" represents the path of a study and not
  of the output of a simulation. It can be either the name of the
  simulation or a number indicating which simulation to use. It is
  possible to use negative values to select a simulation from the last
  one: for instance -1 will select the most recent simulation, -2 will
  the penultimate one, etc. There are two special values 0 and "input"
  that tells the function that the user is not interested by the results
  of any simulation, but only by the inputs. In such a case, the
  function [`readAntares`](readAntares.md) is unavailable.

- host:

  `character` host of AntaREST server API

- study_id:

  `character` id of the target study on the API

- token:

  `character` API personnal access token

- timeout:

  `numeric` API timeout (Default to 600 seconds).

- httr_config:

  API httr configuration. See
  [`config`](https://httr.r-lib.org/reference/config.html)

## Value

A list containing various information about the simulation, in
particular:

- studyPath:

  path of the Antares study

- simPath:

  path of the simulation

- inputPath:

  path of the input folder of the study

- studyName:

  Name of the study

- simDataPath:

  path of the folder containing the data of the simulation

- name:

  name of the simulation

- mode:

  type of simulation: economy, adequacy, draft or input

- synthesis:

  Are synthetic results available ?

- yearByYear:

  Are the results for each Monte Carlo simulation available ?

- scenarios:

  Are the Monte-Carlo scenarii stored in output ? This is important to
  reconstruct some input time series that have been used in each
  Monte-Carlo simulation.

- mcYears:

  Vector containing the number of the exported Monte-Carlo scenarios

- antaresVersion:

  Version of Antares used to run the simulation.

- areaList:

  Vector of the available areas.

- districtList:

  Vector of the available districts.

- linkList:

  Vector of the available links.

- areasWithClusters:

  Vector of areas containing clusters.

- areasWithResClusters:

  Vector of areas containing clusters renewable.

- areasWithSTClusters:

  Vector of areas containing clusters storage (\>=v8.6.0).

- variables:

  Available variables for areas, districts and links.

- parameters:

  Other parameters of the simulation.

- binding:

  Table of time series dimensions for each group (\>=v8.7.0).

- timeIdMin:

  Minimum time id of the simulation. It is generally equal to one but
  can be higher if working on a subperiod.

- timeIdMax:

  maximum time id of the simulation.

- start:

  Date of the first day of the year in the simulation. This date
  corresponds to timeId = 1.

- firstWeekday:

  First day of the week.

- districtsDef:

  data.table containing the specification of the districts.

- energyCosts:

  list containing the cost of spilled and unsupplied energy.

- verbose:

  `logical` default to FALSE, put to TRUE to manage diagnostic messages

- sleep:

  timer for api commande execute

## Details

The simulation chosen with `setSimulationPath` or `setSimulationPathAPI`
becomes the default simulation for all functions of the package. This
behavior is fine when working on only one simulation, but it may become
problematic when working on multiple simulations at same time.

In such case, you can store the object returned by the function in a
variable and pass this variable to the functions of the package (see
examples).

## See also

[`simOptions`](simOptions.md), [`readAntares`](readAntares.md),
[`readLayout`](readLayout.md), [`readClusterDesc`](readClusterDesc.md),
[`readBindingConstraints`](readBindingConstraints.md)

[`setTimeoutAPI`](setTimeoutAPI.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Select interactively a study. It only works on windows.

setSimulationPath()

# Specify path of the study. Note: if there are more than one simulation
# output in the study, the function will asks the user to interactively choose
# one simulation.

setSimulationPath("path_of_the_folder_of_the_study")

# Select the first simulation of a study

setSimulationPath("path_of_the_folder_of_the_study", 1)

# Select the last simulation of a study

setSimulationPath("path_of_the_folder_of_the_study", -1)

# Select a simulation by name

setSimulationPath("path_of_the_folder_of_the_study", "name of the simulation")

# Just need to read input data

setSimulationPath("path_of_the_folder_of_the_study", "input")
# or
setSimulationPath("path_of_the_folder_of_the_study", 0)

# Working with API
#--------------------------
setSimulationPathAPI(
    host = "http://antares_api_adress",
    study_id = "study_id_on_api",
    token = "token"
)

## Custom httr options ?

# global using httr package
require(httr)
set_config(verbose())
setSimulationPathAPI(
    host = "http://antares_api_adress",
    study_id = "study_id_on_api",
    token = "token"
)

reset_config()

# or in setSimulationPathAPI
setSimulationPathAPI(
    host = "http://antares_api_adress",
    study_id = "study_id_on_api",
    token = "token",
    httr_config = config(verbose = TRUE)
)

# disable ssl certificate checking ?
setSimulationPathAPI(
    host = "http://antares_api_adress",
    study_id = "study_id_on_api",
    token = "token",
    httr_config = config(ssl_verifypeer = FALSE)
)

# WORKING WITH MULTIPLE SIMULATIONS
#----------------------------------
# Let us assume ten simulations have been run and we want to collect the
# variable "LOAD" for each area. We can create a list containing options
# for each simulation and iterate through this list.

opts <- lapply(1:10, function(i) {
   setSimulationPath("path_of_the_folder_of_the_study", i)
})

output <- lapply(opts, function(o) {
  res <- readAntares(areas = "all", select = "LOAD", timeStep = "monthly", opts = o)
  # Add a column "simulation" containing the name of the simulation
  res$simulation <- o$name
  res
})

# Concatenate all the tables in one super table
output <- rbindlist(output)

# Reshape output for easier comparisons: one line per timeId and one column
# per simulation
output <- dcast(output, timeId + areaId ~ simulation, value.var = "LOAD")

output

# Quick visualization
matplot(output[area == area[1], !c("area", "timeId"), with = FALSE],
        type = "l")
} # }
```
