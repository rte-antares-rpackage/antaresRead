# Import clusters description

This function reads in the input files of an antares study the
properties of each cluster.

Be aware that clusters descriptions are read in the input files so they
may have changed since a simulation has been run.

## Usage

``` r
readClusterDesc(opts = simOptions(), dot_format = TRUE)

readClusterResDesc(opts = simOptions(), dot_format = TRUE)

readClusterSTDesc(opts = simOptions(), dot_format = TRUE)
```

## Arguments

- opts:

  list of simulation parameters returned by the function
  [`setSimulationPath`](setSimulationPath.md)

- dot_format:

  `logical` default TRUE to return `character` with "valid" format (see
  [`make.names()`](https://rdrr.io/r/base/make.names.html))

## Value

A `data.table` with one line per cluster.

Columns are displayed using the 3 key columns (*area*, *cluster*,
*group*). The rest of the properties are displayed according to cluster
type ("thermal", "renewable" or "st-storages").

key columns:

- area:

  Name of the area containing the cluster

- cluster:

  Name of the cluster

- group:

  Type of cluster (gaz, nuclear, etc.)

By default, the function reads the cluster description of the default
antares study. You can use the argument `opts` to specify another study.

`readClusterDesc` : read thermal clusters

`readClusterResDesc` : read renewable clusters (Antares \>= V8.1)

`readClusterSTDesc` : read st-storage clusters (Antares \>= V8.6)

If you have no clusters properties,
`Null data.table (0 rows and 0 cols)` is returned.

## Warning

You have now two format output to display input properties. Default is
format uses by operating team, eg `min.down.time`. Other format is
according to antares simulator, eg `min-down-time`.

All properties are returned with default values according to Antares
Study version.

## Examples

``` r
if (FALSE) { # \dontrun{

# Default format with "dot separator"

# thermal
readClusterDesc()

# renewable
readClusterResDesc()

# st-storage
readClusterSTDesc()

# Antares Simulator format 

#' # thermal
readClusterDesc(dot_format = FALSE)

# renewable
readClusterResDesc(dot_format = FALSE)

# st-storage
readClusterSTDesc(dot_format = FALSE)


# By default, the function reads cluster descriptions for the default study,
# but it is possible to specify another study with parameter "opts"
sim1 <- setSimulationPath()

#[... code that modifies the default antares study]

readClusterDesc(sim1)

} # }
```
