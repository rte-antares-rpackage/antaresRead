# Read areas layout

This function reads in the input files of an antares study the current
areas layout, ie. the position of the areas It may be useful for
plotting the network.

Be aware that the layout is read in the input files so they may have
changed since a simulation has been run.

## Usage

``` r
readLayout(opts = simOptions(), xyCompare = c("union", "intersect"))
```

## Arguments

- opts:

  list of simulation parameters returned by the function
  [`setSimulationPath`](setSimulationPath.md)

- xyCompare:

  Use when passing multiple opts, can be "union" or "intersect".

## Value

A list with three elements:

- areas: :

  A data.frame containing the name, the color and the coordinate of each
  area

- district: :

  A data.frame containing the name, the color and the coordinate of each
  district

- links: :

  A data.frame containing the name, the coordinates of the origin and
  the destination of each link

By default, `readLayout` reads the layout for the current default
antares study. It is possible to specify another study with the
parameter `opts`. And we can pass multiple studies using a `list` of
opts.

## Examples

``` r
if (FALSE) { # \dontrun{
readLayout()

# By default, the function reads layout for the default study,
# but it is possible to specify another study with parameter "opts"
sim1 <- setSimulationPath()

#[... code that modifies the default antares study]

readLayout(sim1)

} # }
```
