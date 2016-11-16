# Read data from an Antares study with R package 'antares'


## Installation

To install the package from Github, you will need to create a personal access token (PAT) here: https://github.com/settings/tokens .

```r
# Install dependencies
install.packages(c("data.table", "plyr", "lubridate", "devtools"))
install_github("rte-antares-rpackage/antares-rpackageRead", auth_token = "your_pat")
```

If you are behind a proxy, you need to first run this code:
```r
library(httr)
set_config(use_proxy("XXX.XXX.XX.XX", port=XXXX, username="proxy_user", password="passwd"))
```

To install the last development version:
```r
install_github("rte-antares-rpackage/antares-rpackageRead", 
               auth_token = "your_pat", ref ="develop")
```

To display the help of the package and see all the functions it provides, type:
```r 
help'(package="antaresRead")
```

To see a practical example of use of the package, look at the vignette:
```r
vignette("antares")
```


## Initialisation

Load the package

```r
library(antares)
```

Select an Antares simulation interactively.

```r
setSimulationPath()
```

You can also select it programmatically:

```r
setsimulationPath("study_path", simulation)
```

The parameter `simulation` can be the name of a simulation, the name of the folder containing the simulation results, or the index of the simulation. `1` corresponds to the oldest simulation, `-1` to the newest one. 


## Read data from a simulation

Most data from a simulation can be imported in the R session with function `readAntares()`. It has many parameters that control what data is imported. Here are a few examples: 

```r
# Read synthetic results of all areas of a study with hourly time step.
areaData <- readAntares(areas = "all")

# Same but with a daily time step:
areaData <- readAntares(areas = "all", timeStep = "daily")

# Read all Monte Carlo scenarios for a given area.
myArea <- readAntares(areas = "my_area", synthesis = FALSE)

# Same but add miscelaneous production time series to the result 
myArea <- readAntares(areas = "my_area", synthesis = FALSE, misc = TRUE)

# Read only columns "LOAD" and "MRG. PRICE"
areaData <- readAntares(areas = "all", select = c("LOAD", "MRG. PRICE"))
```

Functions `getAreas` and `getLinks` are helpful to create a selection of areas or links of interest. Here are a few examples:

```r
# select areas containing "fr"
myareas <- getAreas("fr")

# Same but remove areas containing "hvdc"
myareas <- getAreas("fr", exclude = "hvdc")

# Get the links that connect two of the previous areas
mylinks <- getLinks(myareas, internalOnly = FALSE)

# Get the results for these areas and links
mydata <- readAntares(areas = myareas, links = mylinks)
```

## Work with the imported data

When only one type of elements is imported (only areas or only links, etc.) `readAntares()` read antares returns a `data.table` with some extra attributes. A `data.table` is a table with some enhanced capacities offered by package `data.table`. In particular it provides a special syntax to manipulate its content:

```r
name_of_the_table[filter_rows, select_columns, group_by]
```

Here are some examples:

```r
# Select lines based on some criteria
mydata[area == "fr" & month = "JUL"]

# Select columns, and compute new ones
mydata[, .(area, month, load2 = LOAD^2)]

# Aggregate data by some variables
mydata[, .(total = sum(LOAD)), by = .(month)]

# All three operations can be done with a single line of code
mydata[area == "fr", .(total = sum(LOAD)), by = .(month)]

help(package = "data.table")
```

If you are not familiar with package `data.table`, you should have a look at the documentation and especially at the vignettes of the package:

```r
help(package="data.table")
vignette("datatable-intro")
```


