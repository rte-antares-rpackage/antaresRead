# Read digest file

Read digest file

## Usage

``` r
readDigestFile(opts, endpoint = "mc-all/grid/digest.txt")
```

## Arguments

- opts:

  simulation options

- endpoint:

  Suffix of path for digest file Default is : "mc-all/grid/digest.txt"
  added to opts\$simDataPath

## Value

list of 5 tables (begin, areas, middle, links lin., links quad.)
