# show aliases for variables

Aliases are short names that can be used in the `select` parameter in
function [`readAntares`](readAntares.md) to tell the function which
columns and/or type of data to import.

`setAlias` can be used to create a new alias. It can be especially
useful for package developers to help their users select the data
required by their packages.

`getAlias` return character vector containing columns and/or types of
data

`showAliases` lists available aliases

## Usage

``` r
showAliases(names = NULL)

setAlias(name, desc, select)

getAlias(name)
```

## Arguments

- names:

  optional vector of alias names. If provided, the full list of columns
  selected by these aliases is displayed. Else only the name and a short
  description of all aliases is displayed.

- name:

  Alias name

- desc:

  Short description indicating why the new alias is interesting

- select:

  character vector containing columns and/or types of data to import.

## Value

`setAlias` is only used for its side effects. A data.frame with columns
'name', 'desc' and 'select'. `showAliases` invisibly returns a
data.frame with columns "name", "desc" and "select".

## Examples

``` r
# Display the short description of an alias
showAliases()
#>              name                                                        desc
#> 1         economy      Production costs, prices, exchanges and spilled energy
#> 2        adequacy                                          Adequacy variables
#> 3      generation Production that can be controlled: thermal and hydrostorage
#> 4       renewable                                       Renewable productions
#> 5         thermal                                         Thermal productions
#> 6         netLoad                          Variables used to compute net load
#> 7 rmVA_production                     removeVirtualAreas production varaibles
#> 8          nostat    All variables except summary variable (MIN, MAX and STD)

# Display the full description of an alias
showAliases("renewable")
#>        name                  desc
#> 4 renewable Renewable productions
#>                                                                                                                                                                                  select
#> 4 WIND, WIND OFFSHORE, WIND ONSHORE, SOLAR, SOLAR CONCRT., SOLAR PV, SOLAR ROOFT, RENW. 1, RENW. 2, RENW. 3, RENW. 4, H. ROR, H. STOR, MISC. DTG, MISC. DTG 2, MISC. DTG 3, MISC. DTG 4

getAlias("renewable")
#>  [1] "WIND"          "WIND OFFSHORE" "WIND ONSHORE"  "SOLAR"        
#>  [5] "SOLAR CONCRT." "SOLAR PV"      "SOLAR ROOFT"   "RENW. 1"      
#>  [9] "RENW. 2"       "RENW. 3"       "RENW. 4"       "H. ROR"       
#> [13] "H. STOR"       "MISC. DTG"     "MISC. DTG 2"   "MISC. DTG 3"  
#> [17] "MISC. DTG 4"  

if (FALSE) { # \dontrun{
# Create a new alias that imports flows
setAlias("test", "short description", c("links", "FLOW LIN.")) 
showAliases()
} # }
```
