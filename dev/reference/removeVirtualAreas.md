# Remove virtual areas

This function removes virtual areas from an `antaresDataList` object and
corrects the data for the real areas. The `antaresDataList` object
should contain area and link data to function correctly.

## Usage

``` r
removeVirtualAreas(
  x,
  storageFlexibility = NULL,
  production = NULL,
  reassignCosts = FALSE,
  newCols = TRUE,
  rowBal = TRUE,
  prodVars = getAlias("rmVA_production"),
  costsVars = c("OV. COST", "OP. COST", "CO2 EMIS.", "NP COST"),
  costsOn = c("both", "storageFlexibility", "production")
)
```

## Arguments

- x:

  An object of class `antaresDataList` with at least components `areas`
  and `links`.

- storageFlexibility:

  A vector containing the names of the virtual storage/flexibility
  areas. Can also be a named list. Names are columns to add and elements
  the virtual areas to group.

- production:

  A vector containing the names of the virtual production areas.

- reassignCosts:

  If TRUE, the production costs of the virtual areas are reallocated to
  the real areas they are connected to. If the virtual areas are
  connected to a virtual hub, their costs are first reallocated to the
  hub and then the costs of the hub are reallocated to the real areas.

- newCols:

  If `TRUE`, new columns containing the production of the virtual areas
  are added. If FALSE their production is added to the production of the
  real areas they are connected to.

- rowBal:

  If `TRUE`, then BALANCE will be corrected by ROW. BAL: BALANCE :=
  BALANCE - "ROW. BAL"

- prodVars:

  Virtual productions columns to add to real area. Default to
  `getAlias("rmVA_production")`

- costsVars:

  If parameter `reassignCosts` is TRUE, affected columns. Default to
  `OV. COST`, `OP. COST`, `CO2 EMIS.` and `NP COST`

- costsOn:

  If parameter `reassignCosts` is TRUE, then the costs of the virtual
  areas are reassigned to the real areas they are connected to. You can
  choose to reassigned production & storageFlexibility virtuals areas
  ("both", default), or only "production" or "storageFlexibility"
  virtuals areas

## Value

An `antaresDataList` object in which virtual areas have been removed and
data of the real has been corrected. See details for an explanation of
the corrections.

## Details

Two types of virtual areas have been defined corresponding to different
types of modeling in Antares and different types of post-treatment to
do:

- Flexibility/storage areas are areas created to model pumping unit or
  any other flexibility that behave as a storage. For those virtual
  areas, the important results are flows on the links.

- Production areas are areas created to isolate some generation from the
  "real" areas. They can be isolate for several reasons: to distinguish
  time-series (for example wind onshore/offshore), to select some
  specific unit to participate to day-ahead reserve, etc.

`removeVirtualAreas` performs different corrections:

- Correct the balance of the real areas (and districts) by removing the
  flows to or from virtual areas.

- If parameter `reassignCosts` is TRUE, then the costs of the virtual
  areas are reassigned to the real areas they are connected to. The
  default affected columns are `OV. COST`, `OP. COST`, `CO2 EMIS.` and
  `NP COST`. If a virtual area is connected to a single real area, all
  its costs are attributed to the real area. If it is connected to
  several real areas, then costs at a given time step are divided
  between them proportionally to the flows between them and the virtual
  area. An aggregation is done at the end to correct districts costs.

- For each storage/flexibility area, a column named like the area is
  created. It contains the values of the flow between the virtual area
  and the real areas. This column is interpreted as a production of
  electricity: it is positive if the flow from the virtual area to the
  real area is positive and negative otherwise. If parameter `newCols`
  is `FALSE`, the values are added to the variable `PSP` and the columns
  is removed. An aggregation is done at the end to add virtual
  storage/flexibility to districts.

- If the parameter `production` is specified, then the non null
  productions of the virtual areas are either added to the ones of the
  real areas they are connected to if `newCols = FALSE` or put in new
  columns if `newCols = TRUE`. In the second case the columns are named
  `*_virtual` where "`*`" is a type of production (wind, solar, nuclear,
  ...). Productions that are zero for all virtual areas are omited. If
  virtual production areas contains clusters then they will be move to
  the real area. An aggregation is done at the end to add virtual
  production to districts.

- Finally, virtual areas and the links connected to them are removed
  from the data.

The functions makes a few assumptions about the network. If they are
violated it will not act correctly:

- storage/flexibility areas can be connected to other
  storage/flexibility areas (hubs), but at least one of them is
  connected to a real area. That means that there is no group of virtual
  areas disconnected from the real network. If such a group exists, you
  can either remove them manually or simply not import them.

- production areas are connected to one and only one real area. They
  cannot be connected to virtual areas. But a real area may by connected
  to several production areas.

## Examples

``` r
if (FALSE) { # \dontrun{

# Assume we have a network with two virtual areas acting as pump storage and
# an area representing offshore production
#
#  offshore
#     |
# real area - psp in
#           \
#             psp out
#

data <- readAntares(areas="all", links="all")

# Remove pump storage virtual areas

correctedData <- removeVirtualAreas(
    x = data, 
    storageFlexibility = c("psp in", "psp out"),
    production = "offshore"
)
                                    
correctedData_list <- removeVirtualAreas(
    x = data, 
    storageFlexibility = list(PSP = c("psp in", "psp out")),
    production = "offshore"
)
 
                                   
correctedData_details <- removeVirtualAreas(
    x = data, 
    storageFlexibility = list(PSP_IN = "psp in", PSP_OUT =  "psp out"),
    production = "offshore"
)
                                    
} # }
```
