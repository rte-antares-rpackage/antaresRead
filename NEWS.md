> Copyright © 2016 RTE Réseau de transport d’électricité

# antaresRead 2.9.2

NEW FEATURES (cf. Antares v9.2 changelog) :

* `readClusterSTDesc()` read new clusters parameters (parameters names are now sorted)
* `readInputTS()` read new optional time Series (5 time series)
* **NEW FUNCTION** `read_storages_constraints()` read properties and time series of a *short-term storages/additional constraints*


NEW FEATURES (other) :  

* `readBindingConstraints()` : has a new parameter `'constraint_names'` so the user can read only the binding constraints he wants (optimization)
* `getThematicTrimming()` to read sub section "variables selection" of file `generaldata.ini`  
  - Use new referentials, twice for file system and one for API  
  - New function `list_thematic_variables()` to display available columns according to current study version
* `readAntares()` : has a new parameter `'number_of_batches'` to choose the number of batches for the data you read. Only available in API mode.


BUGFIXES :

* `.manage_list_structure()` : returns the `comments` property in `properties` instead of `coefs`
* `.giveInfoRequest()` : if argument `clustersRes` is not null, argument `areas` should not be equal to `all`
* `.getSimOptions()` : add `Expansion` mode to compute `simDataPath` value
* `.giveSize()` : control the size of the renewables outputs

BREAKING CHANGES :  
 
* `.importOutputForAreas()` / `.importOutputForLinks()` / `.importOutputForClusters()` / `.importOutputForResClusters()` / `.importOutputForSTClusters()` : uses specific endpoint in API mode
* `.api_get_aggregate_areas()` / `.api_get_aggregate_links()` : retrieve aggregated areas/links raw data from study economy outputs
* `format_api_aggregate_result()` : match the legacy names for the output column names
* Rename short-term storage output column from Cashflow to CashFlow

REVDEP (temporary) : 

✔ antaresEditObject 0.9.0 ── E: 0     | W: 0     | N: 0                                                            
✔ antaresProcessing 0.18.3 ── E: 0     | W: 0     | N: 0                                                            
✔ antaresViz 0.18.3 ── E: 0     | W: 0     | N: 0 

DOC :  

* pkgdown site updated to bootsrap 5


# antaresRead 2.9.1 (NOT CRAN)

NEW FEATURES:  
 
* `readBindingConstraints()` : has a new parameter `'with_time_series'` (default to `TRUE`) to enable or disable the time series reading (optimization)

BUGFIXES :  
 
* `api_get() / api_post () / api_put() / api_delete()` : treat case when default_endpoint provided is empty  
* `setSimulationPathAPI()` : The version number returned for a study >= 9.2 (9.2*100) is in fact considered by R as <920 with a precision of `-1.136868e-13`. This falsifies the version number checks (atypical error depending on machine precision, see R doc `?double`)
 
BREAKING CHANGES :  
 
* `setSimulationPathAPI()` : sets timeout to 600s by default. 600s is the default value in Antares Web.

# antaresRead 2.9.0

NEW FEATURES:  

* `setSimulationPathAPI()` : reads and returns the new converted study version format (ex : 9.0 => 900)
* `setSimulationPath()` / `setSimulationPathAPI()` have a new parameter `'verbose'` (default to `FALSE`) to manage diagnostic messages


BUGFIXES :  

* `setSimulationPathAPI()` : encode URL before reading the data in simulation mode
* `api_get()` : add warn_for_status in importFrom section
* `readAntares()` : In disk mode, return all the available columns for a short-term storage output and match the column with the content
* `.importOutput()` : check if output file exists in API mode (`.check_missing_output_files()`)
* `.giveSize()` : take into account ST clusters in the size computing and use enabled == TRUE or empty enabled for enabled clusters and ST clusters 

BREAKING CHANGES :  

* `readClusterDesc()` / `readClusterResDesc()` / `readClusterSTDesc()` have a new parameter (`dot_format = TRUE`) to return two format to display input cluster properties

GITHUB ACTIONS :  

* Actions artifacts v3 is closing down, update to v4  
* test-coverage.yaml updated 



# antaresRead 2.7.2

NEW FEATURES:

* New function `readAntaresSTClusters()`
* `fread_antares()` shiny compatible with a conditional processing of the error messages

BREAKING CHANGES :

* `readClusterDesc()` / `readClusterResDesc()` / `readClusterSTDesc()` are updated with new endpoint "table mode".  
  - In "text" mode, functions return all properties (with default properties) according to study version.

BUGFIXES :  

* `setSimulationPathAPI()`: control the existence of the output folder **links** or **areas** before reading the data (upgrade Antares Web)
* `readClusterDesc()` / `readClusterResDesc()` / `readClusterSTDesc()` return a data.table in API mode
* `setSimulationPathAPI()` : encode URL before reading the data in simulation mode


# antaresRead 2.7.1 

NEW FEATURES:

* `readInputThermal()` :
    - new parameter **areas** to get desired clusters from selected areas.
    - new parameter **thermalAvailabilities** to import time series.  
* `readInputRES()` new parameter **areas** to get desired clusters from selected areas.
* `setSimulationPath()` return a new parameter `binding` (for studies >= v8.7.0). 
It contains a table with group dimensions of time series for binding constraints.
* `readAntares()` new parameter **clustersST** to read (output simulation) short-term clusters

BREAKING CHANGES :

* `readInputThermal()` / `readInputRES()` default value when no time series in the selected clusters.

BUGFIXES :  

* `readInputThermal()` return data from file data.txt with `thermalData` parameter
* `setSimulationPath()` has also the parameter **areasWithSTClusters** in 'output' mode


# antaresRead 2.7.0

### Breaking changes (Antares v8.7.0) : 

* `readBindingConstraints()` read now Scenarized RHS for binding constraints (cf. Antares v8.7 changelog)
  - function returns a new list structure
* Private function `fread_antares()` no longer returns warnings  
* `api_put()/api_delete()` return a server error message 

BUGFIXES :  

* `readBindingConstraints()` read well study >= v8.3.2

DATA : 

* A test study in tar.gz format is available in version v8.7.0  
* An empty test study in version v8.7.0 for marginal cases 

Dependencies :  

* New package `lifecycle` to manage functions status/package status



# antaresRead 2.6.2 

BUGFIXES :
* `readIniFile()` : avoid `utils::type.convert` on specific cases (ex : 789e or 123i) 
* `api_get()` add encoding argument to pass to `httr::content()`


# antaresRead 2.6.1

BUGFIXES :  

* `setSimulationPathAPI()` :  
  - returns an API exception if the requested study ID is incorrect 
  - `simulation` the simulation parameter works with negative values within the limit of the number of simulations
* correction in `readClusterDesc()` calls to add "opts"
* `readAntares()` :  
  - returns the right column names for details-timeStep.txt and details-res-timeStep.txt 
* Correction in `.formatlist()`, read N-level list instead of 2.

BREAKING CHANGES :  

* `api_get()` has a new parameter to control JSON file parsing
* `readInputThermal()` default value when no time series in the selected clusters.
* `readInputRES()` default value when no time series in the selected clusters
* `readClusterDesc()`/ `readClusterRESDesc()` / `readClusterSTDesc()` 
return empty dataTable and warning if no cluster in Antares study.

# antaresRead 2.6.0

NEW FEATURES (Antares v8.6, cf. Antares v8.6 changelog) :

* `readClusterSTDesc()` read "short-term storage" clusters parameters (input files of an antares study)


BREAKING CHANGES (Antares v8.6) :

* `readInputTS()` is now compatible to read time series with :  
  - "short-term storage"  
  - "mingen" (pmin hydro value)
* `setSimulationPath()` has new parameter **areasWithSTClusters** (name of area with "st-storage" cluster)


BUGFIXES : 

* `setSimulationPathAPI` generate new global parameter `sleep` to add timer to API request
* Correction of `.importOutput()` to use `readAntares()` with `parallel == TRUE` in shiny application
* `setSimulationPathAPI()` delete a redundant API request
* `readClusterDesc()` minor fix in API mode + fix if no cluster exists => return specific error message
* `readIniAPI()` read well file `generaldata` for sections "playlist" and "variables selection"


DATA : 

* A test study in tar.gz format is available in version `v8.6.0`


# antaresRead 2.5.1

BUGFIXES:

* Major correction to `setSimulationPath()` (mc-all/mc-ind) (#199)

DEV:

* Added new test study v8.5.
* Package no longer tested on study v6.



# antaresRead 2.5.0

NEW FEATURES:

* Full support of studies up to v8.5
* Function `readAntares()` has new argument for binding constraints output (v8.4+) (#173)
* New functions `readDigestFile()`, `mergeDigests()` and `writeDigest()` to manipulate digest file.
* Mc-all aggregation : 
  * added computation of record years
  * merges original `digest.txt` with the one created
  * loads original `thermal.txt`

BUGFIXES:

* Major corrections to mc-all and mc-ind aggregation (LOLP, H. LEV)
* Fixed existing write digest.txt part in mc-all aggregation 
* Fixed copy of original links in mc-all aggregation



# antaresRead 2.4.2

NEW FEATURES:

* New function `getGeographicTrimming()` returns filtering options for selected areas (links optional).
* New function `readInputRes()` for reading renewable clusters input data 
* Existing function `getLinks()` now has a new argument **withTransmission**. if TRUE, return additional column with type of transmission capacities.
* Existing function `readInputThermal()` : added new argument for thermalData



# antaresRead 2.4.1

NEW FEATURES:

Added new functions `readInputThermal()` and `readAntaresClusters()` :
Both functions take a vector of **clusters** instead of **areas** 
* `readInputThermal()` : read thermal TS (availabilities) and modulation in Input mode
* `readAntaresClusters()` : read output data for clusters only with thematic trimming



# antaresRead 2.4.0

NEW FEATURES:

Major upgrade to `aggregateResult()` and `parAggregateMCall()` :
* Faster & memory efficient
* Support for Antares studies up to v8.3 (v8.4 experimental)
* Dynamic timestep detection
* Creation of grid folder
* Recycling of original mc-all data



# antaresRead 2.3.2

NEW FEATURES:

added "profit by cluster" when reading cluster data.

BUGFIXES:

Fix for 404 error when some output is missing in API mode(#188).



# antaresRead 2.3.1

BUGFIXES:

* readAntares : fix memory problems for linkCapacity = TRUE (>v8.2)
* Support for url with special characters (#181)
* Fix for setSimulationPathAPI not working with EnR aggregated (<v8.1) (#180)



# antaresRead 2.3.0

* Interact directly with AntaREST API with `api_get()`, `api_post()`, `api_put()`, `api_delete()`.
* New function to read ini files : `readIni()`.



# antaresRead 2.2.97

FEATURES:

* NTC by monte carlo years, antares 8.2 update
  * Update readInputTS when linkCapacity argument is used
  * New link inputs 



# antaresRead 2.2.96

BUGFIXES:

* #168 : clean memory parAggregateMCall 



# antaresRead 2.2.95

BUGFIXES:

* removeVirtualAreas : setting custom prodVars same as new storageVars
* API : fix study checking + httr options



# antaresRead 2.2.9

FEATURES:

* Add ENR Cluster support (V8.1)
* removeVirtualAreas : Can pass storageFlexibility named list
* removeVirtualAreas : add prodVars, costsVars and costsOn arguments
* fix linCapacity aggregation

BUGFIXES:

* aggregateResult parallel export (#161)



# antaresRead 2.2.8

FEATURES:

* Fixed bugs in `readAntares` when file is missing



# antaresRead 2.2.7

FEATURES:

* Fixed bugs in `readInputTS()` for `hydroStorage`
* New`hvdcModification()` function
* Add MC Weight functionnality
* First antares api support



# antaresRead 2.2.5

FEATURES:

* Fixed bugs in `removeVirtualAreas()` in `getLinks()`
* Updated documentation & added pkgdown website 



# antaresRead 2.2.4 (2019-02-13)

FEATURES:

* Compatibility with Antares v7


# antaresRead 2.2.3 (2019-02-13)

BUGFIXES:

* Error with next data.table release


# antaresRead 2.2.2 (2019-01-15)

BUGFIXES:

* Duplicated vignette title


# antaresRead 2.2.1 (2018-10-16)

BUGFIXES:

* ".getStartDate()" : was not working correctly with a first-month not equal to "July" (#121)
* ".getStartDate()" : was not working correctly when a user wants to change options with an opts  (#122)


# antaresRead 2.2.0 (2018-09-28)

BREAKING CHANGES:

* "removeVirtualAres()" : as a new logical parameter rowBal, if is TRUE (default) then BALANCE will be corrected by ROW. BAL (#antaresProcessing33)

NEW FEATURES:

* "removeVirtualAres()" : now correct BALANCE, COST and production of disticts.(#119)

BUGFIXES:

* "readLayout()" : prevent bug reading multiple studies
* "readAntares()" : prevent bug dealing with integer64
* "writeAntaresH5()" was not working anymore due to a new version of rhdf5 (h5close() does not work anymore). (#110)


# antaresRead 2.1.2 (2018-06-01)

BUGFIXES:

* getLinks() was sending an error when there are no links in the study. 


# antaresRead 2.1.1 (2018-04-28)

BUGFIXES:

* readInputTS() was sending bad values for hydroStorage. (#58)
* changeTimeStep() will print a warning if the user want to aggregate data. (#54)
* readAntares() will print warnings when a user want to aggregate mustRun data (#54)


# antaresRead 2.1.0 (2018-02-23)

NEW FEATURES:

* readInputTS() has a new parameter "thermalOutages" that permits to read thermal outages characteristics
* Call removeVirtualAreas several times in writeAntaresH5()

BUGFIXES:

* Correction of some bugs in removeVirtualAreas when there is several virtual nodes of production and storageFlexibility 
* Correction of some bugs due to the new lubridate version


# antaresRead 2.0.2 (2017-11-21)

BUGFIXES:

* Some tests failed in some platforms on CRAN.


# antaresRead 2.0.1 (2017-11-14)

BUGFIXES:

* readLayout() was not taking a list of simOptions. 


# antaresRead 2.0.0 (2017-11-03)

NEW FEATURES:

* New function "writeAntaresH5()" convert ANTARES outputs to h5 files.
* The path parameter of "setSimulationPath()" can be a h5 file.
* New function "isH5Opts()" test if the value returned by setSimulationPath() is referring to an h5 file.
* New function "readOptimCriteria()" read the value of the criteria optimized by ANTARES.
* New function "setRam()" specify RAM limit (in Go) of the value returned by readAntares, initially it is 10Go. 
* New vignette "antaresH5" is available.

BUGFIXES:

* removeVirtualAreas() was not moving clusters from virtual production area to the real area. 
* antaresRead() sometimes returned a antaresDataList with element "thermalModulation". (#51) 


# antaresRead 1.1.4 (2017-07-07)

BUGFIXES:

* setSimulation() was crashing with antares v6.0 because of the folder "maps". (#49)


# antaresRead 1.1.3 (2017-05-30)

BUGFIXES:

* readInputTS was crashing when trying to import cluster data and some clusters were disabled.


# antaresRead 1.1.2 (2017-04-18)

BUGFIXES:

* Fixed an issue that caused compilation to crash on mac OS X


# antaresRead 1.1.0 (2017-03-31)

NEW FEATURES:

* The 'select' parameter of readAntares() has been improved. It now accepts new keywords "areas", "links", "clusters", "districts" to indicate which type of data to read,"mcYears" to import detailed results, "misc", "mustRun", etc. to import the corresponding input data. 
* A new function setAlias() has been added. It permits to packages developers to define new aliases to use in the 'select' parameter of readAntares() in order to help their users to import the required data.
* readBindingConstraints() has been improved and returns an object of class 'bindingConstraints' which is a named list where each element has the same structure. 
* A summary method has been added for objects of class 'bindingConstraints'. It is useful to quickly look at the binding contraints defined in a study.
* readAntares(mustRun = TRUE) now adds a new column "thermalPmin". It is similar to mustRunTotal, except it also takes into account the minimum stable power of the cluster units. 

BUGFIXES:

* Columns 'pumpingCapacity' and 'storageCapacity' created by removeVirtualAreas() were reversed.
* setSimulationPath() was sometimes setting a start data that was not consistent with parameters "1st january" and "leap year".
* setSimulationPath() was not working if synthetic results and the first Monte-Carlo scenario were not saved in a given Antares simulation.
* readAntares(clusters = "all") was not working in some situations.



# antaresRead 1.0.0 (2017-02-23)

BUGFIXES:

* In some specific situations, date columns could be filled with NA values. (#26)
* Many small problems that were detected by R CMD CHECK.


# antaresRead 0.15 (2017-01-25)

BREAKING CHANGES:

* Parameter "synthesis" has been removed from readAntares. The new behavior is now to import synthetic results when "mcYears" is NULL and to import detailed results when "mcYears" is set.

NEW FEATURES:

* New function "getIdCols()" returns the Id columns of an antaresDataTable object.
* New method "subset()" for antaresDataList objects permits to quickly extract the data for a given area or a given Monte-Carlo scenario.

BUGFIXES:

* In "input" mode, getLinks() could return links that did not exist anymore.
* removeVirtualAreas was requiring some data that was not necesary. It should be OK now.
* setSimulationPath() was crashing when one wanted to read the input data of a study before having run any simulation.
* Variable "timeId" is now always numeric. Before it was numeric except for annual data where it could be character or factor.



# antaresRead 0.14 (2016-11-14)

NEW FEATURES:

* readAntares() has been significantly improved. Depending on the query, the time needed to import data is decreased by 20% up to 95%. In particular, importing data for clusters is now 20 times faster than before. Moreover, the progress bar is now more precise.
* removeVirtualAreas() has a new parameter "newCols" that permits to choose whether to create new columns containing the production of the virtual areas or to update the existing ones.
* removeVirtualAreas() now also removes virtual links from the data. Before it was removing virtual areas but keeping the virtual links.
* If the transmission capacity of the links is present in the data, removeVirtualAreas() now creates two new columns containing the capacity of pumping and storage of areas. These columns are used to compute upward and downward margins.
* New functions as.antaresDataTable and as.antaresDataList have been added to convert compatible objects to these classes.


# antaresRead 0.13 (2016-10-06)

BREAKING CHANGES:

* add a file LICENSE and copyright to sources files


# antaresRead 0.12 (2016-08-17)

BREAKING CHANGES:

* The object returned by 'setSimulationPath' and 'simOptions' has been modified. In particular, paths have been renamed to make their utility clearer. Moreover a new element named 'linksDef' contains a table with the specification of each link of the study.

NEW FEATURES:

* getLinks() can now return a table with the specification of the links, thanks to two new parameters "namesOnly" and "withDirection"
* It is now possible to import several times the same Monte-Carlo scenario. This can be useful for performance test or to use bootstrap methods.

BUGFIXES:

* readClusterDesc was not working in input mode.
* Sometimes setSimulationPath changed working directory.
* Solved some performance issues in readAntares() and removeVirtualAreas().


# antaresRead 0.11 (2016-08-01)

BREAKING CHANGES:

* The parameters of 'getLinks' have been modified because they were unclear. Now user has to to explicitly specify areas to include and/or exclude.   

BUGFIXES:

* setSimulationPath was crashing if parameter 'horizon' was not set in Antares.
* changeTimeStep sometimes generated NA values because of integer overflow.

# antaresRead 0.10 (2016-07-18)

BREAKING CHANGES:
* Variable mustRunModuction has been renamed minGenModulation for consistency with Antares
* Similarly in the object returned by setSimulationPath and simOptions, "setList" has been renamed "districtList"

NEW FEATURES:
* Now, when one filters, add, remove or update columns of an object of class 'antaresDataTable', the result is still of class 'antaresDataTable'.


# antaresRead 0.9.1 (2016-07-05)

BUGFIXES:

* readAntares was incorrectly setting attributes of its ouput. This resulted in errors when using functions of the antaresProcessing package.


# antaresRead 0.9 (2016-07-04)

BREAKING CHANGES:

* The columns returned by readAntares when 'hydroStorageMaxPower = TRUE' have been renamed.

NEW FEATURES:

* readAntares has a new parameter "thermalModulation" to import modulation time series.
* New aliases have been added. One can inspect them with "showAliases()".


# antaresRead 0.8 (2016-06-20)

NEW FEATURES:

* setSimulationPath nows reads costs of unsupplied et spilled energy.
* ReadLayout computes coordinates for districts and link between them.
* getAreas has a new parameter "district" to get areas in a set of districts.
* getLinks has new parameter "areas" to get links connected to a set of areas.

BUGFIXES:

* Small bug fixes that avoid useless warning messages/


# antaresRead 0.7 (2016-06-07)

BREAKING CHANGES:

* The package has been renamed "antaresRead".

NEW FEATURES:

* changeTimeStep accepts two new functions : min and max.
* readAntares now computes the number of available units.

BUGFIXES:

* setSimulationPath was crashing if parameter "horizon" was not set in Antares.
* setSimulationPath throws a warning when two simulations have same name and it uses the most recent one. 
* In presence of hubs and production nodes, removeVirtualAreas treated production nodes two times.
* Column "month" was sometimes numeric and other times character.
* setSimulationPath was not recognising option "apply-filter = add-all" when retrieving district definition.


# antaresRead 0.6 (2016-05-27)

NEW FEATURES:

* readAntares and readInputTS always add time columns (time, day, month, hour). changeTimeStep now keeps them if they are present.

BUGFIXES:

* readAntares may not work when some MC years were not exported by Antares.
* The construction of the weeks was wrong when a year did not start on january.
* The import of inputs was wrong when the study was not conducted on a whole year but on a smaller period.
* In removeVirtualNodes, option "reassignCosts" was not working correctly.


# antaresRead 0.5 (2016-05-11)

BREAKING CHANGES:

* To be consistent with Antares, 'node' has been replaced by 'area' everywhere in the package.
* Column 'MWh' is now named 'production' when reading clusters output.

NEW FEATURES:

* new function 'readInputTS' to read input time series (without output time series)
* 'setSimulationPath' now accepts two new values for the argument "simulation": 0 and "input". These values indicate that the user is not interested by the results of any simulation and only wants to read input data.
* changeTimeStep now accepts for parameter 'fun' a vector of function names that indicate how to aggregate/disagregate each column.
* getAreas has a new argument "withClustersOnly" to select areas containing clusters
* readAntares can now also add input time series to districts like it does for areas

BUGFIXES:

* 'copyToClipboard' was copying the word "txt" instead of copying data
* 'removeVirtualNodes' was not creating some columns for production virtual nodes.
* The option 'mustRun' in readAntares was not functionning correctly


# antaresRead 0.4 (2016-04-29)

BREAKING CHANGES:

* The parameters and the output of "readAntares" have been modified. The output has now only three components: nodes, links and clusters. If the user asks other information like hydro storage or links capacities, they are added to the corresponding component. The corresponding parameters now expect TRUE/ FALSE instead of a vector of node or link names.
* "getLinks" has now same parameters and behavior than "getNodes" 
* Parameter "trace" has been removed in "setSimulationPath".

NEW FEATURES:

* "readAntares" has a new argument "mustRun"
* "removeVirtualNodes" has been finalised
* New function "simOptions" to get either global simulation options or simulation options used by "readAntares" to create a given object
* New alias "nosum" to remove "min", "max", and "std" columns when "synthesis=TRUE" in "readAntares"
* "setSimulationPath" has a new argument "simulation" to choose a simulation by name or by order inside an antares project
* New function "getDistricts"
* New function "copyToClipboard"
* Added package vignette


# antaresRead 0.3 (2016-04-14)

NEW FEATURES:

* "readAntares" can now read hydro storage, hydro storage maximum power, reserve and links capacity
* New function "readAntaresNodes": read all the desired data for a set of nodes
* New function "readBindingConstraints"
* New function "changeTimeStep": change the timestep of an "antaresOutput" object or any table containing a variable timeId
* New function "getNodes": select and filter nodes of an antares study
* "extractDataList" has been improved for better compatibility with PPSE scripts
* Improved documentation
* New function removeVirtualNodes (still experimental)
* Added this NEWS file :)


# antaresRead 0.2 (2016-04-05)

BREAKING CHANGES:

* "readOutput" has been renamed "readAntares"

NEW FEATURES:

* "readAntares" can now read thermal availabilities
* "setSimulationPath" now reads simulation parameters
* "setSimulationPath" now accepts path to a study and eventually asks the user to choose an output
* Nodes and sets are now separated in "readAntares" and "setSimulationPath"
* aliases for groups of variables in parameter "select" of "readAntares"
