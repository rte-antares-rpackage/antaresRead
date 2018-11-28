#Copyright © 2016 RTE Réseau de transport d’électricité

#' @import data.table
#' @import bit64
#' @import plyr
#' @importFrom shiny withProgress incProgress getDefaultReactiveDomain
#' @importFrom methods is
#' @importFrom grDevices col2rgb rgb
#' @importFrom stats as.formula
#' @importFrom utils View read.table type.convert write.table 
#' @importFrom utils untar
#' @importFrom stringr str_match str_replace

# Private variables accessible only by functions from the package

pkgEnv <- new.env()


pkgEnv$formatName <- read.table(system.file("format_output/tableOutput.csv", package = "antaresRead"), 
                                sep = ";", header = TRUE)

pkgEnv$allCompute <- c("misc", "thermalAvailabilities", "hydroStorage", "hydroStorageMaxPower",
                       "reserve", "linkCapacity", "mustRun", "thermalModulation")

# Column names in the misc input files.
pkgEnv$miscNames <- c("CHP", "Bio_mass", "Bio_gas", "Waste", "GeoThermal", "Other", "PSP_input", "ROW_Balance")

# Column names for ren
pkgEnv$ren <- c("WIND", "SOLAR", "H. ROR", "H. STOR", "MISC. NDG")

# Column names for thermal
pkgEnv$thermal <- c("NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL", "MISC. DTG")

# Column names for generation
pkgEnv$generation <- c("NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL", "MISC. DTG", "H. STOR")

pkgEnv$production <- c("NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL",
                       "MISC. DTG", "H. STOR", "H. ROR", "WIND", "SOLAR", "MISC. NDG", "PSP")

# list of column names that are id variables in output files. When they are
# present in an output file, they are automatically imported, whatever the value
# of "select" is.
pkgEnv$idVars <- c("area", "district", "sector", "cluster", "link", "mcYear", "timeId", "time", "day", "week", "month", "hour")

pkgEnv$idTimeVars <- c("timeId", "time", "day", "week", "month", "hour")
pkgEnv$timeStepPosible <- c("hourly", "daily", "weekly", "monthly", "annual")

setAlias("economy", "Production costs, prices, exchanges and spilled energy",
         c("OV. COST", "OP. COST", "MRG. PRICE", "CO2 EMIS.", "BALANCE", "SPIL. ENRG"))
setAlias("adequacy", "Adequacy variables",
         c("UNSP. ENRG", "LOLD", "LOLP", "AVL DTG", "DTG MRG", "MAX MRG"))
setAlias("generation", "Production that can be controlled: thermal and hydrostorage",
         pkgEnv$generation)
setAlias("renewable", "Renewable productions", pkgEnv$ren)
setAlias("thermal", "Thermal productions", pkgEnv$thermal)
setAlias("netLoad", "Variables used to compute net load", 
         c("areas", "LOAD", "ROW BAL.", "PSP", "MISC. NDG", "H. ROR", 
           "WIND", "SOLAR", "mustRun"))
setAlias("nostat", "All variables except summary variable (MIN, MAX and STD)",
         c("OV. COST", "OP. COST", 
           "MRG. PRICE", "CO2 EMIS.", "BALANCE", "ROW BAL.", "PSP", "MISC. NDG",
           "LOAD", "H. ROR", "WIND", "SOLAR", "NUCLEAR", "LIGNITE", "COAL",
           "GAS", "OIL", "MIX. FUEL", "MISC. DTG", "H. STOR", "UNSP. ENRG",
           "SPIL. ENRG", "LOLD", "LOLP", "AVL DTG", "DTG MRG", "MAX MRG",
           "NP COST", "NODU", "FLOW LIN.", "UCAP LIN.",
           "FLOW QUAD.", "CONG. FEE (ALG.)", "CONG. FEE (ABS.)", "MARG. COST",
           "CONG. PROB +", "CONG. PROB -", "HURDLE COST"))

# The goal of the following lines is only to remove many useless warnings in 
# R CMD CHECK: "no visible binding for global variable 'XXX'".
# They come from the use of the data.table syntax.
utils::globalVariables(
  c("timeId", "area", "hydroStorage", "thermalAvailability",
    "cluster", "FLOW LIN.", "direction", "flow",
    "BALANCE", "totalFlow", "prop", "to", "link", "change",
    "district", "must.run", ".txt", "detailsLength",
    "linkLength", "connectedToVirtualArea", "from", "correction",
    "nominalcapacity", "unitcount", "capacity", "minGenModulation",
    "production", "mustRunPartial", "mustRunTotal", "mcYear",
    "J", "N", "PSP", "availableUnits", "color", "corrPSP", "fromDistrict", 
    "pumpingCapacity", "pumpingCapacity.x", "pumpingCapacity.y", "rarea", 
    "storageCapacity", "storageCapacity.x", "storageCapacity.y", "toDistrict", 
    "transCapacityDirect", "transCapacityIndirect", "varea", "x", "y",
    "NODU", "min.stable.power", "thermalPmin", "name", "value")
)

#-----------------------------  HDF5 ------------------------------------#


is.installed <- function(mypkg) is.element(mypkg, utils::installed.packages()[,1])

rhdf5_version <- "2.24.0"
rhdf5_message <- "This function require 'rhdf5' (>= 2.24.0) package.
         This is a bioconductor package. You can install it with :
         source('https://bioconductor.org/biocLite.R')
         biocLite('rhdf5')"

# !! parameter versionCheck of requireNamespace does not work correctly, use utils::package_version instead
.requireRhdf5_Antares<-function(stopP = TRUE){
  if(.check_rhdf5(stopP = stopP)){
    if(.check_rhdf5_version(stopP = stopP)){
      return(TRUE)
    }
  }
  
  return(FALSE)
}

.stop_rhdf5_version <- function(stopP = TRUE) {
  if(stopP){
    stop(rhdf5_message)
  }else{
    return(FALSE)
  }
}

.check_rhdf5 <- function(stopP = TRUE){
  if(requireNamespace("rhdf5")){
    return(TRUE)
  }else{
    .stop_rhdf5_version(stopP)
  }
}

.check_rhdf5_version <- function(stopP = TRUE){
  if(utils::packageVersion("rhdf5") >= rhdf5_version){
    return(TRUE)
  }else{
    .stop_rhdf5_version(stopP)
  }
}



# .addClassAndAttributes <- antaresRead:::.addClassAndAttributes

pkgEnvAntareasH5 <- new.env()

pkgEnvAntareasH5$varAreas <- c("OV. COST", "OP. COST", "MRG. PRICE", "CO2 EMIS.", "BALANCE",
                               "ROW BAL.", "PSP", "MISC. NDG",  "LOAD", "H. ROR", "WIND", "SOLAR",
                               "NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL", "MISC. DTG", "H. STOR",
                               "UNSP. ENRG", "SPIL. ENRG", "LOLD", "LOLP", "AVL DTG", "DTG MRG", "MAX MRG", "NP COST", "NODU")

pkgEnvAntareasH5$varAreas <- as.vector(sapply(pkgEnvAntareasH5$varAreas, function(X){paste0(X, c("", "_min", "_max", "_std"))}))

pkgEnvAntareasH5$varDistricts <- pkgEnvAntareasH5$varAreas

pkgEnvAntareasH5$varLinks <- c("FLOW LIN.", "UCAP LIN.", "FLOW QUAD.",
                               "CONG. FEE (ALG.)", "CONG. FEE (ABS.)",
                               "MARG. COST", "CONG. PROB +", "CONG. PROB -", "HURDLE COST")

pkgEnvAntareasH5$varLinks <- as.vector(sapply(pkgEnvAntareasH5$varLinks, function(X){paste0(X, c("", "_min", "_max", "_std"))}))

pkgEnvAntareasH5$varClusters <- c("production", "NP Cost", "NODU")

pkgEnvAntareasH5$varAliasCreated <- list()


#misc
pkgEnvAntareasH5$varAliasCreated$misc$areas <- c("CHP",
                                                 "Bio_mass",
                                                 "Bio_gas",
                                                 "mustRunWasteTotal",
                                                 "GeoThermal",
                                                 "Other",
                                                 "PSP_input",
                                                 "ROW_Balance")

pkgEnvAntareasH5$varAliasCreated$misc$districts <- c("CHP",
                                                     "Bio_mass",
                                                     "Bio_gas",
                                                     "mustRunWasteTotal",
                                                     "GeoThermal",
                                                     "Other",
                                                     "PSP_input",
                                                     "ROW_Balance")
#thermalAvailabilities
pkgEnvAntareasH5$varAliasCreated$thermalAvailabilities$clusters <- c("thermalAvailability",
                                                                     "availableUnits")


#hydroStorage
pkgEnvAntareasH5$varAliasCreated$hydroStorage$areas <- c("hydroStorage")

pkgEnvAntareasH5$varAliasCreated$hydroStorage$districts <- c("hydroStorage")

#hydroStorageMaxPower
if(opts$antaresVersion >= 650){
  pkgEnvAntareasH5$varAliasCreated$hydroStorageMaxPower$areas <- c("generatingMaxPower", 
                                                                   "generatingMaxEnergy", 
                                                                   "pumpingMaxPower", 
                                                                   "pumpingMaxEnergy")
}else{
  pkgEnvAntareasH5$varAliasCreated$hydroStorageMaxPower$areas <- c("hstorPMaxLow",
                                                                   "hstorPMaxAvg",
                                                                   "hstorPMaxHigh")
}

#hydroStorageMaxPower
if(opts$antaresVersion >= 650){
  pkgEnvAntareasH5$varAliasCreated$hydroStorageMaxPower$districts <- c("generatingMaxPower", 
                                                                   "generatingMaxEnergy", 
                                                                   "pumpingMaxPower", 
                                                                   "pumpingMaxEnergy")
}else{
  pkgEnvAntareasH5$varAliasCreated$hydroStorageMaxPower$districts <- c("hstorPMaxLow",
                                                                   "hstorPMaxAvg",
                                                                   "hstorPMaxHigh")
}

#reserve
pkgEnvAntareasH5$varAliasCreated$reserve$areas <- c("primaryRes",
                                                    "strategicRes",
                                                    "DSM",
                                                    "dayAhead")

pkgEnvAntareasH5$varAliasCreated$reserve$districts <- c("primaryRes",
                                                        "strategicRes",
                                                        "DSM",
                                                        "dayAhead")

#linkCapacity
pkgEnvAntareasH5$varAliasCreated$linkCapacity$links <- c("transCapacityDirect",
                                                         "transCapacityIndirect",
                                                         "impedances",
                                                         "hurdlesCostDirect",
                                                         "hurdlesCostIndirect")

#mustRun
pkgEnvAntareasH5$varAliasCreated$mustRun$areas <- c("thermalPmin",
                                                    "mustRun",
                                                    "mustRunPartial",
                                                    "mustRunTotal")

pkgEnvAntareasH5$varAliasCreated$mustRun$districts <- c("thermalPmin",
                                                        "mustRun",
                                                        "mustRunPartial",
                                                        "mustRunTotal")

pkgEnvAntareasH5$varAliasCreated$mustRun$clusters <- c("thermalPmin",
                                                       "mustRun",
                                                       "mustRunPartial",
                                                       "mustRunTotal")

pkgEnvAntareasH5$varAliasCreated$thermalModulation$clusters <- c("marginalCostModulation",
                                                                 "marketBidModulation",
                                                                 "capacityModulation",
                                                                 "minGenModulation")

integerVariable <- as.character(unique(pkgEnv$formatName$Name[which(pkgEnv$formatName$digits == 0)]))
integerVariable <- unlist(apply(expand.grid(integerVariable, c("", "_std", "_min", "_max")), 1,
                                function(X){paste0(X, collapse = "")}))


.tidymess <- function(..., prefix = " ", initial = ""){
  as.character(strwrap(..., prefix = prefix, initial = initial))
}

.get_by <- function(x = NULL){
  if (attr(x, "synthesis")) {
    by <- c("timeId")
  } else {
    by <- c("mcYear", "timeId")
  }
  by
}

.get_by_area <- function(x = NULL){
  # Aliases used for aggregation
  byarea <- c("area", .get_by(x))
  return(byarea)
}

.get_by_link <- function(x = NULL){
  bylink <- c("link", .get_by(x))
  return(bylink)
}

.get_by_district <- function(x = NULL){
  bydistrict <- c("district", .get_by(x))
  return(bydistrict)
}


.check_x <- function(x = NULL){
  if ("list" %in% class(x)){
    for (elementI in x){
      .check_x(elementI)
    }
  }else{
    if (!(.isSimOpts(x) | .isAntaresData(x))){
      stop(paste0(substitute(x), 
                  " should be an object of class 'antaresData' (or 'simOptions') created with 'readAntares()' (or 'setSimulationPath()')"))
    }else{
      return(TRUE)
    } 
  }
}

.check_x_simOptions <- function(x = NULL){
  if (!.isSimOpts(x)){
    stop("'x' should be an object of class 'simOptions' created with 'setSimulationPath()'")
  }else {
    return(TRUE)
  }
}

#' Test opst
#' 
#' @param test if x is simOptions class
#' 
#' @noRd
.isSimOpts <- function(x){
  if ("simOptions" %in% class(x)){
    if (!is.null(x$h5path)){
      if (!file.exists(x$h5path)){
        warning(paste0("h5file does not exists for this study :",
                       x$studyName))
        return(FALSE)
      }else{
        return(TRUE)
      }
    }else{
      #opts but no h5 (TXT)
      return(TRUE)
    }
  }else{
    return(FALSE)
  }
}

#' Test lits opst
#' 
#' @param test if x is list of simOptions class
#' 
#' @noRd
.isListSimOpts <- function(x){
  if ("list" %in% class(x)){
    if (length(x) > 0){
      if (.isSimOpts(x[[1]])){
        return(TRUE)
      }else{
        return(FALSE)
      }
    }else{
      return(FALSE)
    }
  }else{
    return(FALSE)
  }
}

#' Test antaresData
#' 
#' @param x if x is antaresData class
#' 
#' @noRd
.isAntaresData <- function(x){
  "antaresData" %in% class(x)
}

.check_x_antaresData <- function(x = NULL){
  if (!.isAntaresData(x)){
    stop("'x' should be an object of class 'antaresData created with readAntares()' or an opts")
  }else {
    return(TRUE)
  }
}



#' edit h5 file for TEST
#' currently for all data except clusters
#'
#' @param pathH5 path H5 file
#' @param instanceData character name of one instance 
#' @param classData character name of class instance 
#' @param timeId timeId to change
#' @param antVar antares Variable to change
#' @param newValue the newValue
#'
#' @noRd
.h5Antares_edit_variable <- function(pathH5 = NULL,
                                     instanceData = NULL,
                                     classData = NULL,
                                     timeId = 1,
                                     antVar = NULL,
                                     newValue = NULL,
                                     mcYear = NULL,
                                     allTimeId = FALSE,
                                     timeStep = "hourly"){
  
  if (is.null(instanceData) | is.null(classData)){
    stop("instanceData and classData must be set together")
  }
  
  if (classData=="areas"){
    classDataS <- "area"
  }else if(classData=="links"){
    classDataS <- "link"
  }else{
    classDataS <- "district"
  }
  
  if (is.null(mcYear)){
    typeOfData <- "/mcAll"
  }else{
    typeOfData <- "/mcInd"
  }
  timeStepType <- paste0("/",paste(timeStep, classData, sep = "/"))
  typeOfDataTime <- paste0(timeStepType, typeOfData)
  nameStructure <- paste0(typeOfDataTime, "/structure")
  
  H5locAntaresh5 <- rhdf5::H5Fopen(name = pathH5)
  resStruc <- rhdf5::h5ls(H5locAntaresh5)
  dimData <- resStruc[ resStruc$group == typeOfDataTime & resStruc$name == "data", ]$dim
  hourlyDataStructure <- rhdf5::h5read(H5locAntaresh5, name = nameStructure)
  
  indexCateroryInstance <- grep(instanceData, hourlyDataStructure[[classDataS]])[1]
  
  indexAntVar <- grep(antVar, hourlyDataStructure$variable)[1]
  if(is.na(indexAntVar)){
    indexAntVar <- grep(antVar, hourlyDataStructure$reCalcVar)[1]
    if(is.na(indexAntVar)){
      stop("error index")
    }else{
      indexAntVar <- indexAntVar + length(hourlyDataStructure$variable)
    }
  }
  if(allTimeId){
    maxTimeId <- as.integer(strsplit(dimData, "x")[[1]][1])
    indexTimeId <- 1:maxTimeId
  }else{
    indexTimeId <- timeId
  }
  
  if (is.null(mcYear)){
    indexMcYear <- 1
  }else{
    indexMcYear <- grep(mcYear, hourlyDataStructure$mcYear)[1]
  }
  
  listIndex <- list(indexTimeId, indexAntVar, indexCateroryInstance, indexMcYear)
  #debug print(listIndex)
  
  hourlyData <- rhdf5::h5read(
    H5locAntaresh5,
    name = paste0(typeOfDataTime, "/data"),
    index = listIndex)
  
  hourlyData[,,,] <- newValue
  
  rhdf5::h5writeDataset.array(
    obj = hourlyData,
    h5loc = H5locAntaresh5,
    name = paste0(typeOfDataTime, "/data"),
    index = listIndex
  )
  
  rhdf5::H5Fclose(h5file = H5locAntaresh5)
  rhdf5::h5closeAll()
}
