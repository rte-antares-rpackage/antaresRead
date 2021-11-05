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
pkgEnv$ren <- c("WIND", "WIND OFFSHORE", "WIND ONSHORE", 
                "SOLAR", "SOLAR CONCRT.", "SOLAR PV", "SOLAR ROOFT",
                "RENW. 1", "RENW. 2", "RENW. 3", "RENW. 4",
                "H. ROR", "H. STOR", 
                "MISC. DTG", "MISC. DTG 2", "MISC. DTG 3", "MISC. DTG 4")

# Column names for thermal
pkgEnv$thermal <- c("NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL", 
                    "MISC. DTG", "MISC. DTG 2", "MISC. DTG 3", "MISC. DTG 4")

# Column names for generation
pkgEnv$generation <- c("NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL", "H. STOR",
                       "MISC. DTG", "MISC. DTG 2", "MISC. DTG 3", "MISC. DTG 4")

pkgEnv$production <- c("NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL",
                       "MISC. DTG", "MISC. DTG 2", "MISC. DTG 3", "MISC. DTG 4", 
                       "H. STOR", "H. ROR", "WIND", "WIND OFFSHORE", "WIND ONSHORE", 
                       "RENW. 1", "RENW. 2", "RENW. 3", "RENW. 4",
                       "SOLAR", "SOLAR CONCRT.", "SOLAR PV", "SOLAR ROOFT", "MISC. NDG", "PSP")

# list of column names that are id variables in output files. When they are
# present in an output file, they are automatically imported, whatever the value
# of "select" is.
pkgEnv$idVars <- c("area", "district", "sector", "cluster", "link", "mcYear", "timeId", "time", "day", "week", "month", "hour")

pkgEnv$idTimeVars <- c("timeId", "time", "day", "week", "month", "hour")

setAlias("economy", "Production costs, prices, exchanges and spilled energy",
         c("OV. COST", "OP. COST", "MRG. PRICE", "CO2 EMIS.", "BALANCE", "SPIL. ENRG"))
setAlias("adequacy", "Adequacy variables",
         c("UNSP. ENRG", "LOLD", "LOLP", "AVL DTG", "DTG MRG", "MAX MRG"))
setAlias("generation", "Production that can be controlled: thermal and hydrostorage",
         pkgEnv$generation)
setAlias("renewable", "Renewable productions", pkgEnv$ren)
setAlias("thermal", "Thermal productions", pkgEnv$thermal)
setAlias("netLoad", "Variables used to compute net load", 
         c("areas", "LOAD", "ROW BAL.", "PSP", "H. ROR", 
           "MISC. DTG", "MISC. DTG 2", "MISC. DTG 3", "MISC. DTG 4",
           "WIND", "WIND OFFSHORE", "WIND ONSHORE", 
           "SOLAR", "SOLAR CONCRT.", "SOLAR PV", "SOLAR ROOFT",
           "RENW. 1", "RENW. 2", "RENW. 3", "RENW. 4",
           "mustRun"))

setAlias("rmVA_production", "removeVirtualAreas production varaibles", 
         setdiff(unique(c(getAlias("generation"), getAlias("netLoad"), "SPIL. ENRG")), c("LOAD", "areas", "mustRun"))
)
         
setAlias("nostat", "All variables except summary variable (MIN, MAX and STD)",
         c("OV. COST", "OP. COST", 
           "MRG. PRICE", "CO2 EMIS.", "BALANCE", "ROW BAL.", "PSP", "MISC. NDG",
           "LOAD", "H. ROR", "WIND", "WIND OFFSHORE", "WIND ONSHORE", 
           "SOLAR", "SOLAR CONCRT.", "SOLAR PV", "SOLAR ROOFT", 
           "NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL", 
           "MISC. DTG", "MISC. DTG 2", "MISC. DTG 3", "MISC. DTG 4",
           "H. STOR", "UNSP. ENRG", "RENW. 1", "RENW. 2", "RENW. 3", "RENW. 4",
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
.requireRhdf5_Antares <- function(stopP = TRUE){
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
  if(requireNamespace("rhdf5", quietly = TRUE)){
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
                               "NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL", "MISC. DTG", 
                               "H. STOR", "UNSP. ENRG", "SPIL. ENRG", "LOLD", "LOLP", "AVL DTG", 
                               "DTG MRG", "MAX MRG", "NP COST", "NODU")

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
pkgEnvAntareasH5$varAliasCreated$hydroStorageMaxPower$areas <- c("hstorPMaxLow",
                                                                 "hstorPMaxAvg",
                                                                 "hstorPMaxHigh")

pkgEnvAntareasH5$varAliasCreated$hydroStorageMaxPower$districts <- c("hstorPMaxLow",
                                                                     "hstorPMaxAvg",
                                                                     "hstorPMaxHigh")

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
