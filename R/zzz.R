#Copyright © 2016 RTE Réseau de transport d’électricité

#' @import data.table
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

rhdf5_version <- "2.20.0"
rhdf5_message <- "This function require 'rhdf5' (>= 2.20.0) package.
         This is a bioconductor package. You can install it with :
         source('https://bioconductor.org/biocLite.R')
         biocLite('rhdf5')"



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
