#Copyright © 2016 RTE Réseau de transport d’électricité

#' @import data.table
#' @import plyr
#' @importFrom methods is
#' @importFrom grDevices col2rgb rgb
#' @importFrom stats as.formula
#' @importFrom utils View read.table type.convert write.table 
#' @importFrom utils untar
#' @importFrom stringr str_match str_replace

# Private variables accessible only by functions from the package

pkgEnv <- new.env()


pkgEnv$formatName <- read.table(system.file("format_output/tableOutput.csv", package
                                            = "antaresRead"), sep = ";", header = TRUE)

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

