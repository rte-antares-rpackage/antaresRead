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





pkgEnvAntareasH5$varAliasCraeted <- list()


#misc
pkgEnvAntareasH5$varAliasCraeted$misc$areas <- c("CHP",
                                                 "Bio_mass",
                                                 "Bio_gas",
                                                 "mustRunWasteTotal",
                                                 "GeoThermal",
                                                 "Other",
                                                 "PSP_input",
                                                 "ROW_Balance")
pkgEnvAntareasH5$varAliasCraeted$misc$districts <- c("CHP",
                                                     "Bio_mass",
                                                     "Bio_gas",
                                                     "mustRunWasteTotal",
                                                     "GeoThermal",
                                                     "Other",
                                                     "PSP_input",
                                                     "ROW_Balance")
#thermalAvailabilities
pkgEnvAntareasH5$varAliasCraeted$thermalAvailabilities$clusters <- c("thermalAvailability",
                                                                     "availableUnits")


#hydroStorage
pkgEnvAntareasH5$varAliasCraeted$hydroStorage$areas <- c("hydroStorage")
pkgEnvAntareasH5$varAliasCraeted$hydroStorage$districts <- c("hydroStorage")



#hydroStorageMaxPower
pkgEnvAntareasH5$varAliasCraeted$hydroStorageMaxPower$areas <- c("hstorPMaxLow",
                                                                 "hstorPMaxAvg",
                                                                 "hstorPMaxHigh")

pkgEnvAntareasH5$varAliasCraeted$hydroStorageMaxPower$districts <- c("hstorPMaxLow",
                                                                     "hstorPMaxAvg",
                                                                     "hstorPMaxHigh")

#reserve
pkgEnvAntareasH5$varAliasCraeted$reserve$areas <- c("primaryRes",
                                                    "strategicRes",
                                                    "DSM",
                                                    "dayAhead")

pkgEnvAntareasH5$varAliasCraeted$reserve$districts <- c("primaryRes",
                                                        "strategicRes",
                                                        "DSM",
                                                        "dayAhead")

#linkCapacity
pkgEnvAntareasH5$varAliasCraeted$linkCapacity$links <- c("transCapacityDirect",
                                                         "transCapacityIndirect",
                                                         "impedances",
                                                         "hurdlesCostDirect",
                                                         "hurdlesCostIndirect")

#mustRun
pkgEnvAntareasH5$varAliasCraeted$mustRun$areas <- c("thermalPmin",
                                                    "mustRun",
                                                    "mustRunPartial",
                                                    "mustRunTotal")

pkgEnvAntareasH5$varAliasCraeted$mustRun$districts <- c("thermalPmin",
                                                        "mustRun",
                                                        "mustRunPartial",
                                                        "mustRunTotal")

pkgEnvAntareasH5$varAliasCraeted$mustRun$clusters <- c("thermalPmin",
                                                       "mustRun",
                                                       "mustRunPartial",
                                                       "mustRunTotal")

pkgEnvAntareasH5$varAliasCraeted$thermalModulation$clusters <- c("marginalCostModulation",
                                                                 "marketBidModulation",
                                                                 "capacityModulation",
                                                                 "minGenModulation")


###Process

pkgEnvAntareasH5$process$addDownwardMargin$areas <- c("isolatedDownwardMargin",
                                                      "interconnectedDownwardMargin")

pkgEnvAntareasH5$process$addUpwardMargin$areas <- c("isolatedUpwardMargin",
                                                    "interconnectedUpwardMargin")

pkgEnvAntareasH5$process$addExportAndImport$areas <- c("import",
                                                       "export")

pkgEnvAntareasH5$process$addLoadFactorLink$links <- c("loadFactor",
                                                      "congestion")

pkgEnvAntareasH5$process$externalDependency$areas <- c("netLoad",
                                                       "exportsLevel",
                                                       "importsLevel",
                                                       "exportsFrequency",
                                                       "importsFrequency")

pkgEnvAntareasH5$process$loadFactor$clusters <- c("loadFactor", "propHoursMinGen", "propHoursMaxGen")

pkgEnvAntareasH5$process$modulation$clusters <- c("upwardModulation", "downwardModulation",
                                                  "absoluteModulation")

pkgEnvAntareasH5$process$netLoadRamp$areas <- c("netLoadRamp", "balanceRamp", "areaRamp")

pkgEnvAntareasH5$process$surplus$areas <- c("consumerSurplus", "producerSurplus", "rowBalanceSurplus",
                                            "storageSurplus", "congestionFees", "globalSurplus")

pkgEnvAntareasH5$process$surplusClusters$clusters <- c("variableCost", "fixedCost", "startupCost",
                                                       "surplusPerUnit", "totalSurplus", "economicGradient")


sapply(names(pkgEnvAntareasH5$process), function(X){
  tpAlias <- pkgEnvAntareasH5$process[[X]]
  X <- paste0("Out_", X)
  sapply(names(tpAlias), function(Y){
    varAlias <- tpAlias[[Y]]
    setAlias(X, X, c(Y, varAlias))
  })
})


#pkgEnvAntareasH5$process$surplusSectors$areas <- c("surplus", "cost")

pkgEnvAntareasH5$processDispo <- data.frame(
  trtName = c("downwardMargin",
              "upwardMargin",
              "exportsImports",
              "loadFactorLink",
              "externalDependency",
              "loadFactor",
              "modulation",
              "netLoadRamp",
              "surplus",
              "surplusClusters"
  )
  , fctname = c( "addDownwardMargin" ,
                 "addUpwardMargin" ,
                 "addExportAndImport" ,
                 "addLoadFactorLink" ,
                 "externalDependency" ,
                 "loadFactor" ,
                 "modulation" ,
                 "netLoadRamp" ,
                 "surplus" ,
                 "surplusClusters"
  ))

