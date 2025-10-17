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


# private variables ----
# Private variables accessible only by functions from the package

pkgEnv <- new.env()


## output variables ----
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
pkgEnv$idVars <- c("area", "district", "sector", "cluster", "link", "bindingConstraint", "mcYear", "timeId", "time", "day", "week", "month", "hour")

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

## global vars package ----
# The goal of the following lines is only to remove many useless warnings in
# R CMD CHECK: "no visible binding for global variable 'XXX'".
# They come from the use of the data.table syntax.
utils::globalVariables(
  c("timeId", "tsId", "area", "hydroStorage", "thermalAvailability",
    "cluster", "FLOW LIN.", "FLOW QUAD.", "direction", "flow",
    "BALANCE", "totalFlow", "prop", "to", "link", "change",
    "district", "must.run", ".txt", "detailsLength",
    "linkLength", "connectedToVirtualArea", "from", "correction",
    "nominalcapacity", "unitcount", "capacity", "minGenModulation",
    "production", "mustRunPartial", "mustRunTotal", "mcYear",
    "J", "N", "PSP", "availableUnits", "color", "corrPSP", "fromDistrict",
    "pumpingCapacity", "pumpingCapacity.x", "pumpingCapacity.y", "rarea",
    "storageCapacity", "storageCapacity.x", "storageCapacity.y", "toDistrict",
    "transCapacityDirect", "transCapacityIndirect", "varea", "x", "y",
    "NODU", "min.stable.power", 'Category', 'Version.Antares', 'Type',
    "thermalPmin", "name", "value", "Tech.Name", '..format_field',
    "Folder", "Mode", "Stats", "Name", "progNam", "mrgprice", "isLOLD_cum",
    "...To", "upstream", "downstream", "LOLD", "LOLD_data", "LOLP", "warn_for_status",
    "MRG. PRICE", "H. LEV", "V2", "V1", "size", "ORDINAL_POSITION_BY_TOPIC",
    "DETAILS_FILES_TYPE","ANTARES_DISPLAYED_NAME","MIN_VERSION")
)

## thematic ----
ref_thematic_880 <- read.table(file = system.file("variables_selection/ref_thematic_by_version/ref_880.csv",
                                                 package = "antaresRead"),
                              header = TRUE,
                              sep = ",")

ref_thematic_880$col_name <- trimws(ref_thematic_880$col_name)

ref_thematic_920 <- read.table(system.file("variables_selection/ref_thematic_by_version/ref_920.csv",
                                           package = "antaresRead"),
                               header = TRUE,
                               sep = ",")

ref_thematic_920$col_name <- trimws(ref_thematic_920$col_name)

ref_thematic_930 <- read.table(system.file("variables_selection/ref_thematic_by_version/ref_930.csv",
                                           package = "antaresRead"),
                               header = TRUE,
                               sep = ",")

ref_thematic_930$col_name <- trimws(ref_thematic_930$col_name)

ref_thematic <- list("880" = ref_thematic_880,
                     "920" = ref_thematic_920,
                    "930" = ref_thematic_930)

pkgEnv$thematic <- ref_thematic

### api ----
ref_thematic_api <- read.table(file = system.file("variables_selection/ref_thematic_by_version/api_ref_conversion.csv",
                                                  package = "antaresRead"),
                               header = TRUE,
                               sep = ",")

pkgEnv$thematic_api <- ref_thematic_api


## INPUT Properties REF ----
cluster_properties <- data.table::fread(system.file("referential_properties/cluster_properties.csv",
                                              package = "antaresRead"),
                                  sep = ";",
                                  header = TRUE)

# append
pkgEnv$inputProperties <- cluster_properties

integerVariable <- as.character(unique(pkgEnv$formatName$Name[which(pkgEnv$formatName$digits == 0)]))
integerVariable <- unlist(apply(expand.grid(integerVariable, c("", "_std", "_min", "_max")), 1,
                                function(X){paste0(X, collapse = "")}))

# some tools functions ----
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

