#' @import data.table
#' @import plyr

# Private variables accessible only by functions from the package

pkgEnv <- new.env()

# list of column names that are id variables in output files. When they are
# present in an output file, they are automatically imported, whatever the value
# of "select" is.
pkgEnv$idVars <- c("node", "link", "timeId", "day", "week", "month", "hour", "mcYear", "cluster")

# Aliases used by the argument "select" of function readOutput.
pkgEnv$varAliases <- list(
  economy = c("OV. COST", "OP. COST", "MRG. PRICE", "CO2 EMIS.", "BALANCE", "SPIL. ENRG"),
  adequacy = c("UNSP. ENRG", "LOLD", "LOLP", "AVL DTG", "DTG MRG", "MAX MRG"),
  "net load" = c("ROW BAL.", "PSP", "MISC. NDG", "LOAD", "H. ROR", "WIND", "SOLAR"),
  generation = c("NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL", "MISC. DTG", "H. STOR")
)

# Column names in the misc input files.
pkgEnv$miscNames <- c("CHP", "Bio_mass", "Bio_gas", "Waste", "GeoThermal", "Other", "PSP", "ROW_Balance")
