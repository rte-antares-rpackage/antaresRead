#' @import data.table
#' @import plyr

# Private variables accessible only by functions from the package

pkgEnv <- new.env()

# list of column names that are id variables in output files. When they are
# present in an output file, they are automatically imported, whatever the value
# of "select" is.
pkgEnv$idVars <- c("area", "district", "cluster", "link", "timeId", "time", "day", "week", "month", "hour", "mcYear")

# Aliases used by the argument "select" of function readOutput.
pkgEnv$varAliases <- list(
  economy = c("OV. COST", "OP. COST", "MRG. PRICE", "CO2 EMIS.", "BALANCE", "SPIL. ENRG"),
  adequacy = c("UNSP. ENRG", "LOLD", "LOLP", "AVL DTG", "DTG MRG", "MAX MRG"),
  "net load" = c("ROW BAL.", "PSP", "MISC. NDG", "LOAD", "H. ROR", "WIND", "SOLAR"),
  generation = c("NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL", "MISC. DTG", "H. STOR"),
  nostat = c("OV. COST", "OP. COST", 
            "MRG. PRICE", "CO2 EMIS.", "BALANCE", "ROW BAL.", "PSP", "MISC. NDG", 
            "LOAD", "H. ROR", "WIND", "SOLAR", "NUCLEAR", "LIGNITE", "COAL", 
            "GAS", "OIL", "MIX. FUEL", "MISC. DTG", "H. STOR", "UNSP. ENRG", 
            "SPIL. ENRG", "LOLD", "LOLP", "AVL DTG", "DTG MRG", "MAX MRG", 
            "NP COST", "NODU", "FLOW LIN.", "UCAP LIN.", 
            "FLOW QUAD.", "CONG. FEE (ALG.)", "CONG. FEE (ABS.)", "MARG. COST", 
            "CONG. PROB +", "CONG. PROB -", "HURDLE COST")
)

# Column names in the misc input files.
pkgEnv$miscNames <- c("CHP", "Bio_mass", "Bio_gas", "Waste", "GeoThermal", "Other", "PSP", "ROW_Balance")
