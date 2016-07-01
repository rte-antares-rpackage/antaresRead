#' @import data.table
#' @import plyr

# Private variables accessible only by functions from the package

pkgEnv <- new.env()

# list of column names that are id variables in output files. When they are
# present in an output file, they are automatically imported, whatever the value
# of "select" is.
pkgEnv$idVars <- c("area", "district", "sector", "cluster", "link", "timeId", "time", "day", "week", "month", "hour", "mcYear")

# Aliases used by the argument "select" of function readOutput.
pkgEnv$varAliases <- list(
  economy = c("OV. COST", "OP. COST", "MRG. PRICE", "CO2 EMIS.", "BALANCE", "SPIL. ENRG"),
  adequacy = c("UNSP. ENRG", "LOLD", "LOLP", "AVL DTG", "DTG MRG", "MAX MRG"),
  "net load" = c("ROW BAL.", "PSP", "MISC. NDG", "LOAD", "H. ROR", "WIND", "SOLAR"),
  generation = c("NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL", "MISC. DTG", "H. STOR"),
  ren = c("WIND", "SOLAR", "H. ROR", "H. STOR"),
  thermal = c("NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL", "MISC. DTG"),
  production = c("NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL",
                "MISC. DTG", "H. STOR", "H. ROR", "WIND", "SOLAR"),
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
pkgEnv$miscNames <- c("CHP", "Bio_mass", "Bio_gas", "Waste", "GeoThermal", "Other", "PSP_input", "ROW_Balance")

# Column names for ren
pkgEnv$ren <- c("WIND", "SOLAR", "H. ROR", "H. STOR")

# Column names for thermal
pkgEnv$thermal <- c("NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL", "MISC. DTG")

# Column names for generation
pkgEnv$generation <- c("NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL", "MISC. DTG", "H. STOR")

# Column names for production
pkgEnv$production <- c("NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL",
              "MISC. DTG", "H. STOR", "H. ROR", "WIND", "SOLAR")

# The goal of the following lines is only to remove many useless warnings in 
# R CMD CHECK: "no visible binding for global variable 'XXX'".
# They come from the use of the data.table syntax.
utils::globalVariables(c("timeId", "area", "hydroStorage", "thermalAvailability",
                         "cluster", "FLOW LIN.", "direction", "flow",
                         "BALANCE", "totalFlow", "prop", "to", "link", "change",
                         "district", "must.run", ".txt", "detailsLength",
                         "linkLength", "connectedToVirtualArea", "from", "correction",
                         "nominalcapacity", "unitcount", "capacity", "mustRunModulation",
                         "production", "mustRunPartial", "mustRunTotal", "mcYear"))
