# Private variables accessible only by functions from the package

pkgEnv <- new.env()

pkgEnv$idVars <- c("node", "link", "timeId", "day", "week", "month", "hour", "mcYear")


pkgEnv$varAliases <- list(
  economy = c("OV. COST", "OP. COST", "MRG. PRICE", "CO2 EMIS.", "BALANCE", "SPIL. ENRG"),
  adequacy = c("UNSP. ENRG", "LOLD", "LOLP", "AVL DTG", "DTG MRG", "MAX MRG"),
  "net load" = c("ROW BAL.", "PSP", "MISC. NDG", "LOAD", "H. ROR", "WIND", "SOLAR"),
  generation = c("NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL", "MISC. DTG", "H. STOR")
)
