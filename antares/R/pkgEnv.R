# Private variables accessible only by functions from the package

pkgEnv <- new.env()

pkgEnv$idVars <- c("node", "link", "timeId", "day", "week", "month", "hour", "mcYear")
