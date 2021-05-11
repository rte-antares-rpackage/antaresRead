# context("setSimulationPathAPI")
# suppressWarnings(suppressPackageStartupMessages(require(lubridate)))
# 
# describe("setSimulationPathAPI", {
#   
#   ##Test identical OPTS
#   testIdenticalOpts <- function(opts, opts2){
#     
#     normal_not_same <- c("studyPath", "simPath", "inputPath", "simOutputName" , "simDataPath", "parameters", "typeLoad")
#     all(sapply(names(opts)[!names(opts) %in% normal_not_same], function(X){
#       if(is.character(opts[[X]])){
#         identical(tolower(opts[[X]]), tolower(opts2[[X]]))
#       }else{
#         identical(opts[[X]], opts2[[X]])
#       }
#     }))
#     
#     all(sapply(names(opts$parameters), function(X){
#       identical(opts$parameters[[X]][order(names(opts$parameters[[X]]))], opts2$parameters[[X]][order(names(opts2$parameters[[X]]))])
#     }))
#     
#   }
#   
#   path = "http://localhost:8080/studies/antaresStd/"
#   path2 <- "C:/Users/TitouanRobert/Desktop/antaresStd"
#   
#   opts <- setSimulationPathAPI(path, 1)
#   opts2 <- setSimulationPath("C:/Users/TitouanRobert/Desktop/antaresStd", 1)
# 
#   testIdenticalOpts(opts, opts2)
#   
#   
#   
#   
#   opts <- setSimulationPathAPI(path, "input")
#   opts2 <- setSimulationPath("C:/Users/TitouanRobert/Desktop/antaresStd", "input")
#   
#   testIdenticalOpts(opts, opts2)
#   
# })
