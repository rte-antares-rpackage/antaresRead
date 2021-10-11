library(antaresRead)

# ./server -c config.yaml --auto-upgrade-db

# ENR
host <- "http://pyre.datastorm.fr:8080"
study_id <- "178c8e7b-46fa-439e-b44d-0e821787781f"
token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ7XCJpZFwiOiAyLCBcImltcGVyc29uYXRvclwiOiAxLCBcInR5cGVcIjogXCJib3RzXCIsIFwiZ3JvdXBzXCI6IFtdfSIsImlhdCI6MTYzMzAxMjkzMywibmJmIjoxNjMzMDEyOTMzLCJqdGkiOiJiMDliZThlNS0zZmYwLTRjZjctOWYzYS1kY2NhMzAwYTViYTQiLCJleHAiOjc5OTIwNTI5MzMsInR5cGUiOiJhY2Nlc3MiLCJmcmVzaCI6ZmFsc2V9.NxMxnD_BYDh_vF2aMA17XN6VljE2Glegs31qvH_7_-A"
simulation = 1

opts_api <- setSimulationPathAPI(host = host, 
                                 study_id = study_id, 
                                 token = token, 
                                 simulation = 2)

opts_api$parameters$`other preferences`$`renewable-generation-modelling`
opts_api$parameters$`variables selection`

opts_local <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\ENR_Cluster\\test_study_no_output/", 2)
opts_local$parameters$`variables selection`
all.equal(opts_api$parameters, opts_local$parameters)

# layout
ly_local <- antaresRead::readLayout(opts = opts_local)
ly_api <- antaresRead::readLayout(opts = opts_api)
all.equal(ly_api, ly_local)

# clusterDesc
# cd_local <- antaresRead::readClusterDesc(opts = opts_local)
# cd_api <- antaresRead::readClusterDesc(opts = opts_api)
# all.equal(cd_api, cd_local[, colnames(cd_api), with = FALSE])

cd_local <- antaresRead::readClusterResDesc(opts = opts_local)
cd_api <- antaresRead::readClusterResDesc(opts = opts_api)
all.equal(cd_api, cd_local[, colnames(cd_api), with = FALSE])

# bindingConstratins
bc_local <- antaresRead::readBindingConstraints(opts = opts_local)
bc_api <- antaresRead::readBindingConstraints(opts = opts_api)
all.equal(bc_api, bc_local)

antaresRead::readBindingConstraints()

# antaresRead::readOptimCriteria(opts = opts_local)
# TO DO ?


# readAntares
dt_local <-  readAntares(opts = opts_local, areas = "all", mcYears = 1)
dt_api <-  readAntares(opts = opts_api, areas = "all", mcYears = 1)

data.table::fsetequal(dt_local, dt_api) 

dt_local <-  readAntares(opts = opts_local, areas = c('area'), mcYears = 1)
dt_api <-  readAntares(opts = opts_api, areas = c('area'), mcYears = 1)

data.table::fsetequal(dt_local, dt_api) 

dt_local <-  readAntares(opts = opts_local, links = "all", areas = "all", 
                         clusters = "all", districts = "all", clustersRes = "all", mcYears = 1)
dt_api <-  readAntares(opts = opts_api, links = "all", areas = "all", 
                       clusters = "all", districts = "all", clustersRes = "all", mcYears = 1)
sapply(1:length(dt_local), function(x){
  data.table::fsetequal(dt_local[[x]], dt_api[[x]]) 
})


for(ts in c("hourly", "daily", "weekly", "monthly", "annual")){
  print(ts)
  dt_local <-  readAntares(opts = opts_local, links = "all", areas = "all", clusters = "all", 
                           clustersRes = "all", districts = "all", mcYears = 1:2, timeStep = ts, showProgress = F)
  dt_api <-  readAntares(opts = opts_api, links = "all", areas = "all", clusters = "all", 
                         clustersRes = "all", districts = "all", mcYears = 1:2, timeStep = ts, showProgress = F)
  print(sapply(1:length(dt_local), function(x){
    data.table::fsetequal(dt_local[[x]], dt_api[[x]]) 
  }))
}


dt_local <-  readAntares(opts = opts_local, links = "all", areas = "all", 
                         clustersRes = "all", districts = "all", clusters = "all",
                       misc = TRUE, linkCapacity = TRUE, 
                       hydroStorageMaxPower = TRUE,
                       hydroStorage = FALSE,
                       thermalModulation = TRUE,
                       thermalAvailabilities = FALSE,
                       reserve = TRUE, mustRun = TRUE)

dt_api <-  readAntares(opts = opts_api, links = "all", areas = "all", 
                       clustersRes = "all", districts = "all", clusters = "all",
                       misc = TRUE, linkCapacity = TRUE, 
                       hydroStorageMaxPower = TRUE,
                       hydroStorage = FALSE,
                       thermalModulation = TRUE,
                       thermalAvailabilities = FALSE,
                       reserve = TRUE, mustRun = TRUE)


sapply(1:length(dt_local), function(x){
  data.table::fsetequal(dt_local[[x]], dt_api[[x]]) 
})


# readInputTS
load_local <- readInputTS(load = 'all', opts = opts_local)
load_api <- readInputTS(load = 'all', opts = opts_api)
data.table::fsetequal(load_local, load_api)

for(ts in c("hourly", "daily", "weekly", "monthly", "annual")){
  print(ts)
  fullinput_local <- readInputTS(load = 'all',
                                 ror = "all", 
                                 thermalAvailabilities = "all",
                                 hydroStorage = "all",
                                 hydroStorageMaxPower = "all",
                                 wind = "all",
                                 solar = "all",
                                 misc = "all",
                                 reserve = "all",
                                 linkCapacity = "all",
                                 opts = opts_local, 
                                 timeStep = ts, showProgress = FALSE)
  
  fullinput_api <- readInputTS(load = 'all',  
                               ror = "all", 
                               thermalAvailabilities = "all",
                               hydroStorage = "all",
                               hydroStorageMaxPower = "all",
                               wind = "all",
                               solar = "all",
                               misc = "all",
                               reserve = "all",
                               linkCapacity = "all",
                               opts = opts_api, 
                               timeStep = ts, showProgress = FALSE)
  
  
  print(sapply(1:length(fullinput_local), function(x){
    if(!is.null(fullinput_local[[x]]) && nrow(fullinput_local[[x]]) > 0){
      data.table::fsetequal(fullinput_local[[x]], fullinput_api[[x]]) 
    } else {
      TRUE
    }
  }))
  
}

# et en mode "input" ?
opts_api_input <- setSimulationPathAPI(
  host = host,
  study_id = study_id,
  token = token, 
  simulation = "input"
)

opts_local_input <- setSimulationPath(path = "C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\ENR_Cluster\\test_study_no_output/", "input")

for(ts in c("hourly", "daily", "weekly", "monthly", "annual")){
  print(ts)
  fullinput_local <- readInputTS(load = 'all',
                                 ror = "all", 
                                 thermalAvailabilities = "all",
                                 hydroStorage = "all",
                                 hydroStorageMaxPower = "all",
                                 wind = "all",
                                 solar = "all",
                                 misc = "all",
                                 reserve = "all",
                                 linkCapacity = "all",
                                 opts = opts_local_input, 
                                 timeStep = ts, showProgress = FALSE)
  
  fullinput_api <- readInputTS(load = 'all',  
                               ror = "all", 
                               thermalAvailabilities = "all",
                               hydroStorage = "all",
                               hydroStorageMaxPower = "all",
                               wind = "all",
                               solar = "all",
                               misc = "all",
                               reserve = "all",
                               linkCapacity = "all",
                               opts = opts_api_input, 
                               timeStep = ts, showProgress = FALSE)
  
  
  print(sapply(1:length(fullinput_local), function(x){
    if(!is.null(fullinput_local[[x]]) && nrow(fullinput_local[[x]]) > 0){
      data.table::fsetequal(fullinput_local[[x]], fullinput_api[[x]]) 
    } else {
      TRUE
    }
  }))
}
