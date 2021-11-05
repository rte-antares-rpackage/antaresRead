library(antaresRead)

# ./server -c config.yaml --auto-upgrade-db

# PATH api
host <- "http://pyre.datastorm.fr:8080"
study_id <- "3decaa72-9b75-494c-b7a9-e90644e26015"
token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ7XCJpZFwiOiAzLCBcImltcGVyc29uYXRvclwiOiAxLCBcInR5cGVcIjogXCJib3RzXCIsIFwiZ3JvdXBzXCI6IFtdfSIsImlhdCI6MTYzMTE4Mzg3MywibmJmIjoxNjMxMTgzODczLCJqdGkiOiIwZDQwOGE2My1mMWQ1LTRjN2QtOTYwMC1jZmNiOWIzZjFhODMiLCJleHAiOjc5OTAyMjM4NzMsInR5cGUiOiJhY2Nlc3MiLCJmcmVzaCI6ZmFsc2V9.0NqQBmjFEpNAcZAeqV9PXzJipMR15pcsefyAARstwFM"
simulation = 2

opts_api <- setSimulationPathAPI(host = host, 
                                 study_id = study_id, 
                                 token = token, 
                                 simulation = simulation)

opts_local <- setSimulationPath(path = "C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\Test_packages_R", simulation = 2)

all.equal(opts_api$parameters$general, opts_local$parameters$general)

getAreas(opts = opts_local)
getDistricts(opts = opts_local)
getLinks(opts = opts_local)

getAreas(opts = opts_api)
getDistricts(opts = opts_api)
getLinks(opts = opts_api)

# layout
ly_local <- antaresRead::readLayout(opts = opts_local)
ly_api <- antaresRead::readLayout(opts = opts_api)
all.equal(ly_api, ly_local)

# clusterDesc
cd_local <- antaresRead::readClusterDesc(opts = opts_local)
cd_api <- antaresRead::readClusterDesc(opts = opts_api)
all.equal(cd_api, cd_local[, colnames(cd_api), with = FALSE])

# bindingConstratins
bc_local <- antaresRead::readBindingConstraints(opts = opts_local)
bc_api <- antaresRead::readBindingConstraints(opts = opts_api)
all.equal(bc_api, bc_local)

antaresRead::readBindingConstraints()

# antaresRead::readOptimCriteria(opts = opts_local)
# TO DO ?

# readAntares
dt_local <-  readAntares(opts = opts_local, areas = "all")
dt_api <-  readAntares(opts = opts_api, areas = "all")

data.table::fsetequal(dt_local, dt_api) 

dt_local <-  readAntares(opts = opts_local, areas = c('a', 'b'), mcYears = 1:2)
dt_api <-  readAntares(opts = opts_api, areas = c('a', 'b'), mcYears = 1:2)

data.table::fsetequal(dt_local, dt_api) 

dt_local <-  readAntares(opts = opts_local, links = "all", areas = "all", 
                         clusters = "all", districts = "all")
dt_api <-  readAntares(opts = opts_api, links = "all", areas = "all", 
                       clusters = "all", districts = "all")
sapply(1:length(dt_local), function(x){
  data.table::fsetequal(dt_local[[x]], dt_api[[x]]) 
})

dt_local <-  readAntares(opts = opts_local, links = "all", areas = "all", 
                         clusters = "all", districts = "all")
dt_api <-  readAntares(opts = opts_api, links = "all", areas = "all", 
                       clusters = "all", districts = "all")
sapply(1:length(dt_local), function(x){
  data.table::fsetequal(dt_local[[x]], dt_api[[x]]) 
})

for(ts in c("hourly", "daily", "weekly", "monthly", "annual")){
  print(ts)
  dt_local <-  readAntares(opts = opts_local, links = "all", areas = "all", clusters = "all", 
                           districts = "all", mcYears = 1:2, timeStep = ts, showProgress = F)
  dt_api <-  readAntares(opts = opts_api, links = "all", areas = "all", clusters = "all", 
                         districts = "all", mcYears = 1:2, timeStep = ts, showProgress = F)
  print(sapply(1:length(dt_local), function(x){
    data.table::fsetequal(dt_local[[x]], dt_api[[x]]) 
  }))
}


dt_local <-  readAntares(opts = opts_local, links = opts_local$linkList[1:2], 
                         areas = opts_local$areaList[1:2], clusters = NULL, 
                         districts = "all", mcYears = 1:2)
dt_api <-  readAntares(opts = opts_api, links = opts_local$linkList[1:2], 
                       areas = opts_local$areaList[1:2], clusters = NULL, 
                       districts = "all", mcYears = 1:2)

sapply(1:length(dt_local), function(x){
  data.table::fsetequal(dt_local[[x]], dt_api[[x]]) 
})


dt_local <-  readAntares(opts = opts_local, links = "all", areas = "all", 
                       clusters = "all", districts = "all",
                       misc = TRUE, linkCapacity = TRUE, 
                       hydroStorageMaxPower = TRUE,
                       hydroStorage = TRUE,
                       thermalModulation = TRUE,
                       thermalAvailabilities = TRUE,
                       reserve = TRUE, mustRun = TRUE)

dt_api <-  readAntares(opts = opts_api, links = "all", areas = "all", 
                       clusters = "all", districts = "all",
                       misc = TRUE, linkCapacity = TRUE, 
                       hydroStorageMaxPower = TRUE,
                       hydroStorage = TRUE,
                       thermalModulation = TRUE,
                       thermalAvailabilities = TRUE,
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
    data.table::fsetequal(fullinput_local[[x]], fullinput_api[[x]]) 
  }))
}

# et en mode "input" ?
opts_api_input <- setSimulationPathAPI(
  host = host,
  study_id = study_id,
  token = token, 
  simulation = "input"
)

opts_local_input <- setSimulationPath(path = "C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\Test_packages_R", "input")

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
    data.table::fsetequal(fullinput_local[[x]], fullinput_api[[x]]) 
  }))
}
