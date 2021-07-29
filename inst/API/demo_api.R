library(antaresRead)

# ./server -c config.yaml --auto-upgrade-db

##PATH api
path <- "http://pyre.datastorm.fr:8080/studies/05696f33-cef4-4969-aab2-357175a30b01"
token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ7XCJpZFwiOiAyLCBcImltcGVyc29uYXRvclwiOiAxLCBcInR5cGVcIjogXCJib3RzXCIsIFwiZ3JvdXBzXCI6IFtdfSIsImlhdCI6MTYyNzQ4MTI4MywibmJmIjoxNjI3NDgxMjgzLCJqdGkiOiI4YzRhMDIzZC05NTFiLTRiZTYtYmMyNS0xNWNmNGZhZDg3ZTMiLCJleHAiOjc5ODY1MjEyODMsInR5cGUiOiJhY2Nlc3MiLCJmcmVzaCI6ZmFsc2V9.H3rFD5sUCS1Iic02Uehc-Fhr32K1oNJNeZakvY_sX3w"
simulation = 2
opts <- setSimulationPathAPI(path, token = token, simulation = 2)

antaresRead::readLayout()
antaresRead::readClusterDesc()
antaresRead::readBindingConstraints()

# antaresRead::readOptimCriteria()
reserve
readAntares(clusters = "all")

readAntares(links = "all", reserve = T, mcYears = 1)

readAntares(areas = "a", hydroStorage = T, mcYears = 1)

simulation = 1
opts <- setSimulationPathAPI(path, token = token, simulation = simulation)

opts
opts2


##Lire des données
dt1 <-  readAntares(opts = opts)
dt2 <-  readAntares(opts = opts2)

dt1
dt2

data.table::fsetequal(dt1, dt2) 

system.time( readAntares(opts = opts))
system.time( readAntares(opts = opts2))

## Les layout
ly2 <- readLayout(opts = opts)
ly1 <- readLayout(opts = opts2)

data.table::fsetequal(ly2$areas, ly1$areas)
data.table::fsetequal(ly2$districts, ly1$districts)
data.table::fsetequal(ly2$links, ly1$links)
data.table::fsetequal(ly2$districtLinks, ly1$districtLinks)

## Cluster desc
cl1 <- readClusterDesc(opts)
cl2 <- readClusterDesc(opts2)
data.table::fsetequal(cl1, cl2[, .SD, .SDcols = names(cl1)])


##Les inputs
load1 <- readInputTS(load = 'at', opts = opts)
load2 <- readInputTS(load = 'at', opts = opts2)
data.table::fsetequal(load1, load2)

