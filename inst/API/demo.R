library(jsonlite)
library(httr)
library(antaresRead)

###DEMO antares read

##PATH file system NON API
opts2 <- setSimulationPath("C:/Users/TitouanRobert/Desktop/antaresStd", 1)



##PATH api
path <- "http://localhost:8080/studies/antaresStd/"
opts <- setSimulationPathAPI(path, 1)


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

