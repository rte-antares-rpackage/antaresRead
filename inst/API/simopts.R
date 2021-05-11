library(antaresRead)
library(jsonlite)
library(curl)
library(httr)
library(RCurl)
library(data.table)

host <- "http://localhost:8080"
path <- file.path("http://localhost:8080", "studies/antaresStd")

# 
# opts <- setSimulationPathAPI(path, "input")
opts <- setSimulationPathAPI(path, 1)

readInputTS(load = "all")


readAntares(opts = opts)
areas = NULL
links = NULL
clusters = NULL
districts = NULL
misc = FALSE
thermalAvailabilities = FALSE
hydroStorage = FALSE
hydroStorageMaxPower = FALSE
reserve = FALSE
linkCapacity = FALSE
mustRun = FALSE
thermalModulation = FALSE
select = NULL
mcYears = NULL
timeStep = c("hourly", "daily", "weekly", "monthly", "annual")
opts = simOptions()
parallel = FALSE
simplify = TRUE
showProgress = TRUE


dd <- httr::HEAD("http://localhost:8080/file/antaresStd/output/20201015-0957eco-test3a/economy/mc-all/areas/de/details-hourly.txt")


system.time(fread("http://localhost:8080/file/antaresStd/output/20201015-0957eco-test3a/economy/mc-all/areas/de/ddetails-hourly.txt"))


try({fread("http://localhost:8080/file/antaresStd/output/20201015-0957eco-test3a/economy/mc-all/areas/de/ddetails-hourly.txt")})


opop <- setSimulationPath("C:/Users/TitouanRobert/Desktop/antaresStd", 1)
system.time(readAntares(areas = "all", links = "all", mcYears = "all", opts = opop))


path <- "http://localhost:8080/file/antaresStd/output/20201015-0957eco-test3a/economy/mc-all/areas/de/details-hourly.txt"


fread(path, skip = 7, showProgress = FALSE)


url.exists(c(path, path))


url.exists("http://localhost:8080/studies/antaresStd/output/1/economy/mc-all/areas/at/values-hourly.txt")


download.file("http://localhost:8080/file/antaresStd/output/20201015-0957eco-test3a/economy/mc-all/areas/at/details-hourly.txt",
              "C:/Users/TitouanRobert/Desktop")
