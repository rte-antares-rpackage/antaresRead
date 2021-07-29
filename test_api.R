require(antaresRead)
require(jsonlite)

opts <- setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\test_case")

setSimulationPath("C:\\Users\\BenoitThieurmel\\Desktop\\Antares\\Test_packages_R", 1)

# C:\Users\BenoitThieurmel\Desktop\server-windows-latest.exe -j C:\Users\BenoitThieurmel\Documents\git\api-iso-antares\examples\jsonschemas\STA-mini\jsonschema.json -s C:\Users\BenoitThieurmel\Desktop\Antares
# C:\Users\BenoitThieurmel\Desktop\server-windows-latest.exe -j C:\Users\BenoitThieurmel\Documents\git\api-iso-antares\examples\jsonschemas\STA-mini\jsonschema.json -s C:\Users\BenoitThieurmel\Documents\git\api-iso-antares\examples\studies

test <- jsonlite::read_json("http://localhost:8080/studies/STA-mini/")

require(data.table)
require(RCurl)

file_path <- "http://localhost:8080/file/STA-mini/output/20201014-1422eco-hello/economy/mc-ind/00001/areas/fr/details-hourly.txt"

read_RCurl <- function(file_path){
  txt <- RCurl::getURL(file_path)
  data <- fread(text = txt, skip = 7)
  data
}

profvis::profvis({
  read_RCurl(file_path)
})

microbenchmark::microbenchmark(data.table::fread(file_path, skip = 7, showProgress = FALSE),
                               read_RCurl(file_path), times = 500)
  


system.time({
  read_RCurl(file_path)
})

system.time(t <- curl::curl_fetch_memory(file_path))
system.time({
  test_txt <- data.table::fread("C:\\Users\\BenoitThieurmel\\Documents\\git\\api-iso-antares\\examples\\studies\\STA-mini\\output\\20201014-1422eco-hello\\economy\\mc-ind\\00001\\areas\\de\\details-hourly.txt", skip = 7, showProgress = FALSE)
})


system.time({
  test_txt_rt <- data.table::as.data.table(read.table(file_path, skip = 7))
})


mydat <- read.table(textConnection(file_path), skip = 7)

require(profvis)

profvis::profvis({
  test_txt <- data.table::fread(file_path, skip = 7, sep = "\t", header = F,
                                integer64 = "numeric",
                                na.strings = "N/A", showProgress = FALSE)
})

jsonlite::read_json("http://localhost:8080/file/STA-mini/output/20201014-1422eco-hello/economy/mc-ind/00001/areas/de/details-hourly.txt") 


all_output_info <- jsonlite::read_json("http://localhost:8080/studies/STA-mini/output/")

?readAntares
?readInputTS

data <- readAntares(areas = "all", 
                    mcYears = c(1, 2), 
                    timeStep = "hourly", 
                    opts = opts)

data <- readAntares(areas = "all", links = "all", mcYears = c(1, 2), 
                    timeStep = "hourly")

data$links

?readInputTS



x <- readAntares(areas = "all",  links = "all", timeStep = "hourly")
x <- change_antares_year(x, year = 2050)
class(data$time)

timestamp <- as.POSIXct(opts$start)
lubridate::hour(timestamp) <- lubridate::hour(timestamp) + 1:(24*365) - 1

timestamp[x$areas$timeId]

opts <- simOptions(x)
if(!is.null(x$areas)){
  x$areas[,time]
  x$areas[,time := antaresRead:::.timeIdToDate(x$areas$timeId, attr(x, "timeStep"), opts)]
}

if(!is.null(x$links)){
  x$links[,time := antaresRead:::.timeIdToDate(x$links$timeId, attr(x, "timeStep"), opts)]
}

init_dateRange <- range(as.Date(x$areas$time))

year(data$time)

month(data$time)

data$time[1:10]

as.POSIXct(format(data$time[1:10], format = "2050-%m-%d %H:%M:%S"), tz = "UTC")

change_antares_year <- function(data, year){
  
  if("antaresDataTable" %in% class(data) && nrow(data) > 0){
    begin_year <- as.numeric(substring(x$time, 1, 4))
    diff_year <- year - min(begin_year)
    begin_year <- begin_year + diff_year
    data[, time := paste0(begin_year, substring(time, 5))]
    if("hourly" %in% attributes(data)$timeStep){
      data[, time := as.POSIXct(time, tz = "UTC")]
    } else if("daily" %in% attributes(data)$timeStep){
      data[, time := as.Date(time)]
    } else if("weekly" %in% attributes(data)$timeStep){
      data[, time := as.factor(time)]
    } else if("monthly" %in% attributes(data)$timeStep){
      data[, time := as.character(time)]
    } else if("annual" %in% attributes(data)$timeStep){
      data[, time := as.numeric(time)]
    }
    
    attributes(data)$opts$start <- as.POSIXct(paste0(as.numeric(substring(attributes(data)$opts$start, 1, 4)) + diff_year, substring(attributes(data)$opts$start, 5)), tz = "UTC")
  } else if("antaresDataList" %in% class(data) && length(data) > 0){
    ctrl <- sapply(1:length(data), function(i){
      x <- data[[i]]
      if(!is.null(x) & nrow(x) > 0){
        begin_year <- as.numeric(substring(x$time, 1, 4))
        diff_year <- year - min(begin_year)
        begin_year <- begin_year + diff_year
        x[, time := paste0(begin_year, substring(time, 5))]
        if("hourly" %in% attributes(data)$timeStep){
          x[, time := as.POSIXct(time, tz = "UTC")]
        } else if("daily" %in% attributes(data)$timeStep){
          x[, time := as.Date(time)]
        } else if("weekly" %in% attributes(data)$timeStep){
          x[, time := as.factor(time)]
        } else if("monthly" %in% attributes(data)$timeStep){
          x[, time := as.character(time)]
        } else if("annual" %in% attributes(data)$timeStep){
          x[, time := as.numeric(time)]
        }
        if(i == 1){
          new_start <<- as.POSIXct(paste0(as.numeric(substring(attributes(data)$opts$start, 1, 4)) + diff_year, substring(attributes(data)$opts$start, 5)), tz = "UTC")
        }
        attributes(data[[i]])$opts$start <<- new_start
      }
      invisible()
    })
    
    attributes(data)$opts$start <- new_start
  }
  
  attributes(data)$opts$parameters$general$horizon <- year
  
  data
}

data <- change_antares_year(data, year = 2050)

data_area <- data$areas
as.Date(antaresRead:::.timeIdToDate(range(data_area$timeId), timeStep = "hourly", simOptions(data_area)))
