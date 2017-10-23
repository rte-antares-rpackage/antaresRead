#' Get timeId from antares study
#'
#' @param data \code{antaresDataList} see \link{readAntares}
#' @param timeStep \code{character} timeStep
#'
#' @noRd
getTime <- function(data, timeStep){
  time <- unique(data[[1]]$time)
  current_locale <- Sys.getlocale(category = "LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  if(timeStep %in% c("weekly", "monthly")){
    dt_date <- data.table(time = as.character(time))

  }else if(timeStep == "annual"){
    dt_date <- data.table(time)

  }else{
    dt_date <- data.table(IDateTime(time, tz =" UTC"))

  }
  Sys.setlocale("LC_TIME", current_locale)
  dt_date
}

#' Read time and generate column who can be calculate from time
#'
#' @param fid \code{H5IdComponent} id of h5 file open which \link[rhdf5]{H5Fopen}
#' @param group \code{group} group where time are stocked
#'
#' @noRd
#' 
getAllDateInfoFromDate <- function(fid, group){
  # affectation des classes
  
  if(!requireNamespace("rhdf5", versionCheck = list(op = ">=", version = rhdf5_version))) stop(rhdf5_message)
  
  groupT <- paste0(group, "/time")
  did <- rhdf5::H5Dopen(fid, groupT)
  datetime_data <- data.table(rhdf5::H5Dread(did))


  rhdf5::H5Dclose(did)
  if(group %in% c("weekly", "annual")){
    return(datetime_data)
  }

  current_locale <- Sys.getlocale(category = "LC_TIME")
  # mise en locale english pour le time (extraction des mois)
  Sys.setlocale("LC_TIME", "C")
  if(group == c("monthly")){
    timCop <- datetime_data$time
    timCop <- paste0(timCop, "-01")
    timCop <- as.Date(timCop)
    class(timCop) <- c("IDate", "Date")
    datetime_data$month <- as.factor(toupper(format(timCop, format = "%b")))
    return(datetime_data)
  }
  idate <- NULL
  itime <- NULL
  class(datetime_data$idate) <- c("IDate", "Date")
  class(datetime_data$itime) <- c("ITime")
  # recuperation de la locale actuelle du pc
  uniqueDate <- unique(datetime_data[,.SD, .SDcols = "idate"])
  uniqueTime <-  unique(datetime_data[,.SD, .SDcols = "itime"])


  # Calcul day & mounth
  uniqueDate[,c("day", "month") := list(
    mday(idate),
    as.factor(toupper(format(idate, format = "%b"))))]



  # calculs
  datetime_data[, c("time") := list(
    as.POSIXct(idate, time = itime, tz = "UTC")
  )]

  if(group == "daily"){
    datetime_data$time <- as.Date(datetime_data$time)
  }

  mthList <- c("APR",
               "AUG",
               "DEC",
               "FEB",
               "JAN",
               "JUL",
               "JUN",
               "MAR",
               "MAY",
               "NOV",
               "OCT",
               "SEP")

  toAdd <- mthList[!mthList %in% levels(uniqueDate$month)]
  if(length(toAdd)>0)
  {
    levels(uniqueDate$month) <- c(levels(uniqueDate$month), toAdd)
  }

  if(group == "hourly")
  {
    uniqueTime[, c("hour") := as.factor(substring(as.character(itime), 1, 5))]

  }
  uniqueDate$month <- factor(uniqueDate$month , levels(uniqueDate$month)[order(levels(uniqueDate$month))])

  #Merge
  datetime_data <- merge(datetime_data, uniqueDate)
  datetime_data <- merge(datetime_data, uniqueTime, by = "itime")
  datetime_data[, "idate" := NULL]
  datetime_data[, "itime" := NULL]
  setkey(datetime_data, "time")
  Sys.setlocale("LC_TIME", current_locale)
  datetime_data
}


#' Write time in h5 file
#'
#' @param data \code{antaresDataList} see \link{readAntares}
#' @param path \code{character} path of h5 file
#' @param group \code{group} group where time are stored
#'
#' @noRd
writeTime <- function(data, path, group){
  if(!requireNamespace("rhdf5", versionCheck = list(op = ">=", version = rhdf5_version))) stop(rhdf5_message)
  time <- getTime(data, group)
  group <- paste0(group, "/time")
  rhdf5::h5write.default(time, path, group)
  rhdf5::H5close()
}
