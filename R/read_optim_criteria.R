#' Read Optimization Criteria
#' 
#' @inheritParams readAntares
#' 
#' @export
#' 
readOptimCriteria <- function(opts = simOptions) {
  fileList <- list.files(opts$simPath, pattern = "criterion", full.names = TRUE)
  if (length(fileList) == 0) stop("Optimization criteria not found")
  
  info <- str_match(fileList, "criterion-(\\d+)-(\\d+)-\\d+-\\d+-?(\\d*)")
  mcYear <- as.numeric(info[,2])
  weekId <- as.numeric(info[,3])
  
  # Correctly order files: we cannot just use alphanumerical order because file
  # names are a mix of text and numbers 
  # For instance "criterion-1-1-20170101-0000-10" would be before
  # "criterion-1-1-20170101-0000-2". We fix that by replacing this name by:
  # "criterion-1-1-20170101-0000-02"
  
  idx <- info[, 4]
  idx[idx == ""] <- "0"
  idx <- sprintf("%02.f", as.numeric(idx))
  fixedFileNames <- str_replace(basename(fileList), 
                                "((-\\d+){4})-?\\d*.txt$", 
                                paste0("\\1-",idx))
  
  # extract values from files
  values <- vapply(fileList, function(f) {
    value <- readLines(f)
    value <- str_replace(value, "^.+ : +", "")
    as.numeric(value)
  }, FUN.VALUE = numeric(1), USE.NAMES = FALSE)
  
  res <- data.table(mcYear = mcYear, weekId = weekId, value = values, name = fixedFileNames)
  res <- res[, data.table(matrix(value[order(name)], ncol = 2)), 
             keyby = .(mcYear, weekId)]
  setnames(res, 3:4, c("criterion1", "criterion2"))
  
  # Set timeId
  
  # If weekly optimisation
  firstWeek <- .getTimeId(opts$timeIdMin, "weekly", opts)
  res[,timeId := firstWeek + weekId - 1]
  
  # If daily optimisation
  firstDay <- .getTimeId(opts$timeIdMin, "daily", opts)
  res[, timeId := firstDay - 1 + 1:.N + 7 * (weekId - 1), keyby = .(mcYear, weekId)]
}
