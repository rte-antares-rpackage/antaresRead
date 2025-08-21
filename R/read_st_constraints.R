
utils::globalVariables(c("cluster_name", "full_path"))

# RULES :
# read only written files values
# no default values for TS => return data.table() empty

#' @title Read Short-term storages / additional constraints
#' @description
#' `r antaresRead:::badge_api_no()`
#' `r lifecycle::badge("experimental")`
#'
#' This function reads constraints of an Antares project (by area/cluster) :
#'  - Properties
#'  - Time series
#'
#' *Be aware that constraints are read in the input files of a study.
#' So they may have changed since a simulation has been run.*
#'
#' @inheritParams readAntares
#'
#' @return `list` with 2 sections per cluster/constraint (properties + values).
#'
#' @importFrom data.table data.table rbindlist
#' @export
#'
#' @examples
#' \dontrun{
#' # read/load an existing study (version >= 9.2)
#' setSimulationPath(path = "mypath/study")
#'
#' read_storages_constraints()
#' }
read_storages_constraints <- function(opts=simOptions()){
  assertthat::assert_that(inherits(opts, "simOptions"))
  stopifnot(opts$antaresVersion>=920)

  ##
  # API bloc
  ##
  if(is_api_study(opts = opts)){}

  ##
  # Desktop
  ##
  path <- file.path(opts$inputPath,
                    "st-storage",
                    "constraints")

  # scan current directory
  current_files <- data.table(
    full_path = list.files(path, recursive = TRUE, full.names = TRUE),
    content_data = list.files(path, recursive = TRUE))

  # split and structure data
  splited_values <- strsplit(current_files$content_data, split = "/")

  list_structured <- lapply(splited_values, function(x){
    df <- data.table(area=x[1],
                     cluster_name=x[2],
                     file=x[3])
    df
  })

  df_structured <- rbindlist(list_structured)
  current_files$content_data <- NULL

  df_structured <- cbind(current_files,  df_structured)

  # re split to have a list by area
  df_structured_splited <- split(x = df_structured, f = df_structured$area)

  # read and build object with properties + values
  list_prop <- lapply(names(df_structured_splited), function(x){

    df <- df_structured_splited[[x]]

    # filter properties
    df_prop_filtered <- df[grep(pattern = ".ini", x = file)]

    # filter TS values
    df_ts_filtered <- df[grep(pattern = ".txt", x = file)]

    # names structure
    cluster_names <- df_prop_filtered[, cluster_name]
    area_name <- x

    # read properties + TS by cluster
    l_ts <- lapply(cluster_names, function(x_c){
      # properties part
      ini_path <- df_prop_filtered[cluster_name %in% x_c, full_path]
      properties <- readIniFile(ini_path)

      # ts part
      ts_path <- df_ts_filtered[cluster_name %in% x_c, full_path]
      ts_file_names <- df_ts_filtered[cluster_name %in% x_c, file]

      if(identical(ts_path, character(0)))
        return(list(
          properties=properties,
          values=data.table()))

      # read one or multiple ts files
      list_ts <- lapply(ts_path, function(x_ts){
        ts_value <- fread_antares(opts = opts,
                                  file = x_ts,
                                  integer64 = "numeric",
                                  header = FALSE,
                                  showProgress = FALSE)
      })

      names(list_ts) <- sub(pattern = ".txt",
                            replacement = "",
                            x = ts_file_names)

      return(list(
        properties=properties,
        values=list_ts))
    })

    names(l_ts) <- cluster_names

    # output list
    l_ts
  })

  names(list_prop) <- names(df_structured_splited)
  list_prop
}
