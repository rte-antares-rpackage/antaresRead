#'
#'
#'
#'
# RULES :
# read only written files values
# no default values for TS => return data.table() empty
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

  # filter properties
  df_prop_filtered <- df_structured[grep(pattern = ".ini", x = file)]

  # filter TS values
  df_ts_filtered <- df_structured[grep(pattern = ".txt", x = file)]

  # TODO split/filter by area

  list_prop <- lapply(seq(1, nrow(df_prop_filtered)), function(x){

    # names structure
    clust_name <- df_prop_filtered[x, cluster_name]
    area_name <- df_prop_filtered[x, area]

    # read TS
    df_ts <- df_ts_filtered[area %in% area_name,]
    l_ts <- lapply(df_ts$full_path, function(x){
      fread_antares(opts = opts,
                    file = x,
                    integer64 = "numeric",
                    header = FALSE,
                    showProgress = FALSE)
    })

    names(l_ts) <- sub(pattern = ".txt", replacement = "", x = df_ts$file)

    # output list
    ll <- list(
      list(properties=readIniFile(df_prop_filtered[x, full_path]),
           values=l_ts))

    names(ll) <- clust_name
    ll
   })

  names(list_prop) <- df_prop_filtered[, area]
  list_prop
}
