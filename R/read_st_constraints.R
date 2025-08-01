#'
#'
#'
#'
# RULES :
# read only written files values
# no default values for TS => return data.table() empty
read_storages_constraints <- function(opts=simOptions()){
  browser()

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

  # read properties
  df_prop_filtered <- df_structured[grep(pattern = ".ini", x = file)]

  # TODO split/filter by area

  list_prop <- lapply(seq(1, nrow(df_prop_filtered)), function(x){
    clust_name <- df_structured[x, cluster_name]
    area <- df_structured[x, area]

    ll <- list(readIniFile(df_structured[x, full_path]))
    names(ll) <- clust_name
    ll
   })

  names(list_prop) <- df_prop_filtered[, area]

}
