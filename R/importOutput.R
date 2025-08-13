# Copyright © 2016 RTE Réseau de transport d’électricité

#' .getOutputHeader
#'
#' Private function that uses the first lines of an output file to generate
#' column names for this file.
#'
#' @param path
#' Path of the output file
#' @param objectName
#' (character) object type represented in the file (area ou link)
#'
#' @return
#' Vector containing the generated column names.
#'
#' @noRd
#'
.getOutputHeader <- function(path, objectName, api = FALSE, token = NULL, timeout = 60, config = list()) {
  if(!api){
    colname <- read.table(path, header = F, skip = 4, nrows = 3, sep = "\t")
  } else {
    path <- gsub(".txt$", "", path)
    path <- paste0(path, "&formatted=false")
    if(!is.null(token) && token != ""){
      httpResponse <- GET(utils::URLencode(path), timeout(timeout),
                          add_headers(Authorization = paste0("Bearer ", token)), config = config)
    } else {
      httpResponse <- GET(utils::URLencode(path), timeout(timeout), config = config)
    }

    colname <- tryCatch({fread(content(httpResponse, "parsed"), header = F, skip = 4, nrows = 3, sep = "\t")},
                        error = function(e) NULL)
  }
  if(!is.null(colname)){
    colname <- apply(colname[c(1,3),], 2, paste, collapse = "_")
    colname[1:2] <- c(objectName, "timeId")
    colname <- gsub("^_|_EXP$|_values$|_$", "", colname)
  }

  colname
}


#' .check_missing_output_files
#'
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{\link{setSimulationPath}}
#' @param args
#' (data frame) contains the arguments to read the outputs
#'
#' @return
#' Logical vector containing the existence of the output file.
#'
#' @noRd
#'
.check_missing_output_files <- function(opts, args){

  if(is_api_study(opts)){
    outputMissing <- !sapply(gsub(pattern = ".txt$", replacement = "", args$path), FUN = .getSuccess, token = opts$token)
  }else{
    outputMissing <- !file.exists(args$path)
  }

  return(outputMissing)
}


#' .generate_output_data_to_read
#'
#' @return
#' Data frame containing the file to read in column path
#'
#' @noRd
#'
.generate_output_data_to_read <- function(folder, fileName, ids, timeStep, mcYears, opts){

  api_study <- is_api_study(opts = opts)
  w_links_folder <- folder %in% "links"
  no_mcYears <- is.null(mcYears)

  if (no_mcYears) {
    if(w_links_folder && api_study){
      args <- merge(expand.grid(link = ids), opts$linksDef)
      args$id <- paste0(args$from, "/", args$to)
    } else {
      args <- expand.grid(id = ids)
    }
  } else {
    if(w_links_folder && api_study){
      args <- merge(expand.grid(link = ids, mcYear = mcYears), opts$linksDef)
      args$id <- paste0(args$from, "/", args$to)
    } else {
      args <- expand.grid(id = ids, mcYear = mcYears)
    }
  }

  if (no_mcYears) {
    args$path <- sprintf("%s/mc-all/%s/%s/%s-%s.txt",
                         opts[["simDataPath"]], folder, args$id, fileName, timeStep)
  } else {
    args$path <- sprintf("%s/mc-ind/%05.0f/%s/%s/%s-%s.txt",
                         opts[["simDataPath"]], args$mcYear, folder, args$id, fileName, timeStep)
  }

  if (w_links_folder && api_study) {
    args$id <- args$link
  }

  return(args)
}


#' .importOutput
#'
#' Private function used to import the results of a simulation. The type of result
#' is determined by the arguments "folder" and "file"
#' - "areas", "values"  => areas
#' - "areas", "details" => clusters
#' - "areas", "details-res" => renewables clusters
#' - "areas", "details-STstorage" => short-term clusters
#' - "links", "values"  => links
#'
#' @return
#' a table if synthesis=TRUE or a list of tables (one table per Monte-Carlo year)
#'
#' @noRd
#'
.importOutput <- function(folder, fileName, objectName, ids, timeStep, select = NULL,
                          mcYears = NULL,
                          showProgress, opts, processFun = NULL, sameNames = TRUE,
                          objectDisplayName = objectName, parallel) {

  if (is.null(ids)) return(NULL)

  if (showProgress) cat("Importing ", objectDisplayName, "s\n", sep = "")

  args <- .generate_output_data_to_read(folder = folder, fileName = fileName, ids = ids, timeStep = timeStep, mcYears = mcYears, opts = opts)
  outputMissing <- .check_missing_output_files(opts = opts, args = args)

  if (all(outputMissing)) {
    message("No data corresponding to your query.")
    return(NULL)
  } else if (any(outputMissing)) {
    message("Some requested output files are missing.")
    args <- args[!outputMissing, ]
  }

  # columns to retrieve
  if (sameNames) {
    colNames <- .getOutputHeader(
      args$path[1], objectName, api = "api" %in% opts$typeLoad,
      token = opts$token, timeout = opts$timeout, config = opts$httr_config
    )

    if (is.null(select)) {
      # read all columns except the time variables that will be recreated
      selectCol <- which(!colNames %in% pkgEnv$idVars)
    } else {
      selectCol <- which(colNames %in% select)
    }
    colNames <- colNames[selectCol]
  }

  # time ids
  if (timeStep == "annual") {
    timeIds <- 1L
  } else {
    timeRange <- .getTimeId(c(opts$timeIdMin, opts$timeIdMax), timeStep, opts)
    timeIds <- seq(timeRange[1], timeRange[2])
  }


  if(!is.null((getDefaultReactiveDomain())))
  {
    n <- nrow(args)
    withProgress(message = 'antaresRead', value = 0, {
      res <- llply(
        1:nrow(args),
        function(i) {
          if(showProgress){ incProgress(1/n, detail = paste0("Importing ", folder, " data")) }
          data <- NULL
          try({
            if (!sameNames) {
              colNames <- .getOutputHeader(
                args$path[i], objectName, api = "api" %in% opts$typeLoad,
                token = opts$token, timeout = opts$timeout, config = opts$httr_config
              )
              selectCol <- which(!colNames %in% pkgEnv$idVars)
              colNames <- colNames[selectCol]
            }

            if (length(selectCol) == 0) {
              if(opts$typeLoad != "api"){
                data <- data.table(timeId = timeIds)
              } else {
                data <- NULL
              }
            } else {
              data <- fread_antares(opts = opts, file = args$path[i],
                                    sep = "\t", header = F, skip = 7,
                                    select = selectCol, integer64 = "numeric",
                                    na.strings = "N/A")

              if(!is.null(data)){
                # fix data.table bug on integer64
                any_int64 <- colnames(data)[which(sapply(data, function(x) "integer64" %in% class(x)))]
                if(length(any_int64) > 0){
                  data[, c(any_int64) := lapply(.SD, as.numeric), .SDcols = any_int64]
                }

                setnames(data, names(data), colNames)
                data[, timeId := timeIds]
              }
            }

            if(!is.null(data)){
              data[, c(objectName) := args$id[i]]
              if (!is.null(mcYears)) data[, mcYear := args$mcYear[i]]

              if (!is.null(processFun)) data <- processFun(data)
            }
            data
          })
          data
        },
        .progress = ifelse(showProgress, "text", "none"),
        .parallel = parallel,
        .paropts = list(.packages = "antaresRead")
      )
    })

  }else{
    res <- llply(
      1:nrow(args),
      function(i) {
        if (!sameNames) {
          colNames <- .getOutputHeader(
            args$path[i], objectName, api = "api" %in% opts$typeLoad,
            token = opts$token, timeout = opts$timeout, config = opts$httr_config
          )
          selectCol <- which(!colNames %in% pkgEnv$idVars)
          colNames <- colNames[selectCol]
        }

        if (length(selectCol) == 0) {
          if(opts$typeLoad != "api"){
            data <- data.table(timeId = timeIds)
          } else {
            data <- NULL
          }
        } else {
          data <- fread_antares(opts = opts, file = args$path[i],
                                sep = "\t", header = F, skip = 7,
                                select = selectCol, integer64 = "numeric",
                                na.strings = "N/A", showProgress = FALSE)

          if(!is.null(data)){
            # fix data.table bug on integer64
            any_int64 <- colnames(data)[which(sapply(data, function(x) "integer64" %in% class(x)))]
            if(length(any_int64) > 0){
              data[, c(any_int64) := lapply(.SD, as.numeric), .SDcols = any_int64]
            }

            setnames(data, names(data), colNames)
            data[, timeId := timeIds]
          }

        }

        if(!is.null(data)){
          data[, c(objectName) := args$id[i]]
          if (!is.null(mcYears)) data[, mcYear := args$mcYear[i]]

          if (!is.null(processFun)) data <- processFun(data)
        }
        data
      },
      .progress = ifelse(showProgress, "text", "none"),
      .parallel = parallel,
      .paropts = list(.packages = "antaresRead")
    )
  }

  rbindlist(res)
}


#' .importOutputForAreas
#'
#' Private function used to import the output for one area.
#'
#' @return
#' a data.table
#'
#' @noRd
#'
.importOutputForAreas <- function(areas, timeStep, select = NULL, mcYears = NULL,
                                  showProgress, opts, parallel, number_of_batches) {

  if (is_api_study(opts)) {

    if (is.null(areas)) {
      return (NULL)
    }

    .download_and_format_api_get_aggregate_areas_result_bulk(areas = areas,
                                                             timeStep = timeStep,
                                                             query_file = "values",
                                                             select = select,
                                                             mcYears = mcYears,
                                                             number_of_batches = number_of_batches,
                                                             opts = opts
                                                            )
  } else {
    suppressWarnings(
      .importOutput("areas", "values", "area", areas, timeStep, select,
                    mcYears, showProgress, opts, parallel = parallel)
    )
  }
}


# Compute the pattern to put in the url to select the desired columns
.compute_pattern_select_url <- function(select, query_file) {
  
  columns_url <- ""
  
  if (!identical(select, "")) {
    if (query_file == "values") {
      columns_url <- paste0("&columns_names=", paste0(select, collapse = ","))
    }
    if (query_file %in% c("details", "details-res", "details-STstorage")) {
      filtered_variables_names <- .filter_referential_output_column_names_by_type(type_res = query_file)
      new_select <- filtered_variables_names[filtered_variables_names$RPACKAGE_DISPLAYED_NAME %in% select,]$OUTPUT_DISPLAYED_NAME
      if (length(new_select) > 0) {
        columns_url <- paste0("&columns_names=", paste0(new_select, collapse = ","))
      }    
    }
  }

  return(columns_url)
}


# Retrieve download_id for aggregated areas raw data from study economy outputs
.api_get_aggregate_areas <- function(areas, timeStep, query_file, select, mcYears, opts) {

  areas_url <- ""
  mc_years_url <- ""

  if (is.null(mcYears)) {
    pattern_endpoint <- "mc-all"
  } else {
    pattern_endpoint <- "mc-ind"
    # MC years file
    if (!identical(mcYears, "")) {
      mc_years_url <- paste0("&mc_years=", paste0(mcYears, collapse = ","))
    }
  }
  
  if (!identical(areas, "")) {
    areas_url <- paste0("&areas_ids=", paste0(areas, collapse = ","))
  }
  
  columns_url <- .compute_pattern_select_url(select = select, query_file = query_file)
  
  endpoint_root <- paste0(opts[["study_id"]], "/areas/aggregate/", pattern_endpoint, "/", opts[["simOutputName"]], "?format=csv")
  endpoint <- paste0(endpoint_root,
                     paste0("&query_file=", query_file),
                     paste0("&frequency=", timeStep),
                     columns_url,
                     areas_url,
                     mc_years_url
                    )

  return(api_get(opts = opts, endpoint = endpoint, default_endpoint = "v1/studies"))
}


.download_and_format_api_get_aggregate_areas_result <- function(areas, timeStep, query_file, select, mcYears, opts) {
  
  download_id <- .api_get_aggregate_areas(areas = areas,
                                          timeStep = timeStep,
                                          query_file = query_file,
                                          select = select,
                                          mcYears = mcYears,
                                          opts = opts
                                         )
  res <- .download_api_aggregate_result(download_id = download_id, opts = opts)
  .format_api_aggregate_result(res = res, type_res = query_file)
}


.download_and_format_api_get_aggregate_areas_result_bulk <- function(areas, timeStep, query_file, select, mcYears, number_of_batches, opts) {
  
  if (is.null(mcYears)) {
    res <- .download_and_format_api_get_aggregate_areas_result(areas = areas, timeStep = timeStep, query_file = query_file, select = select, mcYears = mcYears, opts = opts)
  } else {
    batches <- split_vector_in_equal_parts(x = mcYears, n = number_of_batches) 
    lst_res <- lapply(batches,
                      FUN = .download_and_format_api_get_aggregate_areas_result,
                      areas = areas,
                      timeStep = timeStep,
                      query_file = query_file,
                      select = select,
                      opts = opts
                      )
    res <- do.call("rbind", lst_res)
  }
  
  return(res)
}


#' Retrieve information on a file's state of preparation and retrieve download file.
#'
#' @importFrom data.table fread
#'
#' @param download_id the id of the download.
#' @template opts
#'
#' @return a data.table
.download_api_aggregate_result <- function(download_id, opts) {
  
  default_endpoint <- "v1/downloads"
  
  download_status <- api_get(opts = opts,
                             default_endpoint = default_endpoint,
                             endpoint = paste0(download_id,"/metadata?wait_for_availability=true")
                            )
  response <- api_get(opts = opts,
                      default_endpoint = default_endpoint,
                      endpoint = download_id,
                      parse_result = "text",
                      encoding = "UTF-8"
                     )
  
  if (identical(response, "")) {
    return(data.table())
  } else {
    return(fread(input = response, data.table = TRUE))
  }
     
}


.filter_referential_output_column_names_by_type <- function(type_res){
  
  simulation_variables_names_by_support <- read.table(system.file(
    "format_output","simulation_variables_names_by_support.csv", package = "antaresRead"
    ),
    sep = ";",
    fileEncoding = "UTF-8",
    header = TRUE
  )
  
  return(subset(simulation_variables_names_by_support,DETAILS_FILES_TYPE==type_res))
}


#' .rename_api_aggregate_result_colnames_to_legacy_names
#'
#' Private function used to rename the columns delivered by the endpoint aggregate as the legacy ones.
#'
#' @return
#' a vector
#'
#' @importFrom assertthat assert_that
#'
#' @noRd
#'
.rename_api_aggregate_result_colnames_to_legacy_names <- function(res_colnames, type_res) {
  
  filtered_variables_names <- .filter_referential_output_column_names_by_type(type_res = type_res)
  
  res_cols <- data.frame("cols" = res_colnames,
                         "ORDINAL_POSITION" = seq(res_colnames)
                        )
  
  res_cols <- merge(x = res_cols, 
                    y = filtered_variables_names[,c("OUTPUT_DISPLAYED_NAME", "RPACKAGE_DISPLAYED_NAME")],
                    by.x = "cols",
                    by.y = "OUTPUT_DISPLAYED_NAME",
                    all.x = TRUE
                    )
  res_cols <- res_cols[order(res_cols$ORDINAL_POSITION),]
  res_cols$RPACKAGE_DISPLAYED_NAME[is.na(res_cols$RPACKAGE_DISPLAYED_NAME)] <- res_cols$cols[is.na(res_cols$RPACKAGE_DISPLAYED_NAME)]
  
  return(res_cols$RPACKAGE_DISPLAYED_NAME)
}


#' @importFrom stringi stri_replace_all_regex
.format_api_aggregate_result <- function(res, type_res) {

  if (is.null(res)) {
    return(NULL)
  }
  
  res_cols <- colnames(res)

  cols_to_factor_lower <- c("area", "link", "cluster")
  cols_to_factor_lower <- intersect(cols_to_factor_lower, res_cols)
  #To remove when endpoint will not send uppercase anymore
  res[,(cols_to_factor_lower):=lapply(.SD, tolower), .SDcols=cols_to_factor_lower]
  res[,(cols_to_factor_lower):=lapply(.SD, as.factor), .SDcols=cols_to_factor_lower]

  cols_to_integer <- c("timeId")
  cols_to_integer <- intersect(cols_to_integer, res_cols)
  res[,(cols_to_integer):=lapply(.SD, as.integer), .SDcols=cols_to_integer]

  # Endpoint aggregate does not deliver the legacy column names : OP. COST MIN (endpoint) vs OP. COST_min(legacy)
  pattern <- c(" MIN$", " MAX$", " STD$", " EXP$", " VALUES$")
  replacement <- c("_min", "_max", "_std", "", "")
  new_cols <- stri_replace_all_regex(str = res_cols, pattern = pattern, replacement = replacement, vectorize_all = FALSE)
  
  new_cols <- .rename_api_aggregate_result_colnames_to_legacy_names(res_colnames = new_cols, type_res = type_res)
  
  return(setnames(res, old = res_cols, new = new_cols))
}


.importOutputForDistricts <- function(districts, timeStep, select = NULL, mcYears = NULL,
                                      showProgress, opts, parallel) {
  if (is.null(districts)) return(NULL)

  processFun <- function(dt) {
    dt[, district := as.factor(gsub("^@ ", "", district))]
  }

  suppressWarnings(
    .importOutput("areas", "values", "district", paste("@", districts), timeStep, select,
                  mcYears, showProgress, opts, processFun, parallel = parallel)
  )
}


#' .get_value_columns_details_file
#'
#' Private function used to get the column names for the details-timeStep.txt, details-res-timeStep.txt, or details-STstorage-timeStep.txt.
#' Used in .importOutputForClusters(), .importOutputForResClusters(), and .importOutputForSTClusters()
#' From the opts, we detect which outputs the user decides to take
#'
#' @return
#' a vector
#'
#' @importFrom assertthat assert_that
#'
#' @noRd
#'
.get_value_columns_details_file <- function(opts, type) {

  assert_that(type %in% c("details","details-res","details-STstorage"))

  simulation_variables_names_by_support <- read.table(system.file(
    "format_output","simulation_variables_names_by_support.csv",package="antaresRead"
  ),sep=";",fileEncoding="UTF-8",header = TRUE)

  filtered_variables_names <- subset(simulation_variables_names_by_support,DETAILS_FILES_TYPE==type)
  if (type=="details" && opts$antaresVersion < 830)
    filtered_variables_names <- subset(filtered_variables_names,ANTARES_DISPLAYED_NAME!="Profit by plant")
  filtered_variables_names <- subset(filtered_variables_names,opts[["antaresVersion"]] >= MIN_VERSION | is.na(MIN_VERSION)) 
  # Order is important. There is a correspondance between elements
  ordered_filtered_variables_names <- filtered_variables_names[
    order(filtered_variables_names$ORDINAL_POSITION_BY_TOPIC),
  ]

  all_thematic_variables <- ordered_filtered_variables_names$ANTARES_DISPLAYED_NAME
  colNames <- ordered_filtered_variables_names$RPACKAGE_DISPLAYED_NAME
  
  # With thematic-trimming enabled
  if (opts$parameters$general$`thematic-trimming`) {
    if ("variables selection" %in% names(opts$parameters)) {
      var_selection <- opts$parameters$`variables selection`
      selection_type <- unique(names(var_selection))
      allowed_selection_type <- c("select_var -", "select_var +")
      # Filter the vector to avoid other properties (for example : selected_vars_reset)
      selection_type <- intersect(selection_type, allowed_selection_type)
      # List with a repeated name
      var_selection <- var_selection[which(names(var_selection) == selection_type)]
      selected_variables <- unlist(var_selection, use.names = FALSE)
      # Index of the variables found in the section "variables selection"
      idx_vars <- which(all_thematic_variables %in% selected_variables)
      if (length(idx_vars) > 0) {
        if (selection_type == "select_var -") {
          # vars to remove
          colNames <- colNames[-idx_vars]
        } else if (selection_type == "select_var +") {
          # vars to keep
          colNames <- colNames[idx_vars]
        }
      }
    }
  }

  return(colNames)
}


#' Read output data for clusters if mustRun is disabled
#'
#' @return
#' a data.table
#'
#' @noRd
#'
.importOutputForClusters_wo_mustrun <- function(areas, timeStep, select = NULL, mcYears = NULL,
                                     showProgress, opts, parallel, number_of_batches){

  if (is_api_study(opts)) {

    if (is.null(areas)) {
      return (NULL)
    }
    .download_and_format_api_get_aggregate_areas_result_bulk(areas = areas,
                                                             timeStep = timeStep,
                                                             query_file = "details",
                                                             select = select,
                                                             mcYears = mcYears,
                                                             number_of_batches = number_of_batches,
                                                             opts = opts
                                                             )
  } else {
    reshapeFun <- function(x){
      .reshape_details_file(x,file_type="details",opts=opts)
    }
    suppressWarnings(
      .importOutput("areas", "details", "area", areas, timeStep, NULL,
                    mcYears, showProgress, opts, reshapeFun, sameNames = FALSE,
                    objectDisplayName = "cluster", parallel = parallel)
    )
  }
}


#' Read output data for clusters if modulation is not enabled
#'
#' @return
#' a data.table
#'
#' @noRd
#'
.importOutputForClusters_wo_modulation <- function(areas, timeStep, select = NULL, mcYears = NULL,
                                                   showProgress, opts, reshapeFun, parallel, number_of_batches) {

  if (is_api_study(opts)) {

    if (is.null(areas)) {
      return (NULL)
    }
    res <- .download_and_format_api_get_aggregate_areas_result_bulk(areas = areas,
                                                                    timeStep = timeStep,
                                                                    query_file = "details",
                                                                    select = select,
                                                                    mcYears = mcYears,
                                                                    number_of_batches = number_of_batches,
                                                                    opts = opts
                                                                    )
  } else {
    res <- suppressWarnings(
      suppressWarnings(
        .importOutput("areas", "details", "area", areas, timeStep, NULL,
                      mcYears, showProgress, opts, reshapeFun, sameNames = FALSE,
                      objectDisplayName = "cluster", parallel = parallel)
      )
    )
  }

  res[, mustRunPartial := 0L]
}


#' Read output data for clusters if modulation is enabled
#'
#' @return
#' a data.table
#'
#' @noRd
#'
.importOutputForClusters_w_modulation <- function(areas, timeStep, select = NULL, mcYears = NULL,
                                                  showProgress, opts, reshapeFun, parallel, mod, clusterDesc, number_of_batches) {
  if (timeStep != "hourly") {
    warning('Hourly data will be imported to compute partial must run min(production_t, capacity * minGenModulation_t). These data will be aggregated at the desired `timeStep`. ')

    #copy of warning in ChangeTimeStep
    warning('Aggregation will be perform approximatively because optimization variables in ANTARES are doubles but ANTARES write only integers in TXT files, with this transformation we lose precision. If you want accurate data then you must import the corresponding data with `readAntares`')

    messageWarningMcYears <- paste0("When mcYears is set to all or NULL : ", mcYears, " and timeStep is set to : " ,timeStep , " result for mustRun are not accurate. Hourly `synthetic` or `details` results will be aggregated at the desired `timeStep`.  " )

    if (is.null(mcYears)) {
      warning(messageWarningMcYears, call. = FALSE)
    } else if (is.character(mcYears)) {
      if (mcYears == "all") {
        warning(messageWarningMcYears, call. = FALSE)
      }
    } else if (length(mcYears) > 1) {
      warning(messageWarningMcYears, call. = FALSE)
    }

    args <- .generate_output_data_to_read(folder = "areas", fileName = "details", ids = areas, timeStep = "hourly", mcYears = mcYears, opts = opts)
    outputMissing <- .check_missing_output_files(opts = opts, args = args)

    if (any(outputMissing)) {
      stop("Some hourly data is not available for some areas.")
    }
  }

  mod[is.na(minGenModulation), minGenModulation := 0]

  .mergeByRef(mod, clusterDesc)
  mod[, mustRunPartial := minGenModulation * capacity]

  setkey(mod, area, cluster, timeId)

  if (is_api_study(opts)) {

    if (is.null(areas)) {
      return (NULL)
    }
    res <- .download_and_format_api_get_aggregate_areas_result_bulk(areas = areas,
                                                                    timeStep = timeStep,
                                                                    query_file = "details",
                                                                    select = select,
                                                                    mcYears = mcYears,
                                                                    number_of_batches = number_of_batches,
                                                                    opts = opts
                                                                    )
    mustRunPartial <- mod[J(res$area, res$cluster, res$timeId), mustRunPartial]
    res[, mustRunPartial := pmin(production, mustRunPartial)]
    res <- changeTimeStep(res, timeStep, "hourly", fun = "sum", opts = opts)
  } else {

    processFun <- function(x) {
      x <- reshapeFun(x)
      mustRunPartial <- mod[J(x$area, x$cluster, x$timeId), mustRunPartial]
      x[, mustRunPartial := pmin(production, mustRunPartial)]
      changeTimeStep(x, timeStep, "hourly", fun = "sum", opts = opts)
    }

    res <- suppressWarnings(
      .importOutput("areas", "details", "area", areas, "hourly", NULL,
                    mcYears, showProgress, opts, processFun,
                    sameNames = FALSE, objectDisplayName = "cluster",
                    parallel = parallel)
    )
  }

  return(res)
}


#' Read output data for clusters if mustRun is enabled
#'
#' @return
#' a data.table
#'
#' @noRd
#'
.importOutputForClusters_w_mustrun <- function(areas, timeStep, select = NULL, mcYears = NULL,
                                               showProgress, opts, parallel, number_of_batches) {

  reshapeFun <- function(x){
    .reshape_details_file(x,file_type="details",opts=opts)
  }

  # The partial must run for a cluster is defined as:
  # sum_t(min(production_t, capacity * minGenModulation_t))
  # This formula is non-linear, so if we need to get hourly data to compute
  # it.
  # To avoid importing large amount of data, we first check if minGenModulation
  # is non null for at least one cluster.
  # If we have to use hourly data, we aggregate it directly at the desired
  # timestep to limit the amount of RAM required.

  # Get cluster capacity and must run mode
  clusterDesc <- readClusterDesc(opts)

  if (is.null(clusterDesc[["must.run"]])) {
    clusterDesc[["must.run"]] <- FALSE
  }
  clusterDesc[is.na(must.run), must.run := FALSE]

  if (is.null(clusterDesc[["min.stable.power"]])) {
    clusterDesc[["min.stable.power"]] <- 0
  }
  clusterDesc[is.na(min.stable.power), min.stable.power := 0]

  clusterDesc <- clusterDesc[, .(area, cluster,
                                 capacity = nominalcapacity * unitcount,
                                 min.stable.power,
                                 `must.run`)]

  # Are clusters in partial must run mode ?
  mod <- llply(areas, .importThermalModulation, opts = opts, timeStep = "hourly")
  mod <- rbindlist(mod)

  # Should we compute the partial must run ?
  if (is.null(mod$minGenModulation) || all(is.na(mod$minGenModulation) | mod$minGenModulation == 0)) {
    # We should not \o/
    res <- .importOutputForClusters_wo_modulation(areas, timeStep, select, mcYears, showProgress, opts, reshapeFun, parallel, number_of_batches = number_of_batches)
  } else {
    # Worst case ! We have to !
    res <- .importOutputForClusters_w_modulation(areas, timeStep, select, mcYears, showProgress, opts, reshapeFun, parallel, mod, clusterDesc, number_of_batches = number_of_batches)
  }

  .mergeByRef(res, clusterDesc[,.(area, cluster, must.run, min.stable.power)])

  if (is.null(res$NODU)) {
    res[, thermalPmin := 0]
  } else {
    res[, thermalPmin := min.stable.power * NODU]
  }

  res[, `:=`(
    mustRun = production * must.run,
    mustRunTotal = production * must.run + mustRunPartial,
    must.run = NULL,
    min.stable.power = NULL
  )]

  res[, thermalPmin := pmax(thermalPmin, mustRunTotal)]

  return(res)
}


#' .importOutputForClusters
#'
#' Private function used to import the output for the thermal clusters of one area
#'
#' @return
#' a data.table
#'
#' @noRd
#'
.importOutputForClusters <- function(areas, timeStep, select = NULL, mcYears = NULL,
                                     showProgress, opts, mustRun = FALSE, parallel, number_of_batches) {

  if (!mustRun) {
    .importOutputForClusters_wo_mustrun(areas = areas, timeStep = timeStep, select = select, mcYears = mcYears,
                                       showProgress = showProgress, opts = opts, parallel = parallel, number_of_batches = number_of_batches)
  } else {
    .importOutputForClusters_w_mustrun(areas = areas, timeStep = timeStep, select = select, mcYears = mcYears,
                                       showProgress = showProgress, opts = opts, parallel = parallel, number_of_batches = number_of_batches)
  }
}


#' .reshape_details_file
#'
#' In output files, there is one file per area with the follwing form:
#' cluster1-var1 | cluster2-var1 | cluster1-var2 | cluster2-var2
#' the following function reshapes the result to have variable cluster in column.
#' To improve greatly the performance we use our knowledge of the position of
#' the columns instead of using more general functions like dcast.
#'
#' @return
#' a data.table
#'
#' @noRd
#'
.reshape_details_file <- function(x,file_type,opts) {

  # Get cluster names
  n <- names(x)
  idx <- ! n %in% pkgEnv$idVars
  clusterNames <- tolower(unique(n[idx]))

  # Id vars names
  idVarsId <- which(!idx)
  idVarsNames <- n[idVarsId]

  # Column names of the output table
  colNames <- .get_value_columns_details_file(opts=opts,type=file_type)

  # Loop over clusters
  nclusters <- length(clusterNames)

  res <- llply(1:nclusters, function(i) {
    dt <- x[, c(nclusters * 0:(length(colNames) - 1) + i, idVarsId), with = FALSE]
    setnames(dt, c(colNames, idVarsNames))
    dt[, cluster := as.factor(clusterNames[i])]
    dt
  })

  rbindlist(res)
}


#' .importOutputForResClusters
#'
#' Private function used to import the output for the renewable clusters of one area
#'
#' @return
#' a data.table
#'
#' @noRd
#'
.importOutputForResClusters <- function(areas, timeStep, select = NULL, mcYears = NULL,
                                        showProgress, opts, parallel, number_of_batches) {

  if (is_api_study(opts)) {

    if (is.null(areas)) {
      return (NULL)
    }
    .download_and_format_api_get_aggregate_areas_result_bulk(areas = areas,
                                                             timeStep = timeStep,
                                                             query_file = "details-res",
                                                             select = select,
                                                             mcYears = mcYears,
                                                             number_of_batches = number_of_batches,
                                                             opts = opts
                                                             )
  } else {
    reshapeFun <- function(x) {
      .reshape_details_file(x,file_type="details-res",opts=opts)
    }

    suppressWarnings(
      .importOutput("areas", "details-res", "area", areas, timeStep, NULL,
                    mcYears, showProgress, opts, reshapeFun, sameNames = FALSE,
                    objectDisplayName = "clustersRe", parallel = parallel)
    )
  }
}


#' .importOutputForSTClusters
#'
#' Private function used to import the output for the short-term clusters of one area
#'
#' @return
#' a data.table
#'
#' @noRd
#'
.importOutputForSTClusters <- function(areas, timeStep, select = NULL, mcYears = NULL,
                                        showProgress, opts, parallel, number_of_batches) {

  if (is_api_study(opts)) {

    if (is.null(areas)) {
      return (NULL)
    }
    .download_and_format_api_get_aggregate_areas_result_bulk(areas = areas,
                                                             timeStep = timeStep,
                                                             query_file = "details-STstorage",
                                                             select = select,
                                                             mcYears = mcYears,
                                                             number_of_batches = number_of_batches,
                                                             opts = opts
                                                             )
  } else {
    reshapeFun <- function(x) {
      .reshape_details_file(x,file_type="details-STstorage",opts=opts)
    }

    suppressWarnings(
      .importOutput("areas", "details-STstorage", "area", areas, timeStep, NULL,
                    mcYears, showProgress, opts, reshapeFun, sameNames = FALSE,
                    objectDisplayName = "clustersST", parallel = parallel)
    )
  }
}


#' .importOutputForBindingConstraints
#'
#' Private function used to import the output for binding constraints.
#'
#' @return
#' a data.table
#'
#' @noRd
#'
.importOutputForBindingConstraints <- function(timeStep, mcYears = NULL,
                                  showProgress, opts, parallel,
                                  sameNames = T, processFun = NULL) {
  folder <- "binding_constraints"
  fileName <- "binding-constraints"
  objectName <- "bindingConstraint"

  if (showProgress) cat("Importing binding constraints\n")

  if (is.null(mcYears)) {
    args <- expand.grid(id = "bindingConstraints")
    args$path <- sprintf("%s/mc-all/%s/%s-%s.txt",
                         opts$simDataPath, folder, fileName, timeStep)
  } else {
    args <- expand.grid(id = "bindingConstraints", mcYear = mcYears)
    args$path <- sprintf("%s/mc-ind/%05.0f/%s/%s-%s.txt",
                         opts$simDataPath, args$mcYear, folder, fileName, timeStep)
  }

  if(opts$typeLoad == "api"){
    # args$path <- sapply(args$path, .changeName, opts = opts)
    # outputMissing <- unlist(sapply(args$path, function(X)httr::HEAD(X)$status_code!=200))
    # print(outputMissing)
    outputMissing <- rep(FALSE, nrow(args))
  }else{
    outputMissing <- !file.exists(args$path)
  }
  if (all(outputMissing)) {
    message("No data corresponding to your query.")
    return(NULL)
  } else if (any(outputMissing)) {
    message("Some requested output files are missing.")
    args <- args[!outputMissing, ]
  }

  # columns to retrieve
  api = "api" %in% opts$typeLoad
  colNames <- .getOutputHeader(
    args$path[1], objectName, api = api,
    token = opts$token, timeout = opts$timeout, config = opts$httr_config
  )

  # read all columns except the time variables that will be recreated
  selectCol <- which(!colNames %in% pkgEnv$idVars)
  colNames <- colNames[selectCol]

  # time ids
  if (timeStep == "annual") {
    timeIds <- 1L
  } else {
    timeRange <- .getTimeId(c(opts$timeIdMin, opts$timeIdMax), timeStep, opts)
    timeIds <- seq(timeRange[1], timeRange[2])
  }


  if(!is.null((getDefaultReactiveDomain())))
  {
    n <- nrow(args)
    withProgress(message = 'antaresRead', value = 0, {
      res <- llply(
        1:nrow(args),
        function(i) {
          incProgress(1/n, detail = paste0("Importing ", folder, " data"))
          data <- NULL
          try({

            if (length(selectCol) == 0) {
              if(opts$typeLoad != "api"){
                data <- data.table(timeId = timeIds)
              } else {
                data <- NULL
              }
            } else {
              data <- fread_antares(opts = opts, file = args$path[i],
                                    sep = "\t", header = F, skip = 7,
                                    select = selectCol, integer64 = "numeric",
                                    na.strings = "N/A")

              if(!is.null(data)){
                # fix data.table bug on integer64
                any_int64 <- colnames(data)[which(sapply(data, function(x) "integer64" %in% class(x)))]
                if(length(any_int64) > 0){
                  data[, c(any_int64) := lapply(.SD, as.numeric), .SDcols = any_int64]
                }

                setnames(data, names(data), colNames)
                data[, timeId := timeIds]
              }
            }

            if(!is.null(data)){
              if (!is.null(mcYears)) data[, mcYear := args$mcYear[i]]
              if (!is.null(processFun)) data <- processFun(data)
            }
            data
          })
          data
        },
        .progress = ifelse(showProgress, "text", "none"),
        .parallel = parallel,
        .paropts = list(.packages = "antaresRead")
      )
    })

  }else{
    res <- llply(
      1:nrow(args),
      function(i) {

        if (length(selectCol) == 0) {
          if(opts$typeLoad != "api"){
            data <- data.table(timeId = timeIds)
          } else {
            data <- NULL
          }
        } else {
          data <- fread_antares(opts = opts, file = args$path[i],
                                sep = "\t", header = F, skip = 7,
                                select = selectCol, integer64 = "numeric",
                                na.strings = "N/A", showProgress = FALSE)

          if(!is.null(data)){
            # fix data.table bug on integer64
            any_int64 <- colnames(data)[which(sapply(data, function(x) "integer64" %in% class(x)))]
            if(length(any_int64) > 0){
              data[, c(any_int64) := lapply(.SD, as.numeric), .SDcols = any_int64]
            }

            setnames(data, names(data), colNames)
            data[, timeId := timeIds]
          }

        }

        if(!is.null(data)){
          if (!is.null(mcYears)) data[, mcYear := args$mcYear[i]]
          if (!is.null(processFun)) data <- processFun(data)
        }
        data
      },
      .progress = ifelse(showProgress, "text", "none"),
      .parallel = parallel,
      .paropts = list(.packages = "antaresRead")
    )
  }

  rbindlist(res)
}


#' .importOutputForLinks
#'
#' Private function used to import the output of one link.
#'
#' @return
#' a data.table
#'
#' @noRd
#'
.importOutputForLinks <- function(links, timeStep, select = NULL, mcYears = NULL,
                                  showProgress, opts, parallel, number_of_batches) {

  if (is_api_study(opts)) {

    if (is.null(links)) {
      return (NULL)
    }
    .download_and_format_api_get_aggregate_links_result_bulk(links = links,
                                                             timeStep = timeStep,
                                                             select = select,
                                                             mcYears = mcYears,
                                                             number_of_batches = number_of_batches,
                                                             opts = opts
                                                             )
  } else {
    suppressWarnings(
      .importOutput("links", "values", "link", links, timeStep, select,
                    mcYears, showProgress, opts, parallel = parallel)
    )
  }
}


# Retrieve download_id for aggregated links raw data from study economy outputs
.api_get_aggregate_links <- function(links, timeStep, select, mcYears, opts) {

  if (is.null(links)) {
    return(NULL)
  }
  
  links_url <- ""
  columns_url <- ""
  mc_years_url <- ""
  
  if (is.null(mcYears)) {
    pattern_endpoint <- "mc-all"
  } else {
    pattern_endpoint <- "mc-ind"
    # MC years file
    if (!identical(mcYears, "")) {
      mc_years_url <- paste0("&mc_years=", paste0(mcYears, collapse = ","))
    }
  }

  # link
  if (!identical(links, "")) {
    links_url <- paste0("&links_ids=", paste0(links, collapse = ","))
  }

  # columns
  if (!identical(select, "")) {
    columns_url <- paste0("&columns_names=", paste0(select, collapse = ","))
  }
  
  endpoint_root <- paste0(opts[["study_id"]], "/links/aggregate/", pattern_endpoint, "/", opts[["simOutputName"]], "?format=csv&query_file=values")
  endpoint <- paste0(endpoint_root,
                     paste0("&frequency=", timeStep),
                     columns_url,
                     links_url,
                     mc_years_url
                    )
  
  return(api_get(opts = opts, endpoint = endpoint, default_endpoint = "v1/studies"))
}


.download_and_format_api_get_aggregate_links_result <- function(links, timeStep, select, mcYears, opts) {
  
  download_id <- .api_get_aggregate_links(links = links,
                                          timeStep = timeStep,
                                          select = select,
                                          mcYears = mcYears,
                                          opts = opts
                                         )
  res <- .download_api_aggregate_result(download_id = download_id, opts = opts)
  .format_api_aggregate_result(res = res, type_res = "links")
}


.download_and_format_api_get_aggregate_links_result_bulk <- function(links, timeStep, select, mcYears, number_of_batches, opts) {
  
  if (is.null(mcYears)) {
    res <- .download_and_format_api_get_aggregate_links_result(links = links, timeStep = timeStep, select = select, mcYears = mcYears, opts = opts)
  } else {
    batches <- split_vector_in_equal_parts(x = mcYears, n = number_of_batches) 
    lst_res <- lapply(batches,
                      FUN = .download_and_format_api_get_aggregate_links_result,
                      links = links,
                      timeStep = timeStep,
                      select = select,
                      opts = opts
                      )
    res <- do.call("rbind", lst_res)
  }
  
  return(res)
}

# The two following functions read input time series that are eventually
# stored in output and rebuild the actual time series used in each Monte-Carlo
# simulation

.importThermal <- function(area, synthesis, timeStep, mcYears, opts, ...) {
  if (!area %in% opts$areasWithClusters) return(NULL)
  if (synthesis) mcYears <- opts$mcYears

  # browser()
  # Path to the files containing the IDs of the time series used for each
  # Monte-Carlo years.
  pathTSNumbers <- file.path(opts$simPath, "ts-numbers/thermal")

  # Path to the time series. Ideally, time series are stored in output. If it is
  # not the case, read the series in the input.
  pathInput <- file.path(opts$simPath, "ts-generator/thermal/mc-0")

  if(!"api" %in% opts$typeLoad){
    if (dir.exists(pathInput)) {
      filePattern <- sprintf("%s/%s/%%s.txt", pathInput, area)
    } else {
      pathInput <- file.path(opts$inputPath, "thermal/series")
      filePattern <- sprintf("%s/%s/%%s/series.txt", pathInput, area)
    }

    # Read the Ids of the time series used in each Monte-Carlo Scenario.
    cls <- list.files(file.path(pathTSNumbers, area))
    if (length(cls) == 0) return(NULL)

    nameCls <- gsub(".txt", "", cls)

    tsIds <- llply(cls, function(cl) {
      as.numeric(readLines(file.path(pathTSNumbers, area, cl))[-1])
    })

    names(tsIds) <- nameCls

  } else {
    gen_check <- .getSuccess(
      file.path(opts$simPath, "ts-generator/hydro/mc-0"),
      token = opts$token, timeout = opts$timeout, config = opts$httr_config
    )

    if (gen_check) {
      filePattern <- sprintf("%s/%s/%%s.txt", pathInput, area)
    } else {
      pathInput <- file.path(opts$inputPath, "thermal/series")
      filePattern <- sprintf("%s/%s/%%s/series.txt", pathInput, area)
    }

    # Read the Ids of the time series used in each Monte-Carlo Scenario.
    cls <- names(
      read_secure_json(file.path(pathTSNumbers, area), token = opts$token,
                       timeout = opts$timeout, config = opts$httr_config)
    )
    if (length(cls) == 0) return(NULL)

    nameCls <- cls

    tsIds <- lapply(
      X = cls,
      FUN = function(cl) {
        # this changed because the api now already return a proper array
        as.numeric(
          read_secure_json(file.path(pathTSNumbers, area, cl), token = opts$token,
                           timeout = opts$timeout, config = opts$httr_config)
        )
      }
    )
    names(tsIds) <- nameCls
    tsIds <- tsIds[vapply(tsIds, length, FUN.VALUE = integer(1)) > 0]

  }

  # Two nested loops: clusters, Monte Carlo simulations.
  series <- ldply(names(tsIds), function(cl) {
    ids <- tsIds[[cl]][mcYears]
    colToRead <- sort(unique(ids)) # Columns to read in the ts file
    colIds <- sapply(ids, function(i) which(colToRead == i)) # correspondance between the initial ids and the columns in the generated table

    # ts <- fread(sprintf(filePattern, cl), integer64 = "numeric", select = colToRead)
    ts <- fread_antares(opts = opts, file = sprintf(filePattern, cl), integer64 = "numeric", select = colToRead)
    if (is.null(ts)) {
      return(NULL)
    }

    ldply(seq_along(ids), function(i) {
      data.frame(
        area = area,
        cluster = cl,
        mcYear = mcYears[i],
        timeId = 1:nrow(ts),
        thermalAvailability = ts[[ colIds[i] ]]
      )
    })
  })

  series <- as.data.table(series)

  series <- series[timeId %in% opts$timeIdMin:opts$timeIdMax]

  # Compute the number of available units
  clusterDesc <- readClusterDesc(opts)
  series <- merge(series, clusterDesc[, .(area, cluster, nominalcapacity)],
                  by = c("area", "cluster"))
  .mergeByRef(series, clusterDesc, on = c("area", "cluster"), "nominalcapacity")

  series[, availableUnits :=  ceiling(thermalAvailability / nominalcapacity)]
  series[, nominalcapacity := NULL]

  # Aggregation
  res <- changeTimeStep(series, timeStep, "hourly", opts=opts, fun = c("sum", "mean"))

  if (synthesis) {
    res <- res[, .(thermalAvailability=mean(thermalAvailability),
                   availableUnits = mean(availableUnits)),
               keyby = .(area, cluster, timeId)]
  }

  res
}

.importHydroStorage <- function(area, synthesis, timeStep, mcYears, opts, ...) {
  if (synthesis) mcYears <- opts$mcYears

  pathTSNumbers <- file.path(opts$simPath, "ts-numbers/hydro")


  # Read the Ids of the time series used in each Monte-Carlo Scenario.
  if(!"api" %in% opts$typeLoad){
    tsIds <- as.numeric(readLines(file.path(pathTSNumbers, paste0(area, ".txt")))[-1])
    tsIds <- tsIds[mcYears]

    # Input time series
    pathInput <- file.path(opts$simPath, "ts-generator/hydro/mc-0")

    if (dir.exists(pathInput)) {
      f <- file.path(pathInput, area, "storage.txt")
    } else {
      pathInput <- file.path(opts$inputPath, "hydro/series")
      f <- file.path(pathInput, area, "mod.txt")
    }
  } else {
    tsIds <- as.numeric(strsplit(
      read_secure_json(file.path(pathTSNumbers, area), token = opts$token,
                       timeout = opts$timeout, config = opts$httr_config),
      "\n")[[1]][-1]
    )
    tsIds <- tsIds[mcYears]

    gen_check <- .getSuccess(file.path(opts$simPath, "ts-generator/hydro/mc-0"),
                             token = opts$token, timeout = opts$timeout, config = opts$httr_config
    )
    if (gen_check) {
      f <- file.path(opts$simPath, "ts-generator/hydro/mc-0", area, "storage.txt")
    } else {
      pathInput <- file.path(opts$inputPath, "hydro/series")
      f <- file.path(pathInput, area, "mod.txt")
    }
  }


  if(opts$antaresVersion >= 700){
    timeRange <- range(.getTimeId(opts$timeIdMin:opts$timeIdMax, "daily", opts))
  }else {
    timeRange <- range(.getTimeId(opts$timeIdMin:opts$timeIdMax, "monthly", opts))
  }

  if (!"api" %in% opts$typeLoad && file.size(f) == 0) {
    series <- ldply(1:length(tsIds), function(i) {
      data.frame(
        area = area,
        mcYear = mcYears[i],
        timeId = timeRange[1]:timeRange[2],
        hydroStorage = rep(0L, length(timeRange[1]:timeRange[2]))
      )
    })
  } else {
    colToRead <- sort(unique(tsIds)) # Columns to read in the ts file
    colIds <- sapply(tsIds, function(i) which(colToRead == i)) # link between the initial ids and the columns in the generated table


    # ts <- fread(f, integer64 = "numeric", select = colToRead)
    ts <- fread_antares(opts = opts, file = f, integer64 = "numeric", select = colToRead)

    if ("api" %in% opts$typeLoad && is.null(ts)) {
      series <- ldply(1:length(tsIds), function(i) {
        data.frame(
          area = area,
          mcYear = mcYears[i],
          timeId = timeRange[1]:timeRange[2],
          hydroStorage = rep(0L, length(timeRange[1]:timeRange[2]))
        )
      })
    } else {
      ts <- ts[timeRange[1]:timeRange[2]]

      series <- ldply(1:length(tsIds), function(i) {
        data.frame(
          area = area,
          mcYear = mcYears[i],
          timeId = timeRange[1]:timeRange[2],
          hydroStorage = ts[[ colIds[i] ]]
        )
      })
    }

  }

  series <- data.table(series)


  if(opts$antaresVersion >= 700){
    res <- changeTimeStep(series, timeStep, "daily", opts=opts)
  }else {
    res <- changeTimeStep(series, timeStep, "monthly", opts=opts)
  }

  if (synthesis) {
    res <- res[, .(hydroStorage=mean(hydroStorage)), keyby = .(area, timeId)]
  }

  res

}
