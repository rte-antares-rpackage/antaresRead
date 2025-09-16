
utils::globalVariables(c("cluster_name", "full_path"))

# RULES :
# read only written files values
# no default values for TS => return data.table() empty

#' @title Read Short-term storages / additional constraints
#' @description
#' `r antaresRead:::badge_api_ok()`
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
  # =========================
  # DIRECTLY transforms:
  #   - body_json (api_get table-mode)
  #   - st_constraints_ts (importSTConstraints_API)
  # into structure:
  # full_st_constraints -> area -> cluster -> {properties, values}
  # =========================

  `%||%` <- function(x, y) if (is.null(x)) y else x

  # returns a list of hour vectors (one entry per occurrence)
  .collect_hours_lists <- function(occs) {
    if (length(occs) == 0) return(list())
    lapply(occs, function(oc) as.integer(oc$hours %||% integer()))
  }

  .to_bracket_string <- function(v) paste0("[", paste(v, collapse = ", "), "]")

  # extract a numeric vector (priority to column V1) for rhs_*
  .extract_rhs_vector <- function(dt_raw) {
    if (is.null(dt_raw)) return(numeric())
    if ("V1" %in% names(dt_raw)) return(as.numeric(dt_raw$V1))
    num_cols <- names(dt_raw)[vapply(dt_raw, is.numeric, logical(1))]
    if (length(num_cols)) return(as.numeric(dt_raw[[ num_cols[1] ]]))
    as.numeric(dt_raw[[1]])
  }

  # --------------- main builder ----------------
  # body_json : named list "area / cluster / constraint" -> list(variable, operator, occurrences, enabled)
  # st_constraints_ts : named list "area/cluster/rhs_name" -> data.table (values)
  # hours_format :
  #   - "list_string"
  #   - "list_vector"
  build_full_st_constraints_from_json <- function(
    body_json,
    st_constraints_ts,
    hours_format = c("list_string", "list_vector")
  ) {
    hours_format <- match.arg(hours_format)
    out <- list()

    # ---- properties from body_json (without using data.table) ----
    if (length(body_json) && !is.null(names(body_json))) {
      for (key in names(body_json)) {
        item  <- body_json[[key]]
        parts <- strsplit(key, " / ", fixed = TRUE)[[1]]
        area       <- parts[1] %||% NA_character_
        cluster    <- parts[2] %||% NA_character_
        constraint <- parts[3] %||% NA_character_

        if (is.null(out[[area]])) out[[area]] <- list()
        if (is.null(out[[area]][[cluster]])) out[[area]][[cluster]] <- list(properties = list(), values = list())

        hrs_lists <- .collect_hours_lists(item$occurrences)
        hrs_out <- switch(
          hours_format,
          list_string = vapply(hrs_lists, .to_bracket_string, character(1)),
          list_vector = hrs_lists
        )

        out[[area]][[cluster]]$properties[[constraint]] <- list(
          variable = item$variable %||% NA_character_,
          operator = item$operator %||% NA_character_,
          hours    = hrs_out,
          enabled  = isTRUE(item$enabled)
        )
      }
    }

    # ---- values from st_constraints_ts (rhs_*) ----
    ks <- names(st_constraints_ts %||% list())
    if (length(ks)) {
      for (k in ks) {
        parts <- strsplit(k, "/", fixed = TRUE)[[1]]  # "area/cluster/file"
        area    <- parts[1] %||% NA_character_
        cluster <- parts[2] %||% NA_character_
        file    <- parts[3] %||% NA_character_

        if (is.null(out[[area]])) out[[area]] <- list()
        if (is.null(out[[area]][[cluster]])) out[[area]][[cluster]] <- list(properties = list(), values = list())

        v <- .extract_rhs_vector(st_constraints_ts[[k]])
        out[[area]][[cluster]]$values[[file]] <- list(V1 = v)
      }
    }

    return( out)
  }

  # ---- API block ----
  table_type <- "st-storages-additional-constraints"

  if (is_api_study(opts = opts)) {

    body_json <- api_get(
      opts = opts,
      endpoint = paste0(opts$study_id, "/table-mode/", table_type),
      query = list(columns = "")
    )
    # Reads ALL rhs_* under input/st-storage/constraints/<area>/<cluster>/,
    # and returns the files as read by fread_antares
    importSTConstraints_API <- function(opts, verbose = FALSE) {
      # remove trailing slashes
      trim_slash <- function(x) sub("/+$", "", x)
      api_base   <- trim_slash(opts$inputPath)

      # list names (areas, clusters, files) from the API (named list, character vector or list of objects)
      ls_names <- function(path) {
        x <- read_secure_json(path, token = opts$token,
                              timeout = opts$timeout, config = opts$httr_config)
        if (is.null(x)) return(character(0))
        nms <- names(x); if (!is.null(nms) && length(nms)) return(nms)
        if (is.character(x)) return(x)
        if (is.list(x) && length(x)) {
          has <- vapply(x, function(e) !is.null(e[["name"]]), logical(1))
          if (all(has)) return(vapply(x, `[[`, character(1), "name"))
        }
        character(0)
      }

      # full URL /raw?path=/input/<rel>
      make_raw_url <- function(rel) {
        paste0(api_base, "/", rel, "&formatted=FALSE")
      }

      # ---- raw reading of ALL rhs_* ----
      res <- list()

      areas <- tolower(ls_names(paste0(api_base, "/st-storage/constraints")))
      for (a in areas) {
        clusters <- tolower(ls_names(paste0(api_base, "/st-storage/constraints/", a)))
        for (cl in clusters) {
          files <- ls_names(paste0(api_base, "/st-storage/constraints/", a, "/", cl))
          rhs   <- grep("^rhs_", files, value = TRUE, ignore.case = TRUE)
          for (f in rhs) {
            rel <- paste("st-storage/constraints", a, cl, f, sep = "/")
            url <- make_raw_url(rel)
            dt_raw <- fread_antares(
              opts = opts, file = url, integer64 = "numeric",
              header = FALSE, showProgress = FALSE
            )
            dt_raw
            res[[paste(a, cl, f, sep = "/")]] <- dt_raw
          }
        }
      }

      res
    }

      st_constraints_ts <- importSTConstraints_API(opts, verbose = FALSE)
      return(res <- build_full_st_constraints_from_json(
        body_json,
        st_constraints_ts,
        hours_format = "list_string"
      ))
  }

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
