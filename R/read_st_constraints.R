
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
#' 

read_storages_constraints <- function(opts=simOptions()){
  assertthat::assert_that(inherits(opts, "simOptions"))
  stopifnot(opts$antaresVersion>=920)

  ##
  # API bloc
  ##
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # JSON -> data.table (1 ligne par occurrence)
  .st_additional_constraints_to_datatable <- function(body_json) {
    if (length(body_json) == 0) return(data.table())
    
    rows <- lapply(names(body_json), function(key) {
      item  <- body_json[[key]]
      parts <- strsplit(key, " / ", fixed = TRUE)[[1]]
      area       <- parts[1] %||% NA_character_
      cluster    <- parts[2] %||% NA_character_
      constraint <- parts[3] %||% NA_character_
      
      occs <- item$occurrences
      if (length(occs) == 0) {
        data.table(
          area, cluster = tolower(cluster), constraint,
          variable = item$variable %||% NA_character_,
          operator = item$operator %||% NA_character_,
          enabled  = isTRUE(item$enabled),
          hours = list(integer())
        )
      } else {
        rbindlist(lapply(occs, function(occ) {
          data.table(
            area, cluster = tolower(cluster), constraint,
            variable = item$variable %||% NA_character_,
            operator = item$operator %||% NA_character_,
            enabled  = isTRUE(item$enabled),
            hours = list(as.integer(occ$hours))
          )
        }), fill = TRUE)
      }
    })
    
    dt <- rbindlist(rows, fill = TRUE)
    dt[, cluster := as.factor(cluster)]
    dt[]
  }
  
  # ---- API bloc (plus AUCUNE référence à 'dir') ----
  table_type <- "st-storages-additional-constraints"
  
  if (is_api_study(opts = opts)) {

    body_json <- api_get(
      opts = opts,
      endpoint = paste0(opts$study_id, "/table-mode/", table_type),
      query = list(columns = "")
    )
    dt_clusters <- .st_additional_constraints_to_datatable(body_json)
      # Lit TOUTES les rhs_* sous input/st-storage/constraints/<area>/<cluster>/,
      # et renvoie les fichiers tels que lus par fread_antares (brut).
    importSTConstraints_API <- function(opts, verbose = FALSE) {
      #retire les “/” de fin d’une chaîne
      trim_slash <- function(x) sub("/+$", "", x)
      api_base   <- trim_slash(opts$inputPath)
      
      # récupère les noms (areas, clusters, fichiers) depuis l'API (liste nommée,vecteur de chaînes ou liste d’objets )
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
      
      # URL complète /raw?path=/input/<rel>
      make_raw_url <- function(rel) {
        paste0(api_base, "/", rel, "&formatted=FALSE")
      }
      
      # ---- lecture brute de TOUTES les rhs_* ----
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
            #if (verbose) message("GET ", url)
            
            dt_raw <- antaresRead:::fread_antares(
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
      # Utilisation
      st_constraints_ts <- importSTConstraints_API(opts, verbose = FALSE)
      return(list(
        dt_clusters      = data.table::as.data.table(dt_clusters),
        st_constraints_ts = st_constraints_ts
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
