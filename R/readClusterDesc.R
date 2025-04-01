#Copyright © 2016 RTE Réseau de transport d’électricité

#' Import clusters description
#'
#' @description 
#' This function reads in the input files of an antares study the
#' properties of each cluster. 
#' 
#' Be aware that clusters descriptions are read
#' in the input files so they may have changed since a simulation has been run.
#'
#' @inheritParams readAntares
#' @param dot_format `logical` default TRUE to return `character` with "valid" format (see [make.names()]) 
#'
#' @section Warning:  
#' You have now two format output to display input properties. 
#' Default is format uses by operating team, eg `min.down.time`.
#' Other format is according to antares simulator, eg `min-down-time`.  
#' 
#' All properties are returned with default values according to Antares Study version.
#'
#' @return
#' A `data.table` with one line per cluster.  
#' 
#' Columns are displayed using the 3 key columns (*area*, *cluster*, *group*). 
#' The rest of the properties are displayed according to cluster type 
#' ("thermal", "renewable" or "st-storages").
#' 
#' key columns:
#' 
#' \item{area}{Name of the area containing the cluster}
#' \item{cluster}{Name of the cluster}
#' \item{group}{Type of cluster (gaz, nuclear, etc.)}
#' 
#' 
#' By default, the function reads the cluster description of the default antares
#' study. You can use the argument \code{opts} to specify another study.
#' 
#' \code{readClusterDesc} : read thermal clusters
#' 
#' \code{readClusterResDesc} : read renewable clusters (Antares >= V8.1)
#' 
#' \code{readClusterSTDesc} : read st-storage clusters (Antares >= V8.6)
#' 
#' If you have no clusters properties, `Null data.table (0 rows and 0 cols)` is returned.
#' 
#'
#' @examples
#' 
#' \dontrun{
#' 
#' # Default format with "dot separator"
#' 
#' # thermal
#' readClusterDesc()
#' 
#' # renewable
#' readClusterResDesc()
#' 
#' # st-storage
#' readClusterSTDesc()
#' 
#' # Antares Simulator format 
#' 
#' #' # thermal
#' readClusterDesc(dot_format = FALSE)
#' 
#' # renewable
#' readClusterResDesc(dot_format = FALSE)
#' 
#' # st-storage
#' readClusterSTDesc(dot_format = FALSE)
#' 
#' 
#' # By default, the function reads cluster descriptions for the default study,
#' # but it is possible to specify another study with parameter "opts"
#' sim1 <- setSimulationPath()
#' 
#' #[... code that modifies the default antares study]
#' 
#' readClusterDesc(sim1)
#' 
#' }
#' 
#' @export
#' 
#' @rdname readClusterDesc
readClusterDesc <- function(opts = simOptions(), dot_format = TRUE) {
  .readClusterDesc(opts = opts, 
                   dir = "thermal/clusters",
                   dot_format = dot_format)
}

#' @export
#'
#' @rdname readClusterDesc
readClusterResDesc <- function(opts = simOptions(), dot_format = TRUE) {
  if((!is.null(opts$parameters$`other preferences`$`renewable-generation-modelling`) &&
      !opts$parameters$`other preferences`$`renewable-generation-modelling` %in% "clusters") || 
     is.null(opts$parameters$`other preferences`$`renewable-generation-modelling`)){
    stop("readClusterDesc is available only on studies with 'renewable-generation-modelling' = 'clusters' (and Antares >= 8.1)", call. = FALSE)
  }
  .readClusterDesc(opts = opts, 
                   dir = "renewables/clusters",
                   dot_format = dot_format)
}


#' @export
#'
#' @rdname readClusterDesc
readClusterSTDesc <- function(opts = simOptions(), dot_format = TRUE) {
  if (opts$antaresVersion < 860) {
    stop("readClusterSTDesc is available only on Antares >= 8.6)", call. = FALSE)
  }
  .readClusterDesc(opts = opts, 
                   dir = "st-storage/clusters",
                   dot_format = dot_format)
}

#' @importFrom stats setNames
.readClusterDesc <- function(opts = simOptions(), 
                             dir = "thermal/clusters",
                             dot_format = TRUE) {
  path <- file.path(opts$inputPath, dir)
  api_study <- is_api_study(opts)

  table_type <- switch(
    dir,
    "thermal/clusters" = "thermals",
    "renewables/clusters" = "renewables",
    "st-storage/clusters" =  "st-storages"
  )
  
  if (api_study) {
    # api request with all columns
    list_clusters <- api_get(
      opts = opts,
      endpoint = paste0(opts$study_id, "/table-mode/", table_type),
      query = list(columns = "")
    )
    
    dt_clusters <- .convert_list_clusterDesc_to_datatable(list_clusters, 
                                                          type = table_type,
                                                          dot_format = dot_format)
  
    return(dt_clusters)
  }
      
  # "text" mode
  areas <- list.files(path)
  
  # cluster properties from REFERENTIAL (according to version of study)
  properties <- get_input_cluster_properties(table_type = table_type, 
                                             opts = opts, 
                                             dot_format = dot_format)
  
  # read properties for each area
  res <- lapply(areas, function(x, prop_ref=properties) {
    clusters <- readIniFile(file.path(path, x, "list.ini"))
    
    if (length(clusters) == 0) 
      return(NULL)
    
    # 'ldply' used to convert list to data.frame
    clusters <- plyr::ldply(clusters, function(x){
      df_clust <- data.frame(x, check.names = dot_format) 
      colnames_to_add <- setdiff(names(prop_ref), names(df_clust))
      if(!identical(colnames_to_add, character(0)))
        df_clust <- cbind(df_clust, prop_ref[, .SD, .SDcols = colnames_to_add])
      df_clust
      }, 
      .id = NULL) # check.names = FALSE (no automatic conversion)
    
    clusters$area <- x
    # re order columns
    clusters[, c("area", setdiff(colnames(clusters), "area"))]
  })
  
  # convert to 'data.table' too
  res <- data.table::rbindlist(l = res, fill = TRUE)
  
  # order names
  id_cols <- intersect(c("area", "name", "group"), colnames(res))
  additional_cols <- setdiff(colnames(res), id_cols)  
  additional_cols <- additional_cols[order(additional_cols)]
  res <- res[, .SD, .SDcols = c(id_cols, additional_cols)]
  
  # NO PROPERTIES CLUSTER FOUND
  if(length(res) == 0)
    return(data.table())
  
  # renames 'name' to 'cluster' legacy agreement
  data.table::setnames(res, "name", "cluster")
  # as factor + tolower ----
  res$cluster <- as.factor(
    tolower(
      res$cluster))
  res
}


# read and manage referential properties 
  # return referential according to type and study version
get_input_cluster_properties <- function(table_type, opts, dot_format = TRUE){
  # READ cluster properties
  full_ref_properties <- pkgEnv[["inputProperties"]]
  
  category_ref_cluster <- switch(
    table_type,
    "thermals" = "thermal",
    "renewables" = "renewable",
    "st-storages" = "storage"
  )
  
  # filter by category
  ref_filter_by_cat <- full_ref_properties[`Category` %in%
                                             category_ref_cluster]
  # filter by study version
  ref_filter_by_vers <- ref_filter_by_cat[Version.Antares <= 
                                            opts$antaresVersion | 
                                            Version.Antares %in% NA]
  
  # detect evolution on parameter ? (new value according to study version) 
    # filter on value according to study version 
    # select column according to format
  select_col <- ifelse(dot_format, "operating_format", "INI.Name")
  df_multi_params <- ref_filter_by_vers[, 
                                        count := .N, 
                                        by = select_col, 
                                        keyby = TRUE][
                                          count>1][, 
                                                   .SD[which.max(Version.Antares)], 
                                                   by=select_col]
  
  df_unique_params <- ref_filter_by_vers[, 
                                         count := .N, 
                                         by = select_col, 
                                         keyby = TRUE][
                                           count==1]
  
  ref_filter_by_vers <- rbind(df_unique_params, df_multi_params)
  
  # select key colums and put wide format
  ref_filter_by_vers <- ref_filter_by_vers[ , 
                                            .SD, 
                                            .SDcols = c(select_col, 
                                                        "Default", 
                                                        "Type")]
  
  # select names columns to convert to logical + numerical
  logical_col_names <- ref_filter_by_vers[Type%in%"bool"][[select_col]]
  numerical_col_names <- ref_filter_by_vers[Type%in%c("int", "float")][[select_col]]
  
  wide_ref <- data.table::dcast(data = ref_filter_by_vers, 
                                formula = as.formula(paste0(".~", select_col)),
                                value.var = "Default")[
                                  , 
                                  .SD,
                                  .SDcols = -c(".", "name")]
  # /!\ column type conversion on 
  wide_ref[, 
           (logical_col_names):= lapply(.SD, as.logical), 
           .SDcols = logical_col_names][
             ,
             (numerical_col_names):= lapply(.SD, as.numeric), 
             .SDcols = numerical_col_names
           ]
  
  return(wide_ref)
}


.convert_list_clusterDesc_to_datatable <- function(list_clusters, 
                                                   type, 
                                                   dot_format) {
  # return like disk mode 
  if (length(list_clusters) == 0) 
    return(data.table())
  
  # to convert camel case to operating format
  category_ref_cluster <- switch(
    type,
    "thermals" = "thermal",
    "renewables" = "renewable",
    "st-storages" = "storage"
  )
  
  # format output according to parameter
  format_field <- ifelse(dot_format, "operating_format", "INI.Name")
  
  # Tech.Name is the field corresponding to what the API returns
  params_categ <- pkgEnv$inputProperties[Category%in%category_ref_cluster, 
                                         Tech.Name]
    
  rows_cluster <- lapply(
    names(list_clusters), 
    function(cl_name) {
      row_cluster <- as.data.frame(list_clusters[[cl_name]])
      row_cluster[,c("area", "cluster")] <- unlist(strsplit(cl_name, 
                                                            split = " / "))
      return(row_cluster)
    }
  )

  # hamburger
  df_clusters <- do.call("rbind", rows_cluster)
  
  # matching columns in internal referential
  col_names <- setdiff(names(df_clusters), 
                       c("area", "cluster")) 
  index <- match(col_names, params_categ)
  new_names <- pkgEnv$inputProperties[Category%in%category_ref_cluster,
                                      ..format_field][index]
  new_names <- append(new_names[[format_field]],  c("area", "cluster"))
  
  # rename cols
  names(df_clusters) <- new_names
  
  # to order cols
  id_cols <- intersect(c("area", "cluster", "group"), colnames(df_clusters))
  additional_cols <- setdiff(colnames(df_clusters), id_cols)  
  df_clusters <- df_clusters[,c(id_cols, additional_cols)]
  
  # # no camel case on colnames + tolower
  # names(df_clusters) <- tolower(
  #   gsub("(?<=[A-Za-z])(?=[A-Z])", ".", names(df_clusters), perl = TRUE)
  # )
  
  # convention ? (return class object like disk mode)
  df_clusters$cluster <- as.factor(tolower(df_clusters$cluster))
  
  return(as.data.table(df_clusters))
}
