#Copyright © 2016 RTE Réseau de transport d’électricité

#' Import clusters description
#'
#' @description 
#' This function reads in the input files of an antares study the
#' characteristics of each cluster. 
#' 
#' Be aware that clusters descriptions are read
#' in the input files so they may have changed since a simulation has been run.
#'
#' @inheritParams readAntares
#'
#' @return
#' A data.table with one line per cluster. The columns of the data.table may
#' change between different projects, but there will always be the following 
#' columns:
#' 
#' \item{area}{Name of the area containing the cluster}
#' \item{cluster}{Name of the cluster}
#' \item{group}{Type of cluster (gaz, nuclear, etc.)}
#' \item{unitcount}{number of production units}
#' \item{nominalcapacity}{production capacity of each unit}
#' 
#' The other present columns depends on the version of antares and the options
#' that have been set: if an option is unset for all clusters, it will not 
#' appear in the table.
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
#' @examples
#' 
#' \dontrun{
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
readClusterDesc <- function(opts = simOptions()) {
  .readClusterDesc(opts = opts, dir = "thermal/clusters")
}

#' @export
#'
#' @rdname readClusterDesc
readClusterResDesc <- function(opts = simOptions()) {
  if((!is.null(opts$parameters$`other preferences`$`renewable-generation-modelling`) &&
      !opts$parameters$`other preferences`$`renewable-generation-modelling` %in% "clusters") || 
     is.null(opts$parameters$`other preferences`$`renewable-generation-modelling`)){
    stop("readClusterDesc is available only on studies with 'renewable-generation-modelling' = 'clusters' (and Antares >= 8.1)", call. = FALSE)
  }
  .readClusterDesc(opts = opts, dir = "renewables/clusters")
}


#' @export
#'
#' @rdname readClusterDesc
readClusterSTDesc <- function(opts = simOptions()) {
  if (opts$antaresVersion < 860) {
    stop("readClusterSTDesc is available only on Antares >= 8.6)", call. = FALSE)
  }
  .readClusterDesc(opts = opts, dir = "st-storage/clusters")
}


.readClusterDesc <- function(opts = simOptions(), 
                             dir = "thermal/clusters") {
  
  path <- file.path(opts$inputPath, dir)
  api_study <- is_api_study(opts)
  
  table_type <- switch(
    dir,
    "thermal/clusters" = "thermals",
    "renewables/clusters" = "renewables",
    "st-storage/clusters" =  "st-storages"
  )
  
  if(api_study){
    
    # api request with all columns
    list_clusters = api_get(
      opts = opts,
      endpoint = paste0(opts$study_id, "/table-mode/", table_type),
      query = list(
        columns = ""
      )
    )
    
    return(list_clusters)
  }
      
  # "text" mode
  areas <- list.files(path)
  
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
  ref_filter_by_vers <- ref_filter_by_cat[`Version Antares` <= 
                                            opts$antaresVersion | 
                                            `Version Antares` %in% NA]
  
  # select key colums and put wide format
  ref_filter_by_vers <- ref_filter_by_vers[ , 
                                            .SD, 
                                            .SDcols = c("INI Name", 
                                                        "Default", 
                                                        "Type")]
  
  # select names columns to convert to logical + numerical
  logical_col_names <- ref_filter_by_vers[Type%in%"bool"][["INI Name"]]
  numerical_col_names <- ref_filter_by_vers[Type%in%c("int", "float")][["INI Name"]]
  
  wide_ref <- data.table::dcast(data = ref_filter_by_vers, 
                                formula = .~`INI Name`,
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
  
  # read properties for each area
  res <- plyr::llply(areas, function(x) {
    clusters <- readIniFile(file.path(path, x, "list.ini"))
    if (length(clusters) == 0) 
      return(NULL)
    # conversion list to data.frame
    clusters <- plyr::ldply(clusters, function(x){
      df_clust <- data.frame(x, check.names = FALSE) 
      colnames_to_add <- setdiff(names(wide_ref), names(df_clust))
      if(!identical(colnames_to_add, character(0)))
        df_clust <- cbind(df_clust, wide_ref[, .SD, .SDcols = colnames_to_add])
      df_clust
      }) # check.names = FALSE (too many side effects)
    clusters$.id <- NULL
    clusters$area <- x
    # re order columns
    clusters[, c("area", setdiff(colnames(clusters), "area"))]
  })
  
  res <- data.table::rbindlist(l = res, fill = TRUE)
  
  # NO PROPERTIES CLUSTER FOUND
  if(length(res) == 0){
    warning("No properties found", 
            call. = FALSE)
    return(NULL)
  } 
  
  # output format conversion
  res <- data.table::as.data.table(res)
  data.table::setnames(res, "name", "cluster")
  res$cluster <- as.factor(tolower(res$cluster))
  res
}
