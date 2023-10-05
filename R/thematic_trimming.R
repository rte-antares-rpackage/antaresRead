

#' @title  Get the Thematic trimming of an Antares study  
#'  
#' @description 
#' `r antaresRead:::badge_api_no()`  
#' 
#' 
#' This function reads the "selection variables" section of the study's 
#' "generaldata.ini" file.
#' 
#' 
#' @inheritParams readAntares
#' 
#' @return `data.frame` with 2 columns :  
#'  - `variables` : names are displayed according to the study version  
#'  - `status_selection` : have 2 possible values {"active"; "skip"}
#' 
#' @export
#' @examples 
#' \dontrun{
#' # Get Thematic trimming of Antares study version >= v8.0
#' getThematicTrimming()
#' }
getThematicTrimming <- function(opts = simOptions()){
  stopifnot(inherits(opts, "simOptions"))
  
  # read study to update opts meta data 
  opts_study <- setSimulationPath(opts$studyPath, 
                                  simulation = "input")
  
  # use global referentiel
  ref_list_vars_thematic <- pkgEnv$thematic
  
  # filter vars with version
  ref_list_vars_thematic <- ref_list_vars_thematic[version<=opts_study$antaresVersion,]
  
  # thematic vars 
  vs_section <- opts_study$parameters$`variables selection`
  
  if(is.null(vs_section)){
    warning("`variables selection` section in file 'generaldata.ini' does not exist", 
            call. = FALSE)
    return()
  }
  else{
    pattern_add <- "select_var +"
    pattern_remove <- "select_var -"
    check_pattern_add <- grepl(pattern = pattern_add, 
                               x = names(vs_section), 
                               fixed = TRUE)
    check_pattern_remove <- grepl(pattern = pattern_remove, 
                                  x = names(vs_section),
                                  fixed = TRUE)
    
    # manage addition variables
    if(any(check_pattern_add)){
      col_names <- vs_section[check_pattern_add]
      col_names <- unlist(col_names, use.names = FALSE)
      
      check_index <- ref_list_vars_thematic$variable %in% col_names
      
      df_thematic <- data.frame(
        variables = ref_list_vars_thematic$variable,
        status_selection = ifelse(check_index, "active", "skip")
        )
      return(df_thematic)
    }else{
      # manage subtraction variables
      col_names <- vs_section[check_pattern_remove]
      col_names <- unlist(col_names, use.names = FALSE)
      
      check_index <- ref_list_vars_thematic$variable %in% col_names
      
      df_thematic <- data.frame(
        variables = ref_list_vars_thematic$variable,
        status_selection = ifelse(check_index, "skip", "active")
      )
      return(df_thematic)
    }
  }
}
