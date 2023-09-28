


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
      # mange subtraction variables
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
