

#' @title  Get the Thematic trimming of an Antares study
#'
#' @description
#' `r antaresRead:::badge_api_no()`
#'
#'
#'
#' This function reads the "selection variables" section of the study's
#' "generaldata.ini" file.
#'
#' Minimal version required is `v8.8`.
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
#' # Get Thematic trimming of Antares study version >= v8.8
#' getThematicTrimming()
#' }
getThematicTrimming <- function(opts = simOptions()){
  stopifnot(opts$antaresVersion>=880)

  ##
  # API bloc
  ##
  if(is_api_study(opts = opts)){
    # use endpoint from API "Get thematic trimming config"
    endpoint_to_use <- "config/thematictrimming/form"
    endpoint_to_use <- file.path(opts$study_id,
                                 endpoint_to_use)

    thematic_vars <- api_get(opts = opts,
                             endpoint = endpoint_to_use)

    return(thematic_vars)
  }

  ##
  # Desktop
  ##

  # use private referentiel according to version
  target_version <- as.character(opts$antaresVersion)
  ref_list_vars_thematic <- pkgEnv$thematic[[target_version]]

  # thematic vars
  vs_section <- opts$parameters$`variables selection`

  # case with no meta data
    # generaldata.ini has no section "[variables selection]"
    # with no data all variables are "active"
  if(is.null(vs_section)){
    warning("`variables selection` section in file 'generaldata.ini' does not exist",
            call. = FALSE)
    warning("All variables status are 'active'",
            call. = FALSE)

    df_thematic <- data.frame(
      variables = ref_list_vars_thematic$col_name,
      status_selection = "active"
    )

    return(df_thematic)
  }

  # case with all variables skiped
  if(!vs_section$selected_vars_reset &
     !length(vs_section)>1){
    message("All variables status are 'skip'")

    df_thematic <- data.frame(
      variables = ref_list_vars_thematic$col_name,
      status_selection = "skip"
    )

    return(df_thematic)
  }

  # case with patterns
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

    check_index <- ref_list_vars_thematic$col_name %in% col_names

    df_thematic <- data.frame(
      variables = ref_list_vars_thematic$col_name,
      status_selection = ifelse(check_index, "active", "skip")
      )
    return(df_thematic)
  }else{
    # manage subtraction variables
    col_names <- vs_section[check_pattern_remove]
    col_names <- unlist(col_names, use.names = FALSE)

    check_index <- ref_list_vars_thematic$col_name %in% col_names

    df_thematic <- data.frame(
      variables = ref_list_vars_thematic$col_name,
      status_selection = ifelse(check_index, "skip", "active")
    )
    return(df_thematic)
  }

}
