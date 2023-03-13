#' Expand the inereventions time frame for all sites
#'
#' @param interventions Site file interventions section
#' @param max_year Maximum year to expand to
#' @param group_var Site grouping
#'
#' @export
expand_interventions <- function(interventions, max_year, group_var){
  expanded_interventions <- interventions |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) |>
    tidyr::complete(year = min(interventions$year):max_year) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(group_var, "year"))))
  return(expanded_interventions)
}


#' Set an intervention change point
#'
#' Modifies the site file interventions section with a new change point for an intervention
#'
#' @param interventions Site file interventions section
#' @param sites Site reference to modify at
#' @param var Interventions variable to modify
#' @param year Year of change point
#' @param target New value of change point
#'
#' @export
set_change_point <- function(interventions, sites, var, year, target){

  template <- sites
  template$year <- year
  template$target <- target

  index <- index_df(interventions, template[,c(names(sites), "year")])

  if(!all(is.na(interventions[index, var]))){
    stop(paste("Trying to overwrite existing value in interventions:", var))
  }

  interventions[index, var] <- template$target

  return(interventions)
}


#' Select areas that have ever used an intervention
#'
#' @param interventions Site file interventions section
#' @param var Interventions variable to check if ever used
#' @param group_var Site grouping
#'
#' @export
ever_used <- function(interventions, var, group_var){
  interventions <- interventions |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) |>
    dplyr::summarise(ever = sum(.data[[var]], na.rm = TRUE) > 0) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$ever) |>
    dplyr::select(dplyr::all_of(group_var))
  return(interventions)
}

#' Select areas that have used an intervention in the last year with data
#'
#' @param interventions Site file interventions section
#' @param var Interventions variable to check if last used
#' @param group_var Site grouping
#'
#' @export
last_used <- function(interventions, var, group_var){
  interventions <- interventions |>
    dplyr::filter(!is.na(.data[[var]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) |>
    dplyr::filter(.data$year == max(.data$year)) |>
    dplyr::ungroup() |>
    dplyr::mutate(last = .data[[var]] > 0) |>
    dplyr::filter(.data$last) |>
    dplyr::select(dplyr::all_of(group_var))
  return(interventions)
}


#' Linearly interpolate NAs points
#'
#' @param interventions Site file interventions section
#' @param vars Character string naming Interventions to be interpolated
#' @param group_var Site grouping
#'
#' @export
linear_interpolate <- function(interventions, vars, group_var){
  interventions <- interventions |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(vars), \(x) zoo::na.approx(x, na.rm = FALSE))) |>
    dplyr::ungroup()
  return(interventions)
}

#' Extrapolation of any NAs with previous value
#'
#' @param interventions Site file interventions section
#' @param group_var Site grouping
#' @param not Character string naming Interventions not to be extrapolated
#'
#' @export
fill_extrapolate <- function(interventions, group_var, not = "itn_input_dist"){
  f_interventions <- interventions |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) |>
    tidyr::fill(-dplyr::all_of(not), .direction = "down") |>
    dplyr::ungroup()
  return(f_interventions)
}
