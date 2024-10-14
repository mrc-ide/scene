#' Title Add future net input distribution
#'
#' Infer cycle period (for N-yearly main distribution frequency) and fit input
#' distribution, ensuring already calibrated values remain fixed). Requires `itn_input_dist`
#' values that are to be fitted to be NA.
#'
#' @param interventions Site file interventions section
#' @param group_var Site grouping
#' @param off_year_max Maximum distribution outside of N-yearly mass
#' @param cycle_period Assumed cycle period (years) for mass distribution
#'
#' @export
add_future_net_dist <- function(interventions, group_var, off_year_max = 0.2, cycle_period = 3){
  interventions <- interventions |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) |>
    dplyr::mutate(
      data_year = max(.data$year[!is.na(.data$itn_input_dist)]),
      # Infer a large scale net distribution year
      cycle_peak = .data$year[which.max(.data$itn_use[.data$year < .data$data_year])],
      # Ensure future distribution are cyclical
      du = ifelse(is.na(.data$itn_input_dist), off_year_max, 1),
      du = ifelse(is.na(.data$itn_input_dist) & ((.data$year - .data$cycle_peak) %% cycle_period == 0), 1, .data$du),
      itn_input_dist = netz::usage_to_model_distribution(
        .data$itn_use,
        1 + (.data$year - min(.data$year) + 0.5) * 365,
        1 + (.data$year - min(.data$year)) * 365,
        distribution_upper  = .data$du,
        mean_retention = .data$mean_retention[1]
      ),
      fitted_usage = netz::model_distribution_to_usage(
        1 + (.data$year - min(.data$year) + 0.5) * 365,
        .data$itn_input_dist,
        1 + (.data$year - min(.data$year)) * 365,
        mean_retention = .data$mean_retention[1]
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-c("du", "cycle_peak", "data_year"))
  return(interventions)
}


#' Add in resistance projections
#'
#' @param interventions Site file interventions section
#' @param pyrethroid_resistance Site file pyrethroid resistance
#' @param group_var Site grouping
#'
#' @export
link_resistance <- function(interventions, pyrethroid_resistance, group_var){
  interventions <- interventions |>
    dplyr::select(-dplyr::any_of("pyrethroid_resistance")) |>
    dplyr::left_join(pyrethroid_resistance, by = c(group_var, "year"))
  return(interventions)
}

#' Link net type, irs type and net resistance to efficacy parameters
#'
#' @param interventions Site file interventions section
#'
#' @export
link_vector_control_parameters <- function(interventions){
  net_efficacy <- net_efficacy
  irs_parameters <- irs_parameters

  ne <- net_efficacy |>
    dplyr::rename(pr = .data$pyrethroid_resistance)

  interventions <- interventions |>
    dplyr::select(-dplyr::any_of(c("dn0", "rn0", "gamman", "rnm"))) |>
    dplyr::left_join(ne, dplyr::join_by(closest("pyrethroid_resistance" >= "pr"), "net_type")) |>
    dplyr::select(-("pr")) |>
    dplyr::select(-dplyr::any_of(c("ls_theta", "ls_gamma", "ks_theta", "ks_gamma", "ms_theta", "ms_gamma"))) |>
    dplyr::left_join(irs_parameters, by = "irs_insecticide")
  return(interventions)
}

utils::globalVariables("closest")
