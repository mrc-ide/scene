#' Plot multi panel interventions
#'
#' @inheritParams plot_interventions
#' @param text vector of text size for aggregated and disaggregated plots
#'
#' @return Combined country and admin plot
#' @export
plot_interventions_combined <- function(interventions, population, group_var, text = c(11, 11)){
  patchwork::wrap_plots(
    list(
      plot_interventions(interventions, population, group_var, text_size = text[1]),
      plot_interventions(interventions, population, group_var, country = FALSE, text_size = text[2])
    )
  ) + patchwork::plot_layout(guides = "collect")
}

#' Plot interventions
#'
#' @param interventions Interventions input
#' @param population Population input
#' @param group_var Site grouping
#' @param country Aggregate to the country level
#' @param include Intervention variables to include
#' @param labels Labels for variables in include
#' @param text_size Size of font
#'
#' @return Intervention plot
#' @export
plot_interventions <- function(interventions,
                               population,
                               group_var,
                               country = TRUE,
                               include = c("itn_use", "itn_input_dist", "fitted_usage", "tx_cov", "irs_cov", "rtss_cov", "smc_cov", "pmc_cov"),
                               labels = c("ITN usage", "ITN model input", "ITN model usage", "Treatment", "IRS", "RTSS", "SMC", "PMC"),
                               text_size = 8){

  intervention_colours <- intervention_colours

  group_var <- c(group_var, "year")

  interventions <- interventions |>
    dplyr::left_join(population, by = group_var)

  # Dashed lines for ITN helpers
  lt <- rep(1, length(include))
  lt[include %in% c("itn_input_dist", "fitted_usage")] <- 2

  group_var <- setdiff(group_var, "urban_rural")
  if(country){
    group_var <- c("country", "iso3c", "year")
  }

  pd <- aggregate_df(df = interventions, groups = group_var, weighted_mean_cols = include, w = "par") |>
    tidyr::pivot_longer(-group_var, names_to = "Intervention", values_to = "Coverage") |>
    dplyr::mutate(Intervention = factor(.data$Intervention, levels = include, labels = labels))

  intervention_plot <- ggplot2::ggplot(
    data = pd,
    ggplot2::aes(x = .data$year, y = .data$Coverage, col = .data$Intervention, linetype = .data$Intervention)
  ) +
    ggplot2::scale_colour_manual(values = intervention_colours) +
    ggplot2::scale_linetype_manual(values = lt) +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::xlab("Year") +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"))

  if("name_1" %in% names(pd)){
    intervention_plot <- intervention_plot +
      ggplot2::facet_wrap(~ .data$name_1, nrow = 4) +
      ggplot2::theme(text = ggplot2::element_text(size = text_size),
                     axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
  }

  return(intervention_plot)
}
