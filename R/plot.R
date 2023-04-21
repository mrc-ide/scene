#' Plot multi panel interventions
#'
#' @inheritParams plot_interventions
#' @param text vector of text size for aggregated and disaggregated plots
#'
#' @return Combined country and admin plot
#' @export
plot_interventions_combined <- function(interventions, population, group_var,
                                        include = c("itn_use", "itn_input_dist", "fitted_usage", "tx_cov", "irs_cov", "rtss_cov", "smc_cov", "pmc_cov"),
                                        labels = c("ITN usage", "ITN model input", "ITN model usage", "Treatment", "IRS", "RTSS", "SMC", "PMC"),
                                        text = c(11, 11),
                                        facet_rows = 4){
  patchwork::wrap_plots(
    list(
      plot_interventions(
        interventions = interventions,
        population = population,
        group_var = group_var[1],
        text_size = text[1],
        include = include,
        labels = labels,
        facet_rows = facet_rows
      ),
      plot_interventions(
        interventions = interventions,
        population = population,
        group_var = group_var,
        text_size = text[2],
        include = include,
        labels = labels,
        facet_rows)
    )
  ) + patchwork::plot_layout(guides = "collect")
}

#' Plot interventions
#'
#' @param interventions Interventions input
#' @param population Population input
#' @param group_var Site grouping
#' @param include Intervention variables to include
#' @param labels Labels for variables in include
#' @param text_size Size of font
#' @param facet_rows Number of rows for faceted plot
#'
#' @return Intervention plot
#' @export
plot_interventions <- function(interventions,
                                population,
                                group_var,
                                include = c("itn_use", "itn_input_dist", "fitted_usage", "tx_cov", "irs_cov", "rtss_cov", "smc_cov", "pmc_cov", "lsm_cov"),
                                labels = c("ITN usage", "ITN model input", "ITN model usage", "Treatment", "IRS", "RTSS", "SMC", "PMC", "LSM"),
                                text_size = 8,
                                facet_rows = 4){

  intervention_colours <- intervention_colours

  # Assume last var is for faceting
  facet_var <- group_var[length(group_var)]
  group_var <- c(group_var, "year")

  interventions <- interventions |>
    dplyr::left_join(population, by = intersect(colnames(interventions), colnames(population)))

  # Dashed lines for ITN helpers
  lt <- rep(1, length(include))
  lt[include %in% c("itn_input_dist", "fitted_usage")] <- 2

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
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size),
      strip.background = ggplot2::element_rect(fill = "white")
    )

  if(length(unlist(unique(pd[,facet_var]))) > 1){
    intervention_plot <- intervention_plot +
      ggplot2::facet_wrap(~ .data[[facet_var]], nrow = facet_rows) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
  }

  return(intervention_plot)
}
