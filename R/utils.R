#' Index a data.frame by another
#'
#' Take a data.frame `df` and return the row-index matching rows from another, `filter_row`.
#'
#' @param df The input data.frame to obtain row indices for
#' @param filter_row The matching data.frame. Each row of `filter_row` is matched to a row in `df`.
#'
#' @export
index_df <- function(df, filter_row) {
  indexed_df <- df |>
    dplyr::mutate(index = 1:dplyr::n())

  filter_row <- filter_row[ , names(filter_row) %in% names(df), drop = FALSE]

  row_index <- filter_row |>
    dplyr::left_join(indexed_df, by = names(filter_row), multiple = "all") |>
    dplyr::pull(.data$index)

  return(row_index)
}

#' Aggregate a data frame
#'
#' @param df data.frame
#' @param groups character vector of grouping variables
#' @param sum_cols character vector of variables to summarise by sum
#' @param mean_cols character vector of variables to summarise by mean
#' @param weighted_mean_cols character vector of variables to summarise by weighted mean
#' @param w column name for weights to be used for weighted mean summary
#'
#' @return An aggregated (un-grouped) data frame
aggregate_df <- function(df, groups,
                         sum_cols = NULL,
                         mean_cols = NULL,
                         weighted_mean_cols = NULL, w = NULL) {

  summary_df <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(sum_cols), sum),
      dplyr::across(dplyr::all_of(mean_cols), mean),
      dplyr::across(dplyr::all_of(weighted_mean_cols), \(x) stats::weighted.mean(x, .data[[w]])),
      .groups = "drop"
    )

  return(summary_df)
}
