test_that("data.frame indexing works", {
  a <- data.frame(a = 1:10, b = 10:1)
  # single row
  f1 <- data.frame(a = 1, b = 10)
  expect_equal(index_df(a, f1), 1)
  # multiple rows
  f2 <- data.frame(a = 1:2, b = 10:9)
  expect_equal(index_df(a, f2), 1:2)
  # no match
  f3 <- data.frame(a = 2, b = 10)
  expect_equal(index_df(a, f3), NA_integer_)
  f4 <- data.frame(a = 2:3, b = 10:9)
  expect_equal(index_df(a, f4), c(NA_integer_, NA_integer_))
  # multiple matches
  f5 <- data.frame(a = c(1, 1), b = c(10,10))
  expect_equal(index_df(a, f5), c(1, 1))
})

test_that("data.frame aggregation works", {
  df <- data.frame(
    group = c("A", "A", "B", "B"),
    to_sum = 1:4,
    to_average = 1:4,
    to_weighted_average = 1:4,
    weights = c(1, 2, 1, 2)
  )

  expect_equal(
    aggregate_df(
      df = df,
      groups = "group",
      sum_cols = "to_sum",
      mean_cols = "to_average",
      weighted_mean_cols = "to_weighted_average",
      w = "weights"
    ),
    tibble::tibble(
      group = c("A", "B"),
      to_sum = c(sum(1:2), sum(3:4)),
      to_average = c(mean(1:2), mean(3:4)),
      to_weighted_average = c(weighted.mean(1:2, 1:2), weighted.mean(3:4, 1:2))
    )
  )
})
