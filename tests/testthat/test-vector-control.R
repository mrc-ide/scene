test_that("Future net distribution works", {
  interventions <- data.frame(
    level1 = "A",
    year = 2001:2010,
    itn_use = c(0, 0, 0, 0, 0.4, 0.3, 0.8, 1, 0.9, 0.9),
    itn_input_dist = c(0, 0, 0, 0, 0.5, 0.6, 0.1, NA, NA, NA)
  )

  fit1 <- add_future_net_dist(
    interventions = interventions,
    group_var = "level1",
    off_year_max = 0.2,
    cycle_period = 3
    )

  expect_equal(tail(fit1$itn_input_dist, 3), c(1, 0.2, 0.2))

  fit2 <- add_future_net_dist(
    interventions = interventions,
    group_var = "level1",
    off_year_max = 0.2,
    cycle_period = 2
  )

  expect_equal(tail(fit2$itn_input_dist, 3), c(0.2, 1, 0.2))

  fit3 <- add_future_net_dist(
    interventions = interventions,
    group_var = "level1",
    off_year_max = 0.5,
    cycle_period = 3
  )

  expect_equal(tail(fit3$itn_input_dist, 3), c(1, 0.5, 0.5))

})
