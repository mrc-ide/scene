test_that("expand interventions works", {
  interventions <- data.frame(
    level1 = rep(c("A", "B"), each = 4),
    year = rep(1:4, 2),
    var1 = rep(c(1:2, NA, NA), 2)
  )

  group_var = "level1"

  expect_equal(
    expand_interventions(
      interventions = interventions,
      max_year = 10,
      group_var = group_var),
    tibble::tibble(
      level1 = rep(c("A", "B"), each = 10),
      year = rep(1:10, 2),
      var1 = rep(c(1:2, rep(NA, 8)), 2)
    )
  )
})

test_that("set_change_point works", {
  interventions <- data.frame(
    level1 = rep(c("A", "B"), each = 4),
    year = rep(1:4, 2),
    var1 = rep(c(1:2, NA, NA), 2)
  )

  expect_equal(
    set_change_point(
      interventions = interventions,
      sites = data.frame(level1 = c("A", "B")),
      var = "var1",
      year = 3,
      target = 100
    ),
    dplyr::mutate(interventions, var1 = rep(c(1:2, 100, NA), 2))
  )

  expect_equal(
    set_change_point(
      interventions = interventions,
      sites = data.frame(level1 = c("A")),
      var = "var1",
      year = 3,
      target = 100
    ),
    dplyr::mutate(interventions, var1 = c(1:2, 100, NA, 1:2, NA, NA))
  )

  expect_error(
    set_change_point(
      interventions = interventions,
      sites = data.frame(level1 = c("A", "B")),
      var = "var1",
      year = 2,
      target = 100
    ),
    "Trying to overwrite existing value in interventions: var1"
  )
})

test_that("ever and last used works", {
  interventions <- data.frame(
    level1 = rep(c("A", "B", "C"), each = 5),
    year = rep(1:5, 3),
    var1 = c(rep(0, 4), NA, rep(1, 4), NA, 1, 0, 0, 0, NA)
  )

  expect_equal(
    ever_used(
      interventions = interventions,
      var = "var1",
      group_var = "level1"
    ),
    tibble::tibble(
      level1 = c("B", "C")
    )
  )

  expect_equal(
    last_used(
      interventions = interventions,
      var = "var1",
      group_var = "level1"
    ),
    tibble::tibble(
      level1 = c("B")
    )
  )
})

test_that("interpolation and fill works", {
  interventions <- data.frame(
    level1 = rep(c("A", "B"), each = 4),
    year = rep(1:4, 2),
    var1 = rep(c(1:2, NA, 4), 2)
  )

  expect_equal(
    linear_interpolate(
      interventions = interventions,
      vars = "var1",
      group_var = "level1"
    ),
    tibble::tibble(
      level1 = rep(c("A", "B"), each = 4),
      year = rep(1:4, 2),
      var1 = rep(c(1:2, 3, 4), 2)
    )
  )

  interventions <- data.frame(
    level1 = rep(c("A", "B"), each = 4),
    year = rep(1:4, 2),
    var1 = rep(c(1:2, NA, NA), 2)
  )

  expect_equal(
    fill_extrapolate(
      interventions = interventions,
      group_var = "level1",
      not = NULL
    ),
    tibble::tibble(
      level1 = rep(c("A", "B"), each = 4),
      year = rep(1:4, 2),
      var1 = rep(c(1:2, 2, 2), 2)
    )
  )
})
