# Link intervention outputs to set colours
intervention_colours <- c(
  "#6923d2",
  "#6923d2",
  "#f0ab00",
  "#00cfb2",
  "#001f68",
  "#00badb",
  "#e9006f",
  "#ec83ff",
  "#9d3f00",
  "#438300",
  "#a40076",
  "#0270c0",
  "#6e0094",
  "#342302",
  "#a40076"
)
names(intervention_colours) <- c("ITN usage", "ITN model input", "ITN model usage", "Treatment", "IRS", "RTSS", "SMC", "PMC", "LSM", "R21")
usethis::use_data(intervention_colours, overwrite = TRUE)

# Link intervention outputs to set line types
intervention_line_type <- c(
  1,
  2,
  3,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1
)
names(intervention_line_type) <- c("ITN usage", "ITN model input", "ITN model usage", "Treatment", "IRS", "RTSS", "SMC", "PMC", "LSM", "R21")
usethis::use_data(intervention_line_type, overwrite = TRUE)
