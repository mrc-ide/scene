# Link intervention outputs to set colours
intervention_colours <- c(
  "#438300",
  "#6923d2",
  "#f0ab00",
  "#00cfb2",
  "#6e0094",
  "#ff9e58",
  "#001f68",
  "#9d3f00",
  "#00badb",
  "#e9006f",
  "#0270c0",
  "#342302",
  "#ec83ff",
  "#a40076"
)
names(intervention_colours) <- c("ITN usage", "ITN model input", "ITN model usage", "Treatment", "IRS", "RTSS", "SMC", "PMC", "LSM")
usethis::use_data(intervention_colours, overwrite = TRUE)
