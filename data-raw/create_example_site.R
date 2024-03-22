itn_use <- c(0, 0.1, 0.2, 0.4, 0.4)
net_input <- round(
  netz::usage_to_model_distribution(
    usage = c(0, 0.1, 0.2, 0.4, 0.4),
    usage_timesteps = 365 * 0:4 + 180,
    distribution_timesteps = 1 + 365 * 0:4
  ),
  2
)

example_site <- list(
  sites = data.frame(
    country = "Eg",
    site = c("A", "B")
  ),
  interventions = rbind(
    data.frame(
      country = "Eg",
      site = "A",
      year = 1:5,
      itn_use = itn_use,
      itn_input_dist = net_input,
      mean_retention = 1000,
      tx_cov = c(0, 0.3, 0.4, 0.45, 0.5),
      irs_cov = 0,
      rtss_cov = 0,
      smc_cov = 0,
      pmc_cov = 0,
      lsm_cov = 0
    ),
    data.frame(
      country = "Eg",
      site = "B",
      year = 1:5,
      itn_use = itn_use,
      itn_input_dist = net_input,
      mean_retention = 1000,
      tx_cov = c(0, 0.3, 0.4, 0.45, 0.5),
      irs_cov = 0,
      rtss_cov = 0,
      smc_cov = c(0, 0, 0.8, 0.8, 0.8),
      pmc_cov = 0,
      lsm_cov = 0
    )
  ),
  population = rbind(
    data.frame(
      country = "Eg",
      site = "A",
      year = 1:10,
      par = 11:20 * 150
    ),
    data.frame(
      country = "Eg",
      site = "B",
      year = 1:10,
      par = 11:20 * 100
    )
  )
)

usethis::use_data(example_site, overwrite = TRUE)
