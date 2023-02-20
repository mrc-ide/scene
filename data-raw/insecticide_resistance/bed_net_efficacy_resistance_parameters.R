# Process bed net efficacy parameters

process_efficacy <-  function(x, net_type){
  x |>
    dplyr::select(bioassay_mortality, dn0_med, rn0_med, gamman_med) |>
    dplyr::mutate(pyrethroid_resistance = 1 - bioassay_mortality) |>
    dplyr::select(-bioassay_mortality) |>
    dplyr::rename(
      dn0 = dn0_med,
      rn0 = rn0_med,
      gamman = gamman_med) |>
    dplyr::mutate(
      rnm = 0.24,
      net_type = net_type)
}

pyrethroid_only <- read.csv("data-raw/insecticide_resistance/pyr_only_llin.csv") |>
  process_efficacy("pyrethroid_only")
pyrethroid_pbo <- read.csv("data-raw/insecticide_resistance/pyr_pbo_itn.csv") |>
  process_efficacy("pyrethroid_pbo")
pyrethroid_pyrrole <- read.csv("data-raw/insecticide_resistance/g2_itn.csv") |>
  process_efficacy("pyrethroid_pyrrole")

net_efficacy <- dplyr::bind_rows(pyrethroid_only, pyrethroid_pbo, pyrethroid_pyrrole)

usethis::use_data(net_efficacy, overwrite = TRUE)

