# Process bed net efficacy parameters

net_efficacy <- read.csv("data-raw/insecticide_resistance/net_efficacy_adjusted.csv")
usethis::use_data(net_efficacy, overwrite = TRUE)

