# Link intervention outputs to set colours
intervention_colours <- RColorBrewer::brewer.pal(8, "Dark2")
names(intervention_colours) <- c("ITN usage", "ITN model input", "ITN model usage", "Treatment", "IRS", "RTSS", "SMC", "PMC")
usethis::use_data(intervention_colours, overwrite = TRUE)
