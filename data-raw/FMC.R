inputs <- c("B1","B2","S1","A1","A2","A3","A4","A5","S2")

FMC <- SCORER::load_dataset("inst/extdata/FMC.csv", inputs = inputs, objectives = c("Investment", "TP", "LeanBuffer")) %>%
  add_distances(parallel_cores = 10)

usethis::use_data(FMC, compress="xz", overwrite = TRUE)
