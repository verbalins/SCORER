inputs <- c("B1","B2","S1","A1","A2","A3","A4","A5","S2")

FMC <- SCORER::loaddataset("inst/extdata/WSC11.csv", inputs = inputs, objectives = c("Investment", "TP", "LeanBuffer"))

usethis::use_data(FMC, compress="xz", overwrite = TRUE)
