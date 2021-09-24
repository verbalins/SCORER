inputs <- c("IuOp1E","IpOp1E","IuOp1G","IpOp1G","IpOp1H","IpOp1J","IuOp1N","IpOp1N","IpOp1O","B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","B13","B14","B15","B16","B17","B18","B19","B20","B21","B22","B23","B24","B25","B26","B27","B28","B29","B30","B31")
ACM <- SCORER::loaddataset("inst/extdata/ACM.csv", inputs = inputs, objectives = c("RunningCost", "InvestmentCost", "BufferCapacity")) %>%
  add_distances(parallel_cores = 10)

usethis::use_data(ACM, compress="xz", overwrite = TRUE)
