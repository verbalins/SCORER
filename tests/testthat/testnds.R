library(SCORER)

testthat::test_that("Normalization works correctly.", {
  df <- SCORER::load_dataset(system.file("extdata", "FMC.csv", package = "SCORER"),
                             objectives = c("Investment", "TP", "LeanBuffer"),
                             inputs = c("B1","B2","S1","A1","A2","A3","A4","A5","S2"))
  normalized <- df %>% normalize_values() %>% dplyr::select(df$objective_names)
  testthat::expect_lte(max(normalized), 1)
  testthat::expect_gte(min(normalized), 0)
})
