context("Tests for loading data from different formats")
library(SCORER)

test_that("Loading is done correctly from external data.", {
  df <- SCORER::loaddataset(system.file("extdata", "FMC.csv", package = "SCORER"),
                      objectives = c("Investment", "TP", "LeanBuffer"),
                      inputs = c("B1","B2","S1","A1","A2","A3","A4","A5","S2"))
  expect_equal(df |> nrow(), SCORER::FMC |> nrow())
  expect_equal(df$parameters, SCORER::FMC$parameters)
  expect_equal(df$objectives, SCORER::FMC$objectives)
  expect_equal(df$inputs, SCORER::FMC$inputs)
})
