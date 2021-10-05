library(SCORER)

df <- SCORER::load_dataset(system.file("extdata", "FMC.csv", package = "SCORER"),
                           objectives = c("Investment", "TP", "LeanBuffer"),
                           inputs = c("B1","B2","S1","A1","A2","A3","A4","A5","S2"))

test_that("Loading is done correctly from external data.", {
  expect_equal(df |> nrow(), SCORER::FMC |> nrow())
  expect_equal(df$parameters, SCORER::FMC$parameters)
  expect_equal(df$objectives, SCORER::FMC$objectives)
  expect_equal(df$inputs, SCORER::FMC$inputs)
})

test_that("Distances are correctly applied, serial.", {
  distances <- df %>% add_distances()
  testthat::expect_true("Distance" %in% colnames(distances))
  testthat::expect_true(all(distances %>% dplyr::filter(Rank == 1) %>% dplyr::select("Distance") == 0))
  testthat::expect_true(all(distances %>% dplyr::filter(Rank > 1) %>% dplyr::select("Distance") != 0))
})

test_that("Distances are correctly applied, parallel.", {
  distances <- df %>% add_distances(parallel_cores = 10)
  testthat::expect_true("Distance" %in% colnames(distances))
  testthat::expect_true(all(distances %>% dplyr::filter(Rank == 1) %>% dplyr::select("Distance") == 0))
  testthat::expect_true(all(distances %>% dplyr::filter(Rank > 1) %>% dplyr::select("Distance") != 0))
})
