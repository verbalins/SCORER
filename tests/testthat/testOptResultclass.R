library(SCORER)

correct_class <- c("OptResult", "tbl_df", "tbl", "data.frame")
df <- SCORER::load_dataset(system.file("extdata", "FMC.csv", package = "SCORER"),
                           objectives = c("Investment", "TP", "LeanBuffer"),
                           inputs = c("B1","B2","S1","A1","A2","A3","A4","A5","S2"))


testthat::test_that("Class is correctly set from load_dataset()", {
  testthat::expect_s3_class(df, "tbl_df")
})

testthat::test_that("Class is retained for dplyr::mutate", {
  testthat::expect_equal(class(df),  correct_class)
  testthat::expect_equal(df %>% dplyr::mutate(c = 1:nrow(df)) %>% class(), correct_class)
})

testthat::test_that("Class is retained for dplyr::rowwise and dplyr::ungroup", {
  testthat::expect_equal(df %>% dplyr::rowwise() %>% dplyr::mutate(c = 1) %>% class(), c("rowwise_df", correct_class))
  testthat::expect_equal(df %>% dplyr::rowwise() %>% dplyr::mutate(c = 1) %>% dplyr::ungroup() %>% class(), correct_class)
})

testthat::test_that("Class has correct metadata attributes", {
  testthat::expect_true(!is.null(attr(df, "metadata")))
  testthat::expect_type(df$objectives, "logical")
  testthat::expect_type(df$objective_names, "character")
  testthat::expect_equal(df$inputs, c("B1","B2","S1","A1","A2","A3","A4","A5","S2"))
  testthat::expect_type(df$outputs, "character")
})
