context("Testing the function loadStartleData")

test_that("Testing importing and data processing", {

  df <- loadStartleData(correction = FALSE)
  df_withcorrection <- loadStartleData(correction = TRUE)
  df_wcor_tailhead <- loadStartleData(correction = TRUE, addtail = 0.5, addhead = 0.5)

  expect_equal(df, readRDS("results1.rds"))
  expect_equal(df_withcorrection, readRDS("results2.rds"))
  expect_equal(df_wcor_tailhead, readRDS("results3.rds"))

})
