context("Testing the function latency")

test_that("Testing basicstartleplot", {

  df <- readRDS("results3.rds")

  x <- latencyPlot(df, addhead = 0.1)

  res <- readRDS("expPl.rds")
  res <- res[[5]]

  expect_equal(x, res)

})
