context("Testing the function plotStartle")

test_that("Testing plotStartle", {

  df <- readRDS("results3.rds")

  x <- list(startlePlot(df, type = 1), startlePlot(df, type = 2), startlePlot(df, type = 3))
  # x <- list(startlePlot(df, type = 1), startlePlot(df, type = 2), startlePlot(df, type = 3), basicStartlePlot(df), latencyPlot(df, addhead = 0.1))
  # saveRDS(x, file = "expPl.rds", version = 2, compress = "xz")
  res <- readRDS("expPl.rds")
  res <- list(res[[1]], res[[2]], res[[3]])

  expect_equal(x, res)

})
