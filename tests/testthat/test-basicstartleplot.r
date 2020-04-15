context("Testing the function basicstartleplot")

test_that("Testing basicstartleplot", {

  df <- readRDS("results3.rds")

  x <- basicStartlePlot(df)

  res <- readRDS("expPl.rds")
  res <- res[[4]]

  expect_equal(x, res)

})
