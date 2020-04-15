context("Testing the function summariseStartle")

test_that("Testing summariseStartle", {

  df <- readRDS("results3.rds")

  x <- summariseStartle(df)
  y <- x$Ratio
  z <- x$RatioS
  x <- x$Values

  x <- sum(mean(x$`mean(value)`), mean(x$`sd(value)`), mean(x$`median(value)`), mean(x$`IQR(value)`))
  y <- sum(mean(y$`mean(ratio)`), mean(y$`sd(ratio)`), mean(y$`median(ratio)`), mean(y$`IQR(ratio)`))
  z <- sum(mean(z$p), mean(z$p.adj))

  expect_equal(x, 61.2449876490168)
  expect_equal(y, 18.04905092283)
  expect_equal(z, 0.298716349690042)

})
