context("Testing the function summariseStartle")

test_that("Testing summariseStartle", {

  df <- readRDS("results3.rds")

  x <- summariseStartle(df)
  y <- x$T2
  x <- x$T1

  x <- sum(mean(x$`mean(value)`), mean(x$`sd(value)`), mean(x$`median(value)`), mean(x$`IQR(value)`))
  y <- sum(mean(y$p), mean(y$p.adj))

  expect_equal(x, 61.2449876490168)
  expect_equal(y, 0.298716349690042)

})
