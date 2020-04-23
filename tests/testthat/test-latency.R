context("Testing Latency plot")
library(ggplot2)

test_that("Testing output of latency plot", {

  set.seed(5)
  data <- list("CTR 1" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)),
               "CTR 2" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)),
               "EXP 1" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)),
               "EXP 2" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)))
  mass <- data.frame("group" = c("CTR 1", "CTR 2", "EXP 1", "EXP 2"), "mass" = c(300, 350, 280, 330))

  df <- loadStartleData(auto_import=FALSE, data = data, mass = mass)

  l <- latencyPlot(df, addhead = 0.1)

  l$LatencyVsCycle <- layer_data(l$LatencyVsCycle)
  l$LatencyVsGroup <- layer_data(l$LatencyVsGroup)

  expect_equal(l, readRDS("lp.rds"))


})



