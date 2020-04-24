context("Testing Basic plot")
library(ggplot2)

test_that("Testing output of basicStartlePlot", {

  set.seed(5)
  data <- list("CTR 1" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)),
               "CTR 2" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)),
               "EXP 1" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)),
               "EXP 2" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)))
  mass <- data.frame("group" = c("CTR 1", "CTR 2", "EXP 1", "EXP 2"), "mass" = c(300, 350, 280, 330))

  df <- loadStartleData(auto_import=FALSE, data = data, mass = mass)

  bpl <- basicStartlePlot(df, filter_groups = c("ctr 1", "ctr 2"), n_col = 2)

  bpl <- layer_data(bpl)

  expect_equal(bpl, readRDS("bpl.rds"))


})



