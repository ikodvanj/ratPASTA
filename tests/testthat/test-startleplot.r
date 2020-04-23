context("Testing startlePlot")
library(ggplot2)

test_that("Testing output of startle plot", {

  set.seed(5)
  data <- list("CTR 1" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)),
               "CTR 2" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)),
               "EXP 1" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)),
               "EXP 2" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)))
  mass <- data.frame("group" = c("CTR 1", "CTR 2", "EXP 1", "EXP 2"), "mass" = c(300, 350, 280, 330))

  df <- loadStartleData(auto_import=FALSE, data = data, mass = mass)

  pl <- startlePlot(df, type = 1)

  pl_d <- layer_data(pl)

  expect_equal(pl_d, readRDS("pl1.rds"))

  pl2 <- startlePlot(df, type = 2)

  pl_d2 <- layer_data(pl2)

  expect_equal(pl_d2, readRDS("pl2.rds"))

  pl3 <- startlePlot(df, type = 3)

  pl_d3 <- layer_data(pl3)

  expect_equal(pl_d3, readRDS("pl3.rds"))

  pl5 <- startlePlot(df, type = 5)

  pl_d5 <- layer_data(pl5)

  expect_equal(pl_d5, readRDS("pl5.rds"))

  pl6 <- startlePlot(df, type = 6)

  pl_d6 <- layer_data(pl6)

  expect_equal(pl_d6, readRDS("pl6.rds"))

})



test_that("Testing output of startle plot", {

  set.seed(5)
  data <- list("CTR 1" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)),
               "CTR 2" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)),
               "EXP 1" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)),
               "EXP 2" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)))
  mass <- data.frame("group" = c("CTR 1", "CTR 2", "EXP 1", "EXP 2"), "mass" = c(300, 350, 280, 330))

  df <- loadStartleData(auto_import=FALSE, data = data, mass = mass)

  expect_error(startlePlot(df),'Argument "type" is missing. Specifiy type to 1, 2 or 3.')

})
