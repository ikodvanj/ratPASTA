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

  pld <- layer_data(pl)
  plde <- readRDS("pl1.rds")

  n <- intersect(names(pld), names(plde))

  pld <- pld[,n]
  plde <- plde[,n]

  expect_is(pl, "ggplot")
  expect_equal(pld, plde)

  # type 2
  pl2 <- startlePlot(df, type = 2)

  pld2 <- layer_data(pl2)
  plde2 <- readRDS("pl2.rds")

  n2 <- intersect(names(pld2), names(plde2))

  pld2 <- pld2[,n2]
  plde2 <- plde2[,n2]

  expect_is(pl2, "ggplot")
  expect_equal(pld2, plde2)

  # type 3
  pl3 <- startlePlot(df, type = 3)

  pld3<- layer_data(pl3)
  plde3<- readRDS("pl3.rds")

  n3<- intersect(names(pld3), names(plde3))

  pld3<- pld3[,n3]
  plde3<- plde3[,n3]

  expect_is(pl3, "ggplot")
  expect_equal(pld3, plde3)

  # type 5
  pl5 <- startlePlot(df, type = 5)

  pld5<- layer_data(pl5)
  plde5<- readRDS("pl5.rds")

  n5<- intersect(names(pld5), names(plde5))

  pld5<- pld5[,n5]
  plde5<- plde5[,n5]

  expect_is(pl5, "ggplot")
  expect_equal(pld5, plde5)

  # type 6
  pl6 <- startlePlot(df, type = 6)

  pld6<- layer_data(pl6)
  plde6<- readRDS("pl6.rds")

  n6<- intersect(names(pld6), names(plde6))

  pld6<- pld6[,n6]
  plde6<- plde6[,n6]

  expect_is(pl6, "ggplot")
  expect_equal(pld6, plde6)

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
