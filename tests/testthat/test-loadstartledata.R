context("Testing the function loadStartleData")

test_that("Testing automated data loading", {

  df <- loadStartleData(correction = FALSE)

  expect_equal(df, readRDS("results1.rds"), check.attributes = FALSE)

})

test_that("Testing correction true", {

  df_withcorrection <- loadStartleData(correction = TRUE)

  expect_equal(df_withcorrection, readRDS("results2.rds"), check.attributes = FALSE)

})


test_that("Testing correction true addtail and addhead", {

  df_wcor_tailhead <- loadStartleData(correction = TRUE, addtail = 0.5, addhead = 0.5)

  expect_equal(df_wcor_tailhead, readRDS("results3.rds"), check.attributes = FALSE)

})


test_that("Auto import false", {
  set.seed(5)
  data <- list("CTR 1" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)),
               "CTR 2" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)),
               "EXP 1" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)),
               "EXP 2" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)))
  mass <- data.frame("group" = c("CTR 1", "CTR 2", "EXP 1", "EXP 2"), "mass" = c(300, 350, 280, 330))

  df_no_auto_import <- loadStartleData(auto_import=FALSE, data = data, mass = mass)

  expect_equal(df_no_auto_import, readRDS("results4.rds"), check.attributes = FALSE)


})



test_that("Auto import false", {
  set.seed(5)
  data <- list("CTR 1" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)),
               "CTR 2" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)),
               "EXP 1" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)),
               "EXP 2" = data.frame(x = seq(1, 210881, by=12), y = runif(17574, min=-30, max=30)))
  mass <- data.frame("group" = c("CTR 1", "CTR 2", "EXP 1", "EXP 2"), "mass" = c(300, 350, 280, 330))

  expect_error(loadStartleData(auto_import=FALSE, mass = mass))

  expect_error(loadStartleData(auto_import=FALSE, data = data))


})


