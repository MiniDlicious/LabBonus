context("Regression Comparison")
library(MASS)
library(nycflights13)

test_that("Coefficients are similar", {
  mass_ridge <- lm.ridge(formula=air_time~dep_delay+arr_delay, data=flights)
  awesome_ridge <- ridgereg$new(formula = air_time~dep_delay + arr_delay, data = flights, lambda = 1)
  expect_equal(mass_ridge$coef ,awesome_ridge$coef(), tolerance=1e-3)
})