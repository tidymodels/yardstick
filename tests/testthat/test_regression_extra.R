library(testthat)
library(yardstick)

set.seed(1812)
ex_dat <- data.frame(obs = rnorm(50))
ex_dat$pred <- .2 + 1.1 * ex_dat$obs + rnorm(50, sd = 0.5)
ex_dat$pred_na <- ex_dat$pred
ind <- (1:5)*10
ex_dat$pred_na[ind] <- NA
ex_dat$rand <- sample(ex_dat$pred)
ex_dat$rand_na <- ex_dat$rand
ex_dat$rand_na[ind] <- NA

###################################################################

test_that('rpd', {
  expect_equal(
    rpd(ex_dat, truth = "obs", estimate = "pred"),
    sd(ex_dat$obs) / (sqrt(mean((ex_dat$obs - ex_dat$pred)^2)))
  )
  expect_equal(
    rpd(ex_dat, truth = "obs", estimate = "pred_na"),
    sd(ex_dat$obs[-ind]) / (sqrt(mean((ex_dat$obs[-ind] - ex_dat$pred[-ind])^2)))
  )
})

###################################################################

test_that('rpiq', {
  expect_equal(
    rpiq(ex_dat, truth = "obs", estimate = "pred"),
    IQR(ex_dat$obs) / sqrt(mean((ex_dat$obs - ex_dat$pred)^2))
  )
  expect_equal(
    rpiq(ex_dat, truth = "obs", estimate = "pred_na"),
    IQR(ex_dat$obs[-ind]) / sqrt(mean((ex_dat$obs[-ind] - ex_dat$pred[-ind])^2))
  )
})
