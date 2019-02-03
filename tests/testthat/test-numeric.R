context("Numeric metrics")

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

test_that('rmse', {
  expect_equal(
    rmse(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    sqrt(mean((ex_dat$obs - ex_dat$pred)^2))
  )
  expect_equal(
    rmse(ex_dat, truth = obs, estimate = "pred_na")[[".estimate"]],
    sqrt(mean((ex_dat$obs[-ind] - ex_dat$pred[-ind])^2))
  )
})

###################################################################

test_that('R^2', {
  expect_equal(
    rsq(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    cor(ex_dat[, 1:2])[1,2]^2
  )
  expect_equal(
    rsq(ex_dat, truth = "obs", estimate = "pred_na")[[".estimate"]],
    cor(ex_dat[, c(1, 3)], use = "complete.obs")[1,2]^2
  )
  expect_equal(
    rsq(ex_dat, truth = "obs", estimate = "rand")[[".estimate"]],
    cor(ex_dat[, c(1, 4)])[1,2]^2
  )
  expect_equal(
    rsq(ex_dat, estimate = rand_na, truth = obs)[[".estimate"]],
    cor(ex_dat[, c(1, 5)], use = "complete.obs")[1,2]^2
  )
})

###################################################################

test_that('Traditional R^2', {
  expect_equal(
    rsq_trad(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    1 -
      (
        sum((ex_dat$obs - ex_dat$pred)^2)/
        sum((ex_dat$obs - mean(ex_dat$obs))^2)
      )
  )
  expect_equal(
    rsq_trad(ex_dat, truth = "obs", estimate = "pred_na")[[".estimate"]],
    1 -
      (
        sum((ex_dat$obs[-ind] - ex_dat$pred[-ind])^2)/
        sum((ex_dat$obs[-ind] - mean(ex_dat$obs[-ind]))^2)
      )
  )
  expect_equal(
    rsq_trad(ex_dat, truth = "obs", estimate = rand)[[".estimate"]],
    1 -
      (
        sum((ex_dat$obs - ex_dat$rand)^2)/
        sum((ex_dat$obs - mean(ex_dat$obs))^2)
      )
  )
  expect_equal(
    rsq_trad(ex_dat, obs, rand_na)[[".estimate"]],
    1 -
      (
        sum((ex_dat$obs[-ind] - ex_dat$rand[-ind])^2)/
        sum((ex_dat$obs[-ind] - mean(ex_dat$obs[-ind]))^2)
      )
  )
})


###################################################################

test_that('Mean Abs Deviation', {
  expect_equal(
    mae(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    mean(abs(ex_dat$obs - ex_dat$pred))
  )
  expect_equal(
    mae(ex_dat, obs, pred_na)[[".estimate"]],
    mean(abs(ex_dat$obs[-ind] - ex_dat$pred[-ind]))
  )
})


###################################################################

test_that('Mean Absolute Percentage Error', {
  expect_equal(
    mape(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    100 * mean(abs((ex_dat$obs - ex_dat$pred)/ex_dat$obs))
  )
  expect_equal(
    mape(ex_dat, obs, pred_na)[[".estimate"]],
    100 * mean(abs((ex_dat$obs[-ind] - ex_dat$pred[-ind])/ex_dat$obs[-ind]))
  )
})


###################################################################

test_that('Symmetric Mean Absolute Percentage Error', {
  expect_equal(
    smape(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    100 * mean(abs((ex_dat$obs - ex_dat$pred)/((abs(ex_dat$obs) + abs(ex_dat$pred))/2)))
  )
  expect_equal(
    smape(ex_dat, obs, pred_na)[[".estimate"]],
    100 * mean(abs((ex_dat$obs[-ind] - ex_dat$pred[-ind])/((abs(ex_dat$obs[-ind]) + abs(ex_dat$pred[-ind]))/2)))
  )
})

###################################################################

test_that('Concordance Correlation Coefficient', {
  expect_equal(
    ccc(ex_dat, truth = "obs", estimate = "pred", bias = TRUE)[[".estimate"]],
    # epiR::epi.ccc(x = ex_dat$obs, y = ex_dat$pred)
    0.8322669,
    tol = 0.001
  )
  expect_equal(
    ccc(ex_dat, truth = obs, estimate = "pred_na", bias = TRUE)[[".estimate"]],
    # epiR::epi.ccc(x = ex_dat$obs[-ind], y = ex_dat$pred_na[-ind])
    0.8161879,
    tol = 0.001
  )
})

###################################################################

test_that('rpd', {
  expect_equal(
    rpd(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    sd(ex_dat$obs) / (sqrt(mean((ex_dat$obs - ex_dat$pred)^2)))
  )
  expect_equal(
    rpd(ex_dat, truth = "obs", estimate = "pred_na")[[".estimate"]],
    sd(ex_dat$obs[-ind]) / (sqrt(mean((ex_dat$obs[-ind] - ex_dat$pred[-ind])^2)))
  )
})

###################################################################

test_that('rpiq', {
  expect_equal(
    rpiq(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    IQR(ex_dat$obs) / sqrt(mean((ex_dat$obs - ex_dat$pred)^2))
  )
  expect_equal(
    rpiq(ex_dat, truth = "obs", estimate = "pred_na")[[".estimate"]],
    IQR(ex_dat$obs[-ind]) / sqrt(mean((ex_dat$obs[-ind] - ex_dat$pred[-ind])^2))
  )
})


###################################################################

test_that('Integer columns are allowed', {
  # Issue #44
  ex_dat$obs <- as.integer(ex_dat$obs)
  expect_equal(
    rmse(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    sqrt(mean((ex_dat$obs - ex_dat$pred)^2))
  )
})

###################################################################

test_that('Huber Loss', {
  delta <- 2

  expect_equal(
    huber_loss(ex_dat, truth = "obs", estimate = "pred", delta = delta)[[".estimate"]],
    {
      a <- ex_dat$obs - ex_dat$pred
      mean(
        ifelse(abs(a) <= delta,
               0.5 * a^2,
               delta * (abs(a) - 0.5 * delta))
      )
    }
  )

  expect_equal(
    huber_loss(ex_dat, truth = "obs", estimate = "pred_na", delta = delta)[[".estimate"]],
    {
      a <- ex_dat$obs[-ind] - ex_dat$pred[-ind]
      mean(
        ifelse(abs(a) <= delta,
               0.5 * a^2,
               delta * (abs(a) - 0.5 * delta))
      )
    }
  )

  expect_error(
    huber_loss(ex_dat, truth = "obs", estimate = "pred_na", delta = -1),
    "`delta` must be a positive value."
  )

  expect_error(
    huber_loss(ex_dat, truth = "obs", estimate = "pred_na", delta = c(1,2)),
    "`delta` must be a single numeric value."
  )

})

###################################################################

test_that('Pseudo-Huber Loss', {
  delta <- 2
  expect_equal(
    huber_loss_pseudo(ex_dat, truth = "obs", estimate = "pred", delta = delta)[[".estimate"]],
    {
      a <- ex_dat$obs - ex_dat$pred
      mean(delta^2 * (sqrt(1 + (a / delta)^2) - 1))
    }
  )
  expect_equal(
    huber_loss_pseudo(ex_dat, truth = "obs", estimate = "pred_na", delta = delta)[[".estimate"]],
    {
      a <- ex_dat$obs[-ind] - ex_dat$pred[-ind]
      mean(delta^2 * (sqrt(1 + (a / delta)^2) - 1))
    }
  )

  expect_error(
    huber_loss_pseudo(ex_dat, truth = "obs", estimate = "pred_na", delta = -1),
    "`delta` must be a positive value."
  )

  expect_error(
    huber_loss_pseudo(ex_dat, truth = "obs", estimate = "pred_na", delta = c(1,2)),
    "`delta` must be a single numeric value."
  )
})

###################################################################

test_that('Mean Absolute Scaled Error', {

  s <- data.frame(list(y = c(5,4,3,2,1),
                       y_hat = c(1, 1, 1, 1, 1)))

  expect_equal(
    mase(s, truth = y, estimate = y_hat, m = 1L)[[".estimate"]],  2
  )

  expect_error(
    mase(s, truth = y, estimate = y_hat, m = "should be int"),
    "`m` must be a single integer value."
  )
})
