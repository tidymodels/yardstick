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

test_that("yardstick correlation warnings are thrown", {
  cnd <- rlang::catch_cnd(rsq_vec(c(1, 2), c(1, 1)))
  expect_is(cnd, "yardstick_warning_correlation_undefined_constant_estimate")

  cnd <- rlang::catch_cnd(rsq_vec(c(1, 1), c(1, 2)))
  expect_is(cnd, "yardstick_warning_correlation_undefined_constant_truth")
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

test_that('Mean Percentage Error', {
  expect_equal(
    mpe(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    100 * mean((ex_dat$obs - ex_dat$pred)/ex_dat$obs)
  )
  expect_equal(
    mpe(ex_dat, obs, pred_na)[[".estimate"]],
    100 * mean((ex_dat$obs[-ind] - ex_dat$pred[-ind])/ex_dat$obs[-ind])
  )
  expect_equal(
    mpe(data.frame(obs=0, pred=1), obs, pred)[[".estimate"]],
    -Inf
  )
  expect_equal(
    mpe(data.frame(obs=0, pred=-1), obs, pred)[[".estimate"]],
    Inf
  )
  expect_equal(
    mpe(data.frame(obs=0, pred=0), obs, pred)[[".estimate"]],
    NaN
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
    "`delta` must be a positive value.",
    class = "dplyr_error"
  )

  expect_error(
    huber_loss(ex_dat, truth = "obs", estimate = "pred_na", delta = c(1,2)),
    "`delta` must be a single numeric value.",
    class = "dplyr_error"
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
    "`delta` must be a positive value.",
    class = "dplyr_error"
  )

  expect_error(
    huber_loss_pseudo(ex_dat, truth = "obs", estimate = "pred_na", delta = c(1,2)),
    "`delta` must be a single numeric value.",
    class = "dplyr_error"
  )
})

###################################################################

test_that('Mean Absolute Scaled Error', {

  truth <- ex_dat$obs
  pred  <- ex_dat$pred

  truth_lag <- dplyr::lag(truth, 1L)
  naive_error <- truth - truth_lag
  mae_denom <- mean(abs(naive_error)[-1])
  scaled_error <- (truth - pred) / mae_denom
  known_mase <- mean(abs(scaled_error))

  m <- 2

  truth_lag <- dplyr::lag(truth, m)
  naive_error <- truth - truth_lag
  mae_denom <- mean(abs(naive_error)[-c(1, 2)])
  scaled_error <- (truth - pred) / mae_denom
  known_mase_with_m <- mean(abs(scaled_error))

  mae_train <- .5

  mae_denom <- mae_train
  scaled_error <- (truth - pred) / mae_denom
  known_mase_with_mae_train <- mean(abs(scaled_error))

  expect_equal(
    mase(ex_dat, obs, pred)[[".estimate"]],
    known_mase
  )

  expect_equal(
    mase(ex_dat, obs, pred, m = 2)[[".estimate"]],
    known_mase_with_m
  )

  expect_equal(
    mase(ex_dat, obs, pred, mae_train = mae_train)[[".estimate"]],
    known_mase_with_mae_train
  )

  expect_error(
    mase_vec(truth, pred, m = "x"),
    "`m` must be a single positive integer value."
  )

  expect_error(
    mase_vec(truth, pred, m = -1),
    "`m` must be a single positive integer value."
  )

  expect_error(
    mase_vec(truth, pred, m = 1.5),
    "`m` must be a single positive integer value."
  )

  expect_error(
    mase_vec(truth, pred, mae_train = -1),
    "`mae_train` must be a single positive numeric value."
  )

  expect_error(
    mase_vec(truth, pred, mae_train = "x"),
    "`mae_train` must be a single positive numeric value."
  )

})

###################################################################

# All tests confirmed against the software:
# http://www.insilico.eu/coral/SOFTWARECORAL.html

test_that("iic() returns known correct results", {
  expect_equal(iic(ex_dat, obs, pred)[[".estimate"]], 0.43306222006167)
})

test_that("iic() can be negative", {
  expect_equal(iic_vec(c(1, 2, 3), c(2, 1, 1)), -0.577350269189626)
})

test_that("iic() is NaN if truth/estimate are equivalent", {
  expect_equal(iic_vec(c(1, 2), c(1, 2)), NaN)
})

test_that("yardstick correlation warnings are thrown", {
  cnd <- rlang::catch_cnd(iic_vec(c(1, 2), c(1, 1)))
  expect_is(cnd, "yardstick_warning_correlation_undefined_constant_estimate")

  cnd <- rlang::catch_cnd(iic_vec(c(1, 1), c(1, 2)))
  expect_is(cnd, "yardstick_warning_correlation_undefined_constant_truth")
})

###################################################################
