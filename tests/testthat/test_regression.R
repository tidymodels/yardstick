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
    rmse(ex_dat, truth = "obs", estimate = "pred"),
    sqrt(mean((ex_dat$obs - ex_dat$pred)^2))
  )
  expect_equal(
    rmse(ex_dat, truth = obs, estimate = "pred_na"),
    sqrt(mean((ex_dat$obs[-ind] - ex_dat$pred[-ind])^2))
  )
})

###################################################################

test_that('R^2', {
  expect_equal(
    rsq(ex_dat, truth = "obs", estimate = "pred"),
    cor(ex_dat[, 1:2])[1,2]^2
  )
  expect_equal(
    rsq(ex_dat, truth = "obs", estimate = "pred_na"),
    cor(ex_dat[, c(1, 3)], use = "complete.obs")[1,2]^2
  )
  expect_equal(
    rsq(ex_dat, truth = "obs", estimate = "rand"),
    cor(ex_dat[, c(1, 4)])[1,2]^2
  )
  expect_equal(
    rsq(ex_dat, estimate = rand_na, truth = obs),
    cor(ex_dat[, c(1, 5)], use = "complete.obs")[1,2]^2
  )
})

###################################################################

test_that('Traditional R^2', {
  expect_equal(
    rsq_trad(ex_dat, truth = "obs", estimate = "pred"),
    1 -
      (
        sum((ex_dat$obs - ex_dat$pred)^2)/
        sum((ex_dat$obs - mean(ex_dat$obs))^2)
      )
  )
  expect_equal(
    rsq_trad(ex_dat, truth = "obs", estimate = "pred_na"),
    1 -
      (
        sum((ex_dat$obs[-ind] - ex_dat$pred[-ind])^2)/
        sum((ex_dat$obs[-ind] - mean(ex_dat$obs[-ind]))^2)
      )
  )
  expect_equal(
    rsq_trad(ex_dat, truth = "obs", estimate = rand),
    1 -
      (
        sum((ex_dat$obs - ex_dat$rand)^2)/
        sum((ex_dat$obs - mean(ex_dat$obs))^2)
      )
  )
  expect_equal(
    rsq_trad(ex_dat, obs, rand_na),
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
    mae(ex_dat, truth = "obs", estimate = "pred"),
    mean(abs(ex_dat$obs - ex_dat$pred))
  )
  expect_equal(
    mae(ex_dat, obs, pred_na),
    mean(abs(ex_dat$obs[-ind] - ex_dat$pred[-ind]))
  )
})


###################################################################

test_that('Mean Abs % Error', {
  expect_equal(
    mape(ex_dat, truth = "obs", estimate = "pred"),
    100 * mean(abs((ex_dat$obs - ex_dat$pred)/ex_dat$obs))
  )
  expect_equal(
    mape(ex_dat, obs, pred_na),
    100 * mean(abs((ex_dat$obs[-ind] - ex_dat$pred[-ind])/ex_dat$obs[-ind]))
  )
})


###################################################################

test_that('Concordance Correlation Coefficient', {
  expect_equal(
    ccc(ex_dat, truth = "obs", estimate = "pred", bias = TRUE),
    # epiR::epi.ccc(x = ex_dat$obs, y = ex_dat$pred)
    0.8322669,
    tol = 0.001
  )
  expect_equal(
    ccc(ex_dat, truth = obs, estimate = "pred_na", bias = TRUE),
    # epiR::epi.ccc(x = ex_dat$obs[-ind], y = ex_dat$pred_na[-ind])
    0.8161879,
    tol = 0.001
  )
})
