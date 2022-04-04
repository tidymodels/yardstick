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

test_that('R^2', {
  expect_equal(
    rsq(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    stats::cor(ex_dat[, 1:2])[1,2]^2
  )
  expect_equal(
    rsq(ex_dat, truth = "obs", estimate = "pred_na")[[".estimate"]],
    stats::cor(ex_dat[, c(1, 3)], use = "complete.obs")[1,2]^2
  )
  expect_equal(
    rsq(ex_dat, truth = "obs", estimate = "rand")[[".estimate"]],
    stats::cor(ex_dat[, c(1, 4)])[1,2]^2
  )
  expect_equal(
    rsq(ex_dat, estimate = rand_na, truth = obs)[[".estimate"]],
    stats::cor(ex_dat[, c(1, 5)], use = "complete.obs")[1,2]^2
  )
})

test_that("yardstick correlation warnings are thrown", {
  cnd <- rlang::catch_cnd(rsq_vec(c(1, 2), c(1, 1)))
  expect_s3_class(cnd, "yardstick_warning_correlation_undefined_constant_estimate")

  cnd <- rlang::catch_cnd(rsq_vec(c(1, 1), c(1, 2)))
  expect_s3_class(cnd, "yardstick_warning_correlation_undefined_constant_truth")
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

test_that('Concordance Correlation Coefficient', {
  expect_equal(
    ccc(ex_dat, truth = "obs", estimate = "pred", bias = TRUE)[[".estimate"]],
    # epiR::epi.ccc(x = ex_dat$obs, y = ex_dat$pred)
    0.8322669,
    tolerance = 0.001
  )
  expect_equal(
    ccc(ex_dat, truth = obs, estimate = "pred_na", bias = TRUE)[[".estimate"]],
    # epiR::epi.ccc(x = ex_dat$obs[-ind], y = ex_dat$pred_na[-ind])
    0.8161879,
    tolerance = 0.001
  )
})

###################################################################

test_that('rpd', {
  expect_equal(
    rpd(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    stats::sd(ex_dat$obs) / (sqrt(mean((ex_dat$obs - ex_dat$pred)^2)))
  )
  expect_equal(
    rpd(ex_dat, truth = "obs", estimate = "pred_na")[[".estimate"]],
    stats::sd(ex_dat$obs[-ind]) / (sqrt(mean((ex_dat$obs[-ind] - ex_dat$pred[-ind])^2)))
  )
})

###################################################################

test_that('rpiq', {
  expect_equal(
    rpiq(ex_dat, truth = "obs", estimate = "pred")[[".estimate"]],
    stats::IQR(ex_dat$obs) / sqrt(mean((ex_dat$obs - ex_dat$pred)^2))
  )
  expect_equal(
    rpiq(ex_dat, truth = "obs", estimate = "pred_na")[[".estimate"]],
    stats::IQR(ex_dat$obs[-ind]) / sqrt(mean((ex_dat$obs[-ind] - ex_dat$pred[-ind])^2))
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
  expect_s3_class(cnd, "yardstick_warning_correlation_undefined_constant_estimate")

  cnd <- rlang::catch_cnd(iic_vec(c(1, 1), c(1, 2)))
  expect_s3_class(cnd, "yardstick_warning_correlation_undefined_constant_truth")
})

###################################################################
