test_that("Calculations are correct - two class", {
  lst <- data_altman()
  pathology <- lst$pathology

  sens <- 231 / 258
  spec <- 54 / 86
  H <- sens
  Fa <- 1 - spec
  expected <- (log(Fa) - log(H) - log(1 - Fa) + log(1 - H)) /
    (log(Fa) + log(H) + log(1 - Fa) + log(1 - H))

  expect_equal(
    sedi_vec(truth = pathology$pathology, estimate = pathology$scan),
    expected
  )
})

test_that("Perfect predictions yield SEDI of 1", {
  truth <- factor(c("a", "a", "b", "b"), levels = c("a", "b"))
  estimate <- truth
  expect_equal(sedi_vec(truth, estimate), 1, tolerance = 1e-6)
})

test_that("Random predictions yield SEDI near 0", {
  # When sens ≈ 1 - spec (i.e. H ≈ F), SEDI → 0
  set.seed(42)
  n <- 10000
  truth <- factor(sample(c("a", "b"), n, replace = TRUE), levels = c("a", "b"))
  estimate <- factor(sample(c("a", "b"), n, replace = TRUE), levels = c("a", "b"))
  result <- sedi_vec(truth, estimate)
  expect_true(abs(result) < 0.1)
})

test_that("SEDI rejects multiclass input", {
  truth <- factor(c("a", "b", "c"), levels = c("a", "b", "c"))
  estimate <- factor(c("a", "b", "a"), levels = c("a", "b", "c"))
  expect_error(
    sedi_vec(truth, estimate),
    "binary"
  )
})

test_that("All interfaces give the same results", {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl
  path_mat <- unclass(path_tbl)

  exp <- sedi_vec(pathology$pathology, pathology$scan)

  expect_identical(
    sedi(path_tbl)[[".estimate"]],
    exp
  )
  expect_identical(
    sedi(path_mat)[[".estimate"]],
    exp
  )
  expect_identical(
    sedi(pathology, truth = pathology, estimate = scan)[[".estimate"]],
    exp
  )
})

test_that("Calculations handle NAs", {
  lst <- data_altman()
  pathology <- lst$pathology

  sens <- 230 / 256
  spec <- 53 / 85
  H <- sens
  Fa <- 1 - spec
  expected <- (log(Fa) - log(H) - log(1 - Fa) + log(1 - H)) /
    (log(Fa) + log(H) + log(1 - Fa) + log(1 - H))

  expect_equal(
    sedi_vec(truth = pathology$pathology, estimate = pathology$scan_na),
    expected
  )
})

test_that("Case weights calculations are correct", {
  df <- data.frame(
    truth = factor(c("x", "x", "y"), levels = c("x", "y")),
    estimate = factor(c("x", "y", "x"), levels = c("x", "y")),
    case_weights = c(1L, 10L, 2L)
  )

  result <- sedi(df, truth, estimate, case_weights = case_weights)[[".estimate"]]
  expect_type(result, "double")
  expect_true(result >= -1 && result <= 1)
})

test_that("work with class_pred input", {
  skip_if_not_installed("probably")

  cp_truth <- probably::as_class_pred(two_class_example$truth, which = 1)
  cp_estimate <- probably::as_class_pred(two_class_example$predicted, which = 2)

  fct_truth <- two_class_example$truth
  fct_truth[1] <- NA

  fct_estimate <- two_class_example$predicted
  fct_estimate[2] <- NA

  expect_identical(
    sedi_vec(fct_truth, cp_estimate),
    sedi_vec(fct_truth, fct_estimate)
  )

  expect_identical(
    sedi_vec(fct_truth, cp_estimate, na_rm = FALSE),
    NA_real_
  )

  expect_snapshot(
    error = TRUE,
    sedi_vec(cp_truth, cp_estimate)
  )
})

test_that("works with hardhat case weights", {
  lst <- data_altman()
  df <- lst$pathology
  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    sedi_vec(df$pathology, df$scan, case_weights = imp_wgt)
  )

  expect_no_error(
    sedi_vec(df$pathology, df$scan, case_weights = freq_wgt)
  )
})

test_that("na_rm argument check", {
  expect_snapshot(
    error = TRUE,
    sedi_vec(1, 1, na_rm = "yes")
  )
})

test_that("`event_level = 'second'` works", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- stats::relevel(df_rev$pathology, "norm")
  df_rev$scan <- stats::relevel(df_rev$scan, "norm")

  expect_equal(
    sedi_vec(df$pathology, df$scan),
    sedi_vec(df_rev$pathology, df_rev$scan, event_level = "second")
  )
})

test_that("SEDI is prevalence-independent", {
  # Same sens and spec should give same SEDI regardless of sample size
  # 10:90 prevalence
  truth1 <- factor(c(rep("a", 10), rep("b", 90)), levels = c("a", "b"))
  est1 <- factor(c(rep("a", 8), rep("b", 2), rep("b", 85), rep("a", 5)),
                 levels = c("a", "b"))


  # 1:999 prevalence (same rates, different N)
  truth2 <- factor(c(rep("a", 100), rep("b", 9000)), levels = c("a", "b"))
  est2 <- factor(c(rep("a", 80), rep("b", 20), rep("b", 8500), rep("a", 500)),
                 levels = c("a", "b"))

  # sens = 0.8, spec ≈ 0.944 for both
  expect_equal(
    sedi_vec(truth1, est1),
    sedi_vec(truth2, est2),
    tolerance = 0.01
  )
})

test_that("range values are correct", {
  direction <- metric_direction(sedi)
  range <- metric_range(sedi)
  perfect <- ifelse(direction == "minimize", range[1], range[2])

  df <- tibble::tibble(
    truth = factor(c("A", "A", "B", "B", "B")),
    off = factor(c("B", "B", "A", "A", "A"))
  )

  expect_equal(
    sedi_vec(df$truth, df$truth),
    perfect,
    tolerance = 1e-6
  )

  if (direction == "maximize") {
    expect_lt(sedi_vec(df$truth, df$off), perfect)
  }
})
