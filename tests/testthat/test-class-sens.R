context("Sensitivity")

# ------------------------------------------------------------------------------

test_that('Two class', {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_equal(
    sens(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    231/258
  )
  expect_equal(
    sens(pathology, estimate = scan, truth = pathology)[[".estimate"]],
    231/258
  )
  expect_equal(
    sens(pathology, pathology, scan)[[".estimate"]],
    231/258
  )
  expect_equal(
    sens(path_tbl)[[".estimate"]],
    231/258
  )
  expect_equal(
    sens(pathology, truth = pathology, estimate = "scan_na")[[".estimate"]],
    230/256
  )
  expect_equal(
    sens(as.matrix(path_tbl))[[".estimate"]],
    231/258
  )
  expect_equal(
    sens(pathology, pathology, scan_na, na_rm = FALSE)[[".estimate"]],
    NA_real_
  )
})

test_that("`event_level = 'second'` works", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- relevel(df_rev$pathology, "norm")
  df_rev$scan <- relevel(df_rev$scan, "norm")

  expect_equal(
    sens_vec(df$pathology, df$scan),
    sens_vec(df_rev$pathology, df_rev$scan, event_level = "second")
  )
})


# ------------------------------------------------------------------------------

test_that('Three class', {
  multi_ex <- data_three_by_three()
  micro <- data_three_by_three_micro()

  # sens = recall
  expect_equal(
    sens(multi_ex, estimator = "macro")[[".estimate"]],
    macro_metric(recall_binary)
  )
  expect_equal(
    sens(multi_ex, estimator = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(recall_binary)
  )
  expect_equal(
    sens(multi_ex, estimator = "micro")[[".estimate"]],
    with(micro, sum(tp) / sum(tp + fp))
  )
})

# ------------------------------------------------------------------------------

test_that("Binary `sens()` returns `NA` with a warning when undefined (tp + fn = 0) (#98)", {
  levels <- c("a", "b")
  truth    <- factor(c("b", "b"), levels = levels)
  estimate <- factor(c("a", "b"), levels = levels)

  expect_warning(
    expect_equal(
      sens_vec(truth, estimate),
      NA_real_
    )
  )

  cnd <- rlang::catch_cnd(sens_vec(truth, estimate))
  expect_known_output(cat(cnd$message), test_path("test-class-sens-warning-binary.txt"), print = TRUE)
  expect_s3_class(cnd, "yardstick_warning_sens_undefined_binary")
})

test_that("Multiclass `sens()` returns averaged value with `NA`s removed + a warning when undefined (tp + fn = 0) (#98)", {
  levels <- c("a", "b", "c", "d")

  # When `d` is the event we get sens      = 0.5 = (tp = 1, fn = 1)
  # When `a` is the event we get sens      = 1   = (tp = 1, fn = 0)
  # When `b` is the event we get a warning = NA  = (tp = 0, fn = 0)
  # When `c` is the event we get a warning = NA  = (tp = 0, fn = 0)
  truth    <- factor(c("a", "d", "d"), levels = levels)
  estimate <- factor(c("a", "d", "c"), levels = levels)
  expect_warning(expect_equal(sens_vec(truth, estimate), 0.75))

  cnd <- rlang::catch_cnd(sens_vec(truth, estimate))
  expect_known_output(cat(cnd$message), test_path("test-class-sens-warning-multiclass.txt"), print = TRUE)
  expect_s3_class(cnd, "yardstick_warning_sens_undefined_multiclass")
})

test_that("`NA` is still returned if there are some undefined sens values but `na.rm = FALSE`", {
  levels <- c("a", "b", "c", "d")
  truth    <- factor(c("a", "d", "d"), levels = levels)
  estimate <- factor(c("a", NA, "c"), levels = levels)
  expect_equal(sens_vec(truth, estimate, na_rm = FALSE), NA_real_)
  expect_warning(sens_vec(truth, estimate, na_rm = FALSE), NA)
})
