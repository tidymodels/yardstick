test_that('Two class - Powers paper', {
  lst <- data_powers()
  tabl_2_1 <- lst$tabl_2_1
  df_2_1 <- lst$df_2_1

  expect_equal(
    precision(df_2_1, truth = "truth", estimate = "prediction")[[".estimate"]],
    30/42
  )
  expect_equal(
    precision(tabl_2_1)[[".estimate"]],
    30/42
  )
  expect_equal(
    precision(df_2_1, truth = truth, estimate = pred_na)[[".estimate"]],
    26/37
  )
})

test_that("`event_level = 'second'` works", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- relevel(df_rev$pathology, "norm")
  df_rev$scan <- relevel(df_rev$scan, "norm")

  expect_equal(
    precision_vec(df$pathology, df$scan),
    precision_vec(df_rev$pathology, df_rev$scan, event_level = "second")
  )
})

# ------------------------------------------------------------------------------

test_that("Binary `precision()` returns `NA` with a warning when undefined (tp + fp = 0) (#98)", {
  truth <- factor("a", levels = c("a", "b"))
  estimate <- factor("b", levels = c("a", "b"))

  expect_warning(
    expect_equal(
      precision_vec(truth, estimate),
      NA_real_
    )
  )

  cnd <- rlang::catch_cnd(precision_vec(truth, estimate))
  expect_known_output(cat(cnd$message), test_path("test-class-precision-warning-binary.txt"), print = TRUE)
  expect_s3_class(cnd, "yardstick_warning_precision_undefined_binary")
})

test_that("Multiclass `precision()` returns averaged value with `NA`s removed + a warning when undefined (tp + fp = 0) (#98)", {
  levels <- c("a", "b", "c", "d")

  # When `d` is the event we get precision = 0 = 0 / (0 + 3)
  # When `a` is the event we get a warning
  # When `b` is the event we get a warning
  # When `c` is the event we get a warning
  truth <- factor(c("a", "b", "c"), levels = levels)
  estimate <- factor(rep("d", 3), levels)
  expect_warning(expect_equal(precision_vec(truth, estimate), 0))

  # When `d` is the event we get precision = 0
  # When `a` is the event we get precision = 1
  # When `b` is the event we get precision = 0
  # When `c` is the event we get a warning
  truth <- factor(c("a", "b", "c"), levels = levels)
  estimate <- factor(c("a", "d", "b"), levels)
  expect_warning(expect_equal(precision_vec(truth, estimate), 1/3))

  cnd <- rlang::catch_cnd(precision_vec(truth, estimate))
  expect_known_output(cat(cnd$message), test_path("test-class-precision-warning-multiclass.txt"), print = TRUE)
  expect_s3_class(cnd, "yardstick_warning_precision_undefined_multiclass")
})

test_that("`NA` is still returned if there are some undefined precision values but `na.rm = FALSE`", {
  levels <- c("a", "b", "c", "d")
  truth <- factor(c("a", "b", "c"), levels = levels)
  estimate <- factor(c("a", NA, "c"), levels)
  expect_equal(precision_vec(truth, estimate, na_rm = FALSE), NA_real_)
  expect_warning(precision_vec(truth, estimate, na_rm = FALSE), NA)
})

# sklearn compare --------------------------------------------------------------

test_that('Two class - sklearn equivalent', {
  py_res <- read_pydata("py-precision")
  r_metric <- precision

  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )
})

test_that('Multi class - sklearn equivalent', {
  py_res <- read_pydata("py-precision")
  r_metric <- precision

  expect_equal(
    r_metric(hpc_cv, obs, pred)[[".estimate"]],
    py_res$macro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, "micro")[[".estimate"]],
    py_res$micro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, "macro_weighted")[[".estimate"]],
    py_res$weighted
  )
})
