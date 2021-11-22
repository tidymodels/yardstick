test_that('Two class - Powers paper', {
  lst <- data_powers()
  tabl_2_1 <- lst$tabl_2_1
  df_2_1 <- lst$df_2_1

  expect_equal(
    f_meas(df_2_1, truth = "truth", estimate = "prediction")[[".estimate"]],
    0.5882353,
    tol = 0.0001
  )
  expect_equal(
    f_meas(tabl_2_1)[[".estimate"]],
    0.5882353,
    tol = 0.0001
  )
  expect_equal(
    f_meas(df_2_1, truth = truth, estimate = pred_na)[[".estimate"]],
    0.5652174,
    tol = 0.0001
  )
})

test_that("`event_level = 'second'` works", {
  lst <- data_powers()
  df <- lst$df_2_1

  df_rev <- df
  df_rev$truth <- relevel(df_rev$truth, "Irrelevant")
  df_rev$prediction <- relevel(df_rev$prediction, "Irrelevant")

  expect_equal(
    f_meas_vec(df$truth, df$prediction),
    f_meas_vec(df_rev$truth, df_rev$prediction, event_level = "second")
  )
})

# ------------------------------------------------------------------------------
# Issue #77

test_that("`NA` values propagate from binary `precision()`", {

  truth <- factor(c(rep("a", 2), rep("b", 2)))
  estimate <- factor(rep("b", length(truth)), levels(truth))

  expect_warning(
    expect_equal(
      precision_vec(truth, estimate),
      f_meas_vec(truth, estimate)
    )
  )

})

test_that("`NA` values propagate from binary `recall()`", {

  estimate <- factor(c(rep("a", 2), rep("b", 2)))
  truth <- factor(rep("b", length(estimate)), levels(estimate))

  expect_warning(
    expect_equal(
      recall_vec(truth, estimate),
      f_meas_vec(truth, estimate)
    )
  )

})

# ------------------------------------------------------------------------------

test_that("Binary `f_meas()` returns `NA` with a warning when recall is undefined (tp + fn = 0) (#98)", {
  levels <- c("a", "b")
  truth    <- factor(c("b", "b"), levels = levels)
  estimate <- factor(c("a", "b"), levels = levels)

  expect_warning(
    expect_equal(
      f_meas_vec(truth, estimate),
      NA_real_
    )
  )

  cnd <- rlang::catch_cnd(f_meas_vec(truth, estimate))
  expect_known_output(cat(cnd$message), test_path("test-class-f_meas-recall-warning-binary.txt"), print = TRUE)
  expect_s3_class(cnd, "yardstick_warning_recall_undefined_binary")
})

test_that("Binary `f_meas()` returns `NA` with a warning when precision is undefined (tp + fp = 0) (#98)", {
  levels <- c("a", "b")
  truth <- factor("a", levels = levels)
  estimate <- factor("b", levels = levels)

  expect_warning(
    expect_equal(
      f_meas_vec(truth, estimate),
      NA_real_
    )
  )

  cnd <- rlang::catch_cnd(f_meas_vec(truth, estimate))
  expect_known_output(cat(cnd$message), test_path("test-class-f_meas-precision-warning-binary.txt"), print = TRUE)
  expect_s3_class(cnd, "yardstick_warning_precision_undefined_binary")
})

test_that("Multiclass `f_meas()` returns averaged value with `NA`s removed + a warning when recall is undefined (tp + fn = 0) (#98)", {
  levels <- c("a", "b", "c")

  truth    <- factor(c("a", "b", "b"), levels = levels)
  estimate <- factor(c("a", "b", "c"), levels = levels)
  expect_warning(expect_equal(f_meas_vec(truth, estimate), 5/6))

  cnd <- rlang::catch_cnd(f_meas_vec(truth, estimate))
  expect_known_output(cat(cnd$message), test_path("test-class-f_meas-recall-warning-multiclass.txt"), print = TRUE)
  expect_s3_class(cnd, "yardstick_warning_recall_undefined_multiclass")
})

test_that("Multiclass `f_meas()` returns averaged value with `NA`s removed + a warning when precision is undefined (tp + fn = 0) (#98)", {
  levels <- c("a", "b", "c")

  truth    <- factor(c("a", "b", "c"), levels = levels)
  estimate <- factor(c("a", "b", "b"), levels = levels)
  expect_warning(expect_equal(f_meas_vec(truth, estimate), 5/6))

  cnd <- rlang::catch_cnd(f_meas_vec(truth, estimate))
  expect_known_output(cat(cnd$message), test_path("test-class-f_meas-precision-warning-multiclass.txt"), print = TRUE)
  expect_s3_class(cnd, "yardstick_warning_precision_undefined_multiclass")
})

test_that("`NA` is still returned if there are some undefined recall values but `na.rm = FALSE`", {
  levels <- c("a", "b", "c")
  truth    <- factor(c("a", "b", "b"), levels = levels)
  estimate <- factor(c("a", NA,  "c"), levels = levels)
  expect_equal(f_meas_vec(truth, estimate, na_rm = FALSE), NA_real_)
  expect_warning(f_meas_vec(truth, estimate, na_rm = FALSE), NA)
})

# sklearn compare --------------------------------------------------------------

test_that('Two class - sklearn equivalent', {
  py_res <- read_pydata("py-f_meas")
  py_res_.5 <- read_pydata("py-f_meas_beta_.5")
  r_metric <- f_meas

  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )
  expect_equal(
    r_metric(two_class_example, truth, predicted, beta = .5)[[".estimate"]],
    py_res_.5$binary
  )
})

test_that('Multi class - sklearn equivalent', {
  py_res <- read_pydata("py-f_meas")
  py_res_.5 <- read_pydata("py-f_meas_beta_.5")
  r_metric <- f_meas

  expect_equal(
    r_metric(hpc_cv, obs, pred)[[".estimate"]],
    py_res$macro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, estimator = "micro")[[".estimate"]],
    py_res$micro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, estimator = "macro_weighted")[[".estimate"]],
    py_res$weighted
  )

  expect_equal(
    r_metric(hpc_cv, obs, pred, beta = .5)[[".estimate"]],
    py_res_.5$macro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, estimator = "micro", beta = .5)[[".estimate"]],
    py_res_.5$micro
  )
  expect_equal(
    r_metric(hpc_cv, obs, pred, "macro_weighted", beta = .5)[[".estimate"]],
    py_res_.5$weighted
  )
})


