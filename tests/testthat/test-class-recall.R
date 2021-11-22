test_that('Two class - Powers paper', {
  lst <- data_powers()
  tabl_2_1 <- lst$tabl_2_1
  df_2_1 <- lst$df_2_1

  expect_equal(
    recall(df_2_1, truth = "truth", estimate = "prediction")[[".estimate"]],
    30/60
  )
  expect_equal(
    recall(tabl_2_1)[[".estimate"]],
    30/60
  )
  expect_equal(
    recall(df_2_1, truth = truth, estimate = pred_na)[[".estimate"]],
    26/(26+29)
  )
})

test_that("`event_level = 'second'` works", {
  lst <- data_powers()
  df <- lst$df_2_1

  df_rev <- df
  df_rev$truth <- relevel(df_rev$truth, "Irrelevant")
  df_rev$prediction <- relevel(df_rev$prediction, "Irrelevant")

  expect_equal(
    recall_vec(df$truth, df$prediction),
    recall_vec(df_rev$truth, df_rev$prediction, event_level = "second")
  )
})

# ------------------------------------------------------------------------------

test_that("Binary `recall()` returns `NA` with a warning when undefined (tp + fn = 0) (#98)", {
  levels <- c("a", "b")
  truth    <- factor(c("b", "b"), levels = levels)
  estimate <- factor(c("a", "b"), levels = levels)

  expect_warning(
    expect_equal(
      recall_vec(truth, estimate),
      NA_real_
    )
  )

  cnd <- rlang::catch_cnd(recall_vec(truth, estimate))
  expect_known_output(cat(cnd$message), test_path("test-class-recall-warning-binary.txt"), print = TRUE)
  expect_s3_class(cnd, "yardstick_warning_recall_undefined_binary")
})

test_that("Multiclass `recall()` returns averaged value with `NA`s removed + a warning when undefined (tp + fn = 0) (#98)", {
  levels <- c("a", "b", "c", "d")

  # When `d` is the event we get recall    = 0.5 = (tp = 1, fn = 1)
  # When `a` is the event we get recall    = 1   = (tp = 1, fn = 0)
  # When `b` is the event we get a warning = NA  = (tp = 0, fn = 0)
  # When `c` is the event we get a warning = NA  = (tp = 0, fn = 0)
  truth    <- factor(c("a", "d", "d"), levels = levels)
  estimate <- factor(c("a", "d", "c"), levels = levels)
  expect_warning(expect_equal(recall_vec(truth, estimate), 0.75))

  cnd <- rlang::catch_cnd(recall_vec(truth, estimate))
  expect_known_output(cat(cnd$message), test_path("test-class-recall-warning-multiclass.txt"), print = TRUE)
  expect_s3_class(cnd, "yardstick_warning_recall_undefined_multiclass")
})

test_that("`NA` is still returned if there are some undefined recall values but `na.rm = FALSE`", {
  levels <- c("a", "b", "c", "d")
  truth    <- factor(c("a", "d", "d"), levels = levels)
  estimate <- factor(c("a", NA, "c"), levels = levels)
  expect_equal(recall_vec(truth, estimate, na_rm = FALSE), NA_real_)
  expect_warning(recall_vec(truth, estimate, na_rm = FALSE), NA)
})

# sklearn compare --------------------------------------------------------------

test_that('Two class - sklearn equivalent', {
  py_res <- read_pydata("py-recall")
  r_metric <- recall

  expect_equal(
    r_metric(two_class_example, truth, predicted)[[".estimate"]],
    py_res$binary
  )
})

test_that('Multi class - sklearn equivalent', {
  py_res <- read_pydata("py-recall")
  r_metric <- recall

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
