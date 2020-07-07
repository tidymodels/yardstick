context("Specificity")

# ------------------------------------------------------------------------------

test_that('Two class', {
  lst <- data_altman()
  pathology <- lst$pathology
  path_tbl <- lst$path_tbl

  expect_equal(
    spec(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    54/86
  )
  expect_equal(
    spec(path_tbl)[[".estimate"]],
    54/86
  )
  expect_equal(
    spec(pathology, truth = pathology, estimate = "scan_na")[[".estimate"]],
    53/85
  )
  expect_equal(
    spec(as.matrix(path_tbl))[[".estimate"]],
    54/86
  )
})

test_that("`event_level = 'second'` works", {
  lst <- data_altman()
  df <- lst$pathology

  df_rev <- df
  df_rev$pathology <- relevel(df_rev$pathology, "norm")
  df_rev$scan <- relevel(df_rev$scan, "norm")

  expect_equal(
    spec_vec(df$pathology, df$scan),
    spec_vec(df_rev$pathology, df_rev$scan, event_level = "second")
  )
})

# ------------------------------------------------------------------------------

test_that('Three class', {
  multi_ex <- data_three_by_three()
  micro <- data_three_by_three_micro()

  expect_equal(
    spec(multi_ex, estimator = "macro")[[".estimate"]],
    macro_metric(spec_binary)
  )
  expect_equal(
    spec(multi_ex, estimator = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(spec_binary)
  )
  expect_equal(
    spec(multi_ex, estimator = "micro")[[".estimate"]],
    with(micro, sum(tn) / sum(tn + fn))
  )
})

# ------------------------------------------------------------------------------

test_that("Binary `spec()` returns `NA` with a warning when undefined (tn + fp = 0) (#98)", {
  levels <- c("a", "b")
  truth    <- factor(c("a", "a"), levels = levels)
  estimate <- factor(c("a", "b"), levels = levels)

  expect_warning(
    expect_equal(
      spec_vec(truth, estimate),
      NA_real_
    )
  )

  cnd <- rlang::catch_cnd(spec_vec(truth, estimate))
  expect_known_output(cat(cnd$message), test_path("test-class-spec-warning-binary.txt"), print = TRUE)
  expect_s3_class(cnd, "yardstick_warning_spec_undefined_binary")
})

test_that("Multiclass `spec()` returns averaged value with `NA`s removed + a warning when undefined (tn + fp = 0) (#98)", {
  levels <- c("a", "b", "c", "d")

  # When `d` is the event we get spec      = 3/3  = (tn = 3, fp = 0)
  # When `a` is the event we get spec      = NA   = (tn = 0, fp = 0)
  # When `b` is the event we get a warning = 1/3  = (tn = 1, fp = 2)
  # When `c` is the event we get a warning = 3/3  = (tn = 3, fp = 0)
  truth    <- factor(c("a", "a", "a"), levels = levels)
  estimate <- factor(c("a", "b", "b"), levels = levels)
  expect_warning(expect_equal(spec_vec(truth, estimate), (1 + 1/3 + 1) / 3))

  cnd <- rlang::catch_cnd(spec_vec(truth, estimate))
  expect_known_output(cat(cnd$message), test_path("test-class-spec-warning-multiclass.txt"), print = TRUE)
  expect_s3_class(cnd, "yardstick_warning_spec_undefined_multiclass")
})

test_that("`NA` is still returned if there are some undefined spec values but `na.rm = FALSE`", {
  levels <- c("a", "b")
  truth    <- factor(c("a", "a"), levels = levels)
  estimate <- factor(c("a", NA), levels = levels)
  expect_equal(spec_vec(truth, estimate, na_rm = FALSE), NA_real_)
  expect_warning(spec_vec(truth, estimate, na_rm = FALSE), NA)
})
