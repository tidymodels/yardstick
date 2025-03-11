test_that("Two class PR Curve", {
  # Known PR Curve result
  pr_example <- data.frame(
    lab = factor(c("Yes", "Yes", "No", "Yes"), levels = c("Yes", "No")),
    score = c(.9, .4, .35, .7)
  )

  pr_result <- list(
    .threshold = c(Inf, 0.9, 0.7, 0.4, 0.35),
    recall = c(0, 1 / 3, 2 / 3, 1, 1),
    precision = c(1, 1, 1, 1, 0.75)
  )

  expect_equal(
    as.list(pr_curve(pr_example, truth = "lab", "score")),
    pr_result
  )
})

test_that("Multiclass PR Curve", {
  res <- pr_curve(hpc_cv, obs, VF:L)

  expect_equal(
    colnames(res),
    c(".level", ".threshold", "recall", "precision")
  )

  expect_equal(
    unique(res$.level),
    levels(hpc_cv$obs)
  )
})

# ------------------------------------------------------------------------------
# More tests involving issue #93

# These are worked out examples with hand written known answers

test_that("PR - perfect separation", {
  truth <- factor(c("x", "x", "y", "y"))
  prob <- c(.9, .8, .4, .3)

  data <- data.frame(truth, prob)
  val_curve <- pr_curve(data, truth, prob)
  val_auc <- pr_auc(data, truth, prob)

  expect_equal(
    val_curve$recall,
    c(0, .5, 1, 1, 1)
  )

  expect_equal(
    val_curve$precision,
    c(1, 1, 1, 2 / 3, 1 / 2)
  )

  expect_equal(
    val_curve$.threshold,
    c(Inf, .9, .8, .4, .3)
  )

  expect_equal(
    val_auc$.estimate,
    1
  )
})

test_that("PR - perfect separation - duplicates probs at the end", {
  truth <- factor(c("x", "x", "y", "y"))
  prob <- c(.9, .8, .3, .3)

  data <- data.frame(truth, prob)
  val_curve <- pr_curve(data, truth, prob)
  val_auc <- pr_auc(data, truth, prob)

  expect_equal(
    val_curve$recall,
    c(0, .5, 1, 1)
  )

  expect_equal(
    val_curve$precision,
    c(1, 1, 1, 1 / 2)
  )

  expect_equal(
    val_curve$.threshold,
    c(Inf, .9, .8, .3)
  )

  expect_equal(
    val_auc$.estimate,
    1
  )
})

test_that("PR - perfect separation - duplicates probs at the start", {
  truth <- factor(c("x", "x", "y", "y"))
  prob <- c(.9, .9, .4, .3)

  data <- data.frame(truth, prob)
  val_curve <- pr_curve(data, truth, prob)
  val_auc <- pr_auc(data, truth, prob)

  expect_equal(
    val_curve$recall,
    c(0, 1, 1, 1)
  )

  expect_equal(
    val_curve$precision,
    c(1, 1, 2 / 3, 1 / 2)
  )

  expect_equal(
    val_curve$.threshold,
    c(Inf, .9, .4, .3)
  )

  expect_equal(
    val_auc$.estimate,
    1
  )
})

test_that("PR - same class prob, different prediction value", {
  # x class prob .9
  # y class prob .9
  truth <- factor(c("x", "y", "y", "x", "x"))
  prob <- c(.9, .9, .8, .4, .3)

  data <- data.frame(truth, prob)
  val_curve <- pr_curve(data, truth, prob)
  val_auc <- pr_auc(data, truth, prob)

  expect_equal(
    val_curve$recall,
    c(0, 1 / 3, 1 / 3, 2 / 3, 1)
  )

  expect_equal(
    val_curve$precision,
    c(1, 1 / 2, 1 / 3, 1 / 2, 3 / 5)
  )

  expect_equal(
    val_curve$.threshold,
    c(Inf, .9, .8, .4, .3)
  )

  expect_equal(
    val_auc$.estimate,
    0.572222222222222
  )
})

# ------------------------------------------------------------------------------

test_that("PR - zero row data frame works", {
  df <- data.frame(y = factor(levels = c("a", "b")), x = double())

  expect <- dplyr::tibble(
    .threshold = Inf,
    recall = 0,
    precision = 1
  )

  class(expect) <- c("pr_df", class(expect))

  expect_snapshot(
    out <- pr_curve(df, y, x)
  )

  expect_identical(out, expect)
})

test_that("PR - No `truth` gives `NaN` recall values", {
  df <- data.frame(
    y = factor(c("b", "b"), levels = c("a", "b")),
    x = c(.1, .2)
  )

  expect_warning({
    curve <- pr_curve(df, y, x)
  })

  expect_identical(curve$recall, c(0, NaN, NaN))
})

# ------------------------------------------------------------------------------

test_that("grouped multiclass (one-vs-all) weighted example matches expanded equivalent", {
  hpc_cv$weight <- rep(1, times = nrow(hpc_cv))
  hpc_cv$weight[c(100, 200, 150, 2)] <- 5

  hpc_cv <- dplyr::group_by(hpc_cv, Resample)

  hpc_cv_expanded <- hpc_cv[
    vec_rep_each(seq_len(nrow(hpc_cv)), times = hpc_cv$weight),
  ]

  expect_identical(
    pr_curve(hpc_cv, obs, VF:L, case_weights = weight),
    pr_curve(hpc_cv_expanded, obs, VF:L)
  )
})

# ------------------------------------------------------------------------------

test_that("zero weights don't affect the curve", {
  # If they weren't removed, we'd get a `NaN` from a division by zero issue
  df <- dplyr::tibble(
    truth = factor(c("b", "a", "b", "a", "a"), levels = c("a", "b")),
    a = c(.75, .7, .4, .9, .8),
    weight = c(0, 1, 3, 0, 5)
  )

  expect_identical(
    pr_curve(df, truth, a, case_weights = weight),
    pr_curve(df[df$weight != 0, ], truth, a, case_weights = weight)
  )
})

test_that("errors with class_pred input", {
  skip_if_not_installed("probably")

  cp_truth <- probably::as_class_pred(two_class_example$truth, which = 1)
  fct_truth <- two_class_example$truth
  fct_truth[1] <- NA

  estimate <- two_class_example$Class1

  expect_snapshot(
    error = TRUE,
    pr_curve_vec(cp_truth, estimate)
  )
})

# ------------------------------------------------------------------------------

test_that("Binary results are the same as scikit-learn", {
  curve <- pr_curve(two_class_example, truth, Class1)

  expect_identical(
    curve,
    read_pydata("py-pr-curve")$binary
  )
})

test_that("Binary weighted results are the same as scikit-learn", {
  two_class_example$weight <- read_weights_two_class_example()

  curve <- pr_curve(two_class_example, truth, Class1, case_weights = weight)

  expect_identical(
    curve,
    read_pydata("py-pr-curve")$case_weight$binary
  )
})

# na_rm ------------------------------------------------------------------------

test_that("na_rm = FALSE errors if missing values are present", {
  df <- two_class_example
  df$Class1[1] <- NA

  expect_snapshot(
    error = TRUE,
    pr_curve_vec(df$truth, df$Class1, na_rm = FALSE)
  )
})
