context("PR Curve/AUC")

# originally this test checked against:
# MLmetrics::PRAUC(two_class_example$Class1, ifelse(two_class_example$truth == lvls[1], 1, 0))
# 0.942570731650901
# but I think that this is actually incorrect after dealing with issue #93 and
# it has to do with the way end points are handled and how duplicates are handled
# scikit learn does it the correct way, so now we use that

# library(reticulate)
# skmetrics <- import("sklearn.metrics")
# sk_pr_curve <- skmetrics$precision_recall_curve(
#   y_true = two_class_example$truth,
#   probas_pred = two_class_example$Class1,
#   pos_label = "Class1"
# )
# names(sk_pr_curve) <- c("precision", "recall", "thresholds")
# yardstick:::auc(sk_pr_curve$recall, sk_pr_curve$precision)

test_that('Two class PR AUC', {
  pr_val <- 0.946446700643149

  expect_equal(
    pr_auc(two_class_example, truth = "truth", "Class1")[[".estimate"]],
    pr_val
  )
  expect_equal(
    pr_auc(two_class_example, truth,  Class1)[[".estimate"]],
    pr_val
  )
})

test_that("`event_level = 'second'` works", {
  df <- two_class_example

  df_rev <- df
  df_rev$truth <- relevel(df_rev$truth, "Class2")

  expect_equal(
    pr_auc_vec(df$truth, df$Class1),
    pr_auc_vec(df_rev$truth, df_rev$Class1, event_level = "second")
  )
})

# ------------------------------------------------------------------------------

test_that('Two class PR Curve', {
  # Known PR Curve result
  pr_example <- data.frame(
    lab   = factor(c("Yes", "Yes", "No", "Yes"), levels = c("Yes", "No")),
    score = c(.9, .4, .35, .7)
  )

  pr_result <- list(
    .threshold = c(Inf, 0.9, 0.7, 0.4, 0.35),
    recall = c(0, 1/3, 2/3, 1, 1),
    precision = c(1, 1, 1, 1, 0.75)
  )

  expect_equal(
    as.list(pr_curve(pr_example, truth = "lab", "score")),
    pr_result
  )
})


test_that('Multiclass PR Curve', {
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

test_that("Multiclass PR AUC", {
  hpc_f1 <- data_hpc_fold1()

  expect_equal(
    pr_auc(hpc_f1, obs, VF:L, estimator = "macro")[[".estimate"]],
    prob_macro_metric(pr_auc_binary)
  )
  expect_equal(
    pr_auc(hpc_f1, obs, VF:L, estimator = "macro_weighted")[[".estimate"]],
    prob_macro_weighted_metric(pr_auc_binary)
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
    c(1, 1, 1, 2/3, 1/2)
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
    c(1, 1, 1, 1/2)
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
    c(1, 1, 2/3, 1/2)
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
    c(0, 1/3, 1/3, 2/3, 1)
  )

  expect_equal(
    val_curve$precision,
    c(1, 1/2, 1/3, 1/2, 3/5)
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

  expect_identical(
    expect_warning(pr_curve(df, y, x)),
    expect
  )
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
