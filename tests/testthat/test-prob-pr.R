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
  df_rev$truth <- stats::relevel(df_rev$truth, "Class2")

  expect_equal(
    pr_auc_vec(df$truth, df$Class1),
    pr_auc_vec(df_rev$truth, df_rev$Class1, event_level = "second")
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

