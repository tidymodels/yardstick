context("PR Curve/AUC")

# from:
# MLmetrics::PRAUC(two_class_example$Class1,
#                  ifelse(two_class_example$truth == lvls[1], 1, 0))
pr_val <- 0.942570731650901

test_that('PR AUC', {
  expect_equal(
    pr_auc(two_class_example, truth = "truth", "Class1")[[".estimate"]],
    pr_val
  )
  expect_equal(
    pr_auc(two_class_example, truth,  Class1)[[".estimate"]],
    pr_val
  )
})

# ------------------------------------------------------------------------------

# Known PR Curve result
pr_example <- data.frame(
  lab   = factor(c("Yes", "Yes", "No", "Yes"), levels = c("Yes", "No")),
  score = c(.9, .4, .35, .7)
)

pr_result <- list(
  .threshold = c(Inf, 0.9, 0.7, 0.4, 0.35),
  recall = c(0, 1/3, 2/3, 1, 1),
  precision = c(NA, 1, 1, 1, 0.75)
)

test_that('PR Curve', {
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

# macro, macro weighted, micro?
