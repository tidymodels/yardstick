# Perfect ----------------------------------------------------------------------

# Perfect gain capture with .5 threshold

test_that('Perfect gain capture', {
  df <- data.frame(
    truth = factor(c("Yes", "Yes", "No", "Yes", "No"), levels = c("Yes", "No")),
    estimate = c(.9, .8, .4, .68, .4)
  )

  expect_equal(
    gain_capture(df, truth, estimate)[[".estimate"]],
    1
  )

  # Antiperfect
  expect_equal(
    gain_capture(df, truth, estimate, event_level = "second")[[".estimate"]],
    -1
  )
})

# 1 Out of order ---------------------------------------------------------------

test_that('1 out of order', {
  # 1 element out of order (3)
  estimate2 <- c(.9, .8, .7, .68, .4)
  truth2 <- factor(c("Yes", "Yes", "No", "Yes", "No"), levels = c("Yes", "No"))
  df2 <- data.frame(truth2, estimate2)

  # triangle + rectangle - .5 = shaded area
  denom <- (3/5 * 1) / 2 + ((1 - 3/5) * 1) - .5

  # triangle + rect + (triangle + rect) + rect - .5 = area under black line
  # but above 45% line
  numer <-
    (.4 * 2/3) / 2 +
    ( (.6 - .4) * 2/3) +
    ((.8 - .6) * 2/3) + ((.8 - .6) * (1-2/3)) / 2 +
    ((1-.8) * 1) -
    .5

  expect_equal(
    gain_capture(df2, truth2, estimate2)[[".estimate"]],
    numer / denom
  )

  # Anti
  expect_equal(
    gain_capture(df2, truth2, estimate2, event_level = "second")[[".estimate"]],
    - numer / denom
  )
})

# Multiclass ---------------------------------------------------------------

test_that("Multiclass gain capture", {
  hpc_f1 <- data_hpc_fold1()

  expect_equal(
    gain_capture(hpc_f1, obs, VF:L, estimator = "macro")[[".estimate"]],
    prob_macro_metric(gain_capture_binary)
  )
  expect_equal(
    gain_capture(hpc_f1, obs, VF:L, estimator = "macro_weighted")[[".estimate"]],
    prob_macro_weighted_metric(gain_capture_binary)
  )
})

# Relationship with ROC AUC ----------------------------------------------------

test_that("gain_capture = 2 * ROCAUC - 1", {
  hpc_f1 <- data_hpc_fold1()

  # Binary case
  expect_equal(
    gain_capture(two_class_example, truth, Class1)[[".estimate"]],
    2 * roc_auc(two_class_example, truth, Class1)[[".estimate"]] - 1
  )

  # must be careful to weight appropriately in the multiclass case
  # must do (2 * ROCAUC - 1) BEFORE weighting
  roc_auc_unweighted <- yardstick:::roc_auc_multiclass(
    hpc_f1$obs,
    as.matrix(hpc_f1[,c("VF", "F", "M", "L")]),
    list()
  )

  truth_table <- matrix(table(hpc_f1$obs), nrow = 1)
  w <- yardstick:::get_weights(truth_table, "macro")

  expect_equal(
    gain_capture(hpc_f1, obs, VF:L, estimator = "macro")[[".estimate"]],
    weighted.mean(2 * roc_auc_unweighted - 1, w)
  )
})
