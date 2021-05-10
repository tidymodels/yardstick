context("ROC Curve/AUC")

# ------------------------------------------------------------------------------

test_that('Two class', {
  roc_curv <- pROC::roc(
    two_class_example$truth,
    two_class_example$Class1,
    levels = rev(levels(two_class_example$truth)),
    direction = "<"
  )

  lvls <- levels(two_class_example$truth)
  roc_val <- as.numeric(roc_curv$auc)

  smooth_curv <- pROC::roc(
    two_class_example$truth,
    two_class_example$Class1,
    levels = rev(levels(two_class_example$truth)),
    direction = "<",
    smooth = TRUE
  )

  expect_equal(
    roc_auc(two_class_example, truth, Class1)[[".estimate"]],
    roc_val
  )
  expect_equal(
    roc_auc(two_class_example, truth = "truth", Class1)[[".estimate"]],
    roc_val
  )
  expect_equal(
    roc_auc(two_class_example, truth, Class1, options = list(smooth = TRUE))[[".estimate"]],
    as.numeric(smooth_curv$auc),
    tol = 0.001
  )
})

test_that("`event_level = 'second'` works", {
  df <- two_class_example

  df_rev <- df
  df_rev$truth <- relevel(df_rev$truth, "Class2")

  expect_equal(
    roc_auc_vec(df$truth, df$Class1),
    roc_auc_vec(df_rev$truth, df_rev$Class1, event_level = "second")
  )
})

test_that("binary roc curve uses `direction = <`", {
  # In yardstick we do events (or cases) as the first level
  truth <- factor(c("control", "case", "case"), levels = c("case", "control"))

  # Make really bad predictions
  # This would force `direction = "auto"` to choose `>`,
  # which would be incorrect. We are required to force `direction = <` for
  # our purposes of having `estimate` match the event
  estimate <- c(.8, .2, .1)

  # pROC expects levels to be in the order of control, then event.
  roc <- pROC::roc(
    truth,
    estimate,
    levels = c("control", "case"),
    direction = "<"
  )

  curve <- roc_curve_vec(truth, estimate)

  expect_identical(curve$specificity, c(0, roc$specificities))
  expect_identical(curve$sensitivity, c(1, roc$sensitivities))
})

test_that('ROC Curve', {
  roc_curv <- pROC::roc(
    two_class_example$truth,
    two_class_example$Class1,
    levels = rev(levels(two_class_example$truth)),
    direction = "<"
  )

  smooth_curv <- pROC::roc(
    two_class_example$truth,
    two_class_example$Class1,
    levels = rev(levels(two_class_example$truth)),
    direction = "<",
    smooth = TRUE
  )

  points <- pROC::coords(roc_curv, x = unique(c(-Inf, two_class_example$Class1, Inf)), input = "threshold", transpose = FALSE)
  points <- dplyr::as_tibble(points) %>% dplyr::arrange(threshold) %>% dplyr::rename(.threshold = threshold)
  s_points <- pROC::coords(smooth_curv, x = unique(c(0, smooth_curv$specificities, 1)), input = "specificity", transpose = FALSE)
  s_points <- dplyr::as_tibble(s_points) %>% dplyr::arrange(specificity)

  expect_equal(
    as.data.frame(roc_curve(two_class_example, truth, Class1)),
    as.data.frame(points)
  )
  expect_equal(
    as.data.frame(roc_curve(two_class_example, truth, Class1, options = list(smooth = TRUE))),
    as.data.frame(s_points)
  )
})

test_that("Multiclass ROC Curve", {
  # HPC_CV takes too long
  hpc_cv2 <- dplyr::filter(hpc_cv, Resample %in% c("Fold06", "Fold07", "Fold08", "Fold09", "Fold10"))

  res <- roc_curve(hpc_cv2, obs, VF:L)

  # structural tests
  expect_equal(colnames(res), c(".level", ".threshold", "specificity", "sensitivity"))
  expect_equal(unique(res$.level), levels(hpc_cv2$obs))

  res_g <- roc_curve(dplyr::group_by(hpc_cv2, Resample), obs, VF:L)

  # structural tests
  expect_equal(colnames(res_g), c("Resample", ".level", ".threshold", "specificity", "sensitivity"))
})

# ------------------------------------------------------------------------------
# Partial AUC tests
# https://github.com/tidymodels/yardstick/issues/97

test_that("pROC::auc() arguments are passed through", {

  # levels = c(<control>, <event>)
  curv <- pROC::roc(
    response = two_class_example$truth,
    predictor = two_class_example$Class1,
    levels = c("Class2", "Class1"),
    direction = "<"
  )

  proc_auc <- as.numeric(pROC::auc(curv, partial.auc = c(1, 0.75)))

  ys_auc <- roc_auc(
    two_class_example,
    truth,
    Class1,
    options = list(partial.auc = c(1, 0.75))
  )

  expect_equal(
    ys_auc[[".estimate"]],
    proc_auc
  )

})

test_that("pROC::auc() arguments are passed through - corrected and focused args", {

  # From `?pROC::auc`
  data("aSAH", package = "pROC")

  curv <- roc(aSAH$outcome, aSAH$s100b, direction = "<")

  proc_auc <- as.numeric(pROC::auc(
    curv,
    partial.auc = c(1, .8),
    partial.auc.focus = "se",
    partial.auc.correct = TRUE
  ))

  ys_auc <- roc_auc(
    aSAH,
    outcome,
    s100b,
    options = list(
      partial.auc = c(1, .8),
      partial.auc.focus = "se",
      partial.auc.correct = TRUE
    ),
    event_level = "second"
  )

  expect_equal(
    ys_auc[[".estimate"]],
    proc_auc
  )

})

# ------------------------------------------------------------------------------

# HandTill2001::auc(HandTill2001::multcap(hpc_cv2$obs, as.matrix(select(hpc_cv2, VF:L))))
test_that("Hand Till multiclass", {
  # HPC_CV takes too long
  hpc_cv2 <- dplyr::filter(hpc_cv, Resample %in% c("Fold06", "Fold07", "Fold08", "Fold09", "Fold10"))

  expect_equal(
    roc_auc(hpc_cv2, obs, VF:L)[[".estimate"]],
    0.827387699597311
  )
})

test_that("can calculate Hand Till when prob matrix column names are different from level values", {
  # HPC_CV takes too long
  hpc_cv2 <- dplyr::filter(hpc_cv, Resample %in% c("Fold06", "Fold07", "Fold08", "Fold09", "Fold10"))

  # Mimic how parsnip returns names
  colnames(hpc_cv2) <- c(
    "obs", "pred",
    ".pred_VF", ".pred_F", ".pred_M", ".pred_L",
    "Resample"
  )

  expect_equal(
    roc_auc(hpc_cv2, obs, .pred_VF:.pred_L)[[".estimate"]],
    0.827387699597311
  )
})

test_that("`roc_auc()` hand-till method ignores levels with 0 observations with a warning (#123)", {
  # Generally we return `NA_real_` for macro/macro-weighted/micro multiclass,
  # but pROC and HandTill2001 both ignore levels with 0 observations, so we do
  # too for consistency
  truth <- factor(c("x", "x", "z"), levels = c("x", "y", "z"))

  estimate <- c(
    c(.8, .5, .6),
    c(.1, .1, .1),
    c(.1, .4, .3)
  )

  estimate <- matrix(estimate, ncol = 3)
  colnames(estimate) <- c("x", "y", "z")

  # HandTill2001::auc(HandTill2001::multcap(truth, estimate))
  expect_warning(
    expect_identical(roc_auc_vec(truth, estimate), 0.5),
    "No observations were detected in `truth` for level[(]s[)]: 'y'"
  )
})

# ------------------------------------------------------------------------------

test_that("binary roc auc uses `direction = <`", {
  # In yardstick we do events (or cases) as the first level
  truth <- factor(c("control", "case", "case"), levels = c("case", "control"))

  # Make really bad predictions
  # This would force `direction = "auto"` to choose `>`,
  # which would be incorrect. We are required to force `direction = <` for
  # our purposes of having `estimate` match the event
  estimate <- c(.8, .2, .1)

  # pROC() expects levels to be in the order of control, then event.
  auc <- pROC::auc(
    truth,
    estimate,
    levels = c("control", "case"),
    direction = "<"
  )

  auc <- as.numeric(auc)

  expect_identical(roc_auc_vec(truth, estimate), auc)
})

test_that("`direction = <` is forced when individual binary AUCs are computed (#123)", {
  truth <- factor(
    c(
      "c", "c", "c", "d", "d", "a", "d", "c", "a",
      "a", "d", "b", "d", "a", "d", "a", "d", "d"
    ),
    levels = c("a", "b", "c", "d")
  )

  estimate <- c(
    c(c(0.5, 0.4, 0.8, 0.5, 0.8, 1, 0.8, 0, 0.5, 2/3, 0, 1, 0.4, 1/6, 0.4, 0.8, 0.5, 0.6)),
    c(0, 0.2, 0, 0, 0, 0, 0, 0.6, 0, 1/3, 0, 0, 0, 5/6, 0.6, 0, 0, 0),
    c(0, 0.2, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    c(0.5, 0.2, 0.2, 0.5, 0.2, 0, 0.2, 0.2, 0.5, 0, 1, 0, 0.6, 0, 0, 0.2, 0.5, 0.4)
  )

  estimate <- matrix(estimate, ncol = 4, dimnames = list(NULL, c("a", "b", "c", "d")))

  # HandTill2001::auc(HandTill2001::multcap(truth, estimate))
  expect_equal(
    roc_auc_vec(truth, estimate),
    0.5890625
  )
})

# ------------------------------------------------------------------------------

test_that("Multiclass ROC AUC", {
  hpc_f1 <- data_hpc_fold1()

  expect_equal(
    roc_auc(hpc_f1, obs, VF:L, estimator = "macro")[[".estimate"]],
    prob_macro_metric(roc_auc_binary, options = list())
  )
  expect_equal(
    roc_auc(hpc_f1, obs, VF:L, estimator = "macro_weighted")[[".estimate"]],
    prob_macro_weighted_metric(roc_auc_binary, options = list())
  )
})

# ------------------------------------------------------------------------------

test_that("warning is thrown when missing events", {
  no_event <- dplyr::filter(two_class_example, truth == "Class2")

  expect_identical(
    expect_warning(
      roc_auc(no_event, truth, Class1)[[".estimate"]],
      "No event observations were detected in `truth` with event level 'Class1'.",
      class = "yardstick_warning_roc_truth_no_event"
    ),
    NA_real_
  )
})

test_that("warning is thrown when missing controls", {
  no_control <- dplyr::filter(two_class_example, truth == "Class1")

  expect_identical(
    expect_warning(
      roc_auc(no_control, truth, Class1)[[".estimate"]],
      "No control observations were detected in `truth` with control level 'Class2'.",
      class = "yardstick_warning_roc_truth_no_control"
    ),
    NA_real_
  )
})

test_that("multiclass one-vs-all approach results in multiple warnings", {
  no_event <- dplyr::filter(two_class_example, truth == "Class2")

  expect_identical(
    expect_warning(
      roc_auc(no_event, truth, Class1, Class2, estimator = "macro")[[".estimate"]],
      "No event observations were detected in `truth` with event level 'Class1'"
    ),
    NA_real_
  )

  expect_identical(
    expect_warning(
      roc_auc(no_event, truth, Class1, Class2, estimator = "macro")[[".estimate"]],
      "No control observations were detected in `truth` with control level '..other'"
    ),
    NA_real_
  )

  expect_identical(
    expect_warning(
      roc_auc(no_event, truth, Class1, Class2, estimator = "macro_weighted")[[".estimate"]],
      "No event observations were detected in `truth` with event level 'Class1'"
    ),
    NA_real_
  )

  expect_identical(
    expect_warning(
      roc_auc(no_event, truth, Class1, Class2, estimator = "macro_weighted")[[".estimate"]],
      "No control observations were detected in `truth` with control level '..other'"
    ),
    NA_real_
  )
})

test_that("hand till approach throws warning and returns `NaN` when only 1 level has observations", {
  x <- factor(c("x", "x", "x"), levels = c("x", "y"))

  estimate <- c(
    c(.8, .5, .6),
    c(.2, .5, .4)
  )

  estimate <- matrix(estimate, ncol = 2)
  colnames(estimate) <- c("x", "y")

  # With two levels -> one
  expect_identical(
    expect_warning(
      roc_auc_vec(x, estimate, estimator = "hand_till"),
      "No observations were detected in `truth` for level[(]s[)]: 'y'"
    ),
    NaN
  )

  x <- factor(c("x", "x", "x"), levels = c("x", "y", "z"))

  estimate <- c(
    c(.8, .5, .6),
    c(.1, .1, .1),
    c(.1, .4, .3)
  )

  estimate <- matrix(estimate, ncol = 3)
  colnames(estimate) <- c("x", "y", "z")

  # With three levels -> one
  expect_identical(
    expect_warning(
      roc_auc_vec(x, estimate, estimator = "hand_till"),
      "No observations were detected in `truth` for level[(]s[)]: 'y', 'z'"
    ),
    NaN
  )
})
