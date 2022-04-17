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
})

test_that("`event_level = 'second'` works", {
  df <- two_class_example

  df_rev <- df
  df_rev$truth <- stats::relevel(df_rev$truth, "Class2")

  expect_equal(
    roc_auc_vec(df$truth, df$Class1),
    roc_auc_vec(df_rev$truth, df_rev$Class1, event_level = "second")
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
    prob_macro_metric(roc_auc_binary)
  )
  expect_equal(
    roc_auc(hpc_f1, obs, VF:L, estimator = "macro_weighted")[[".estimate"]],
    prob_macro_weighted_metric(roc_auc_binary)
  )
})

# ------------------------------------------------------------------------------

test_that("warning is thrown when missing events", {
  no_event <- dplyr::filter(two_class_example, truth == "Class2")

  expect_snapshot(out <- roc_auc(no_event, truth, Class1)[[".estimate"]])

  expect_identical(out, NA_real_)
})

test_that("warning is thrown when missing controls", {
  no_control <- dplyr::filter(two_class_example, truth == "Class1")

  expect_snapshot(out <- roc_auc(no_control, truth, Class1)[[".estimate"]])

  expect_identical(out, NA_real_)
})

test_that("multiclass one-vs-all approach results in multiple warnings", {
  no_event <- dplyr::filter(two_class_example, truth == "Class2")

  expect_snapshot(
    out <- roc_auc(no_event, truth, Class1, Class2, estimator = "macro")[[".estimate"]]
  )
  expect_identical(out, NA_real_)

  expect_snapshot(
    out <- roc_auc(no_event, truth, Class1, Class2, estimator = "macro")[[".estimate"]]
  )
  expect_identical(out, NA_real_)

  expect_snapshot(
    out <- roc_auc(no_event, truth, Class1, Class2, estimator = "macro_weighted")[[".estimate"]]
  )
  expect_identical(out, NA_real_)

  expect_snapshot(
    out <- roc_auc(no_event, truth, Class1, Class2, estimator = "macro_weighted")[[".estimate"]]
  )
  expect_identical(out, NA_real_)
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
  expect_snapshot(
    out <- roc_auc_vec(x, estimate, estimator = "hand_till")
  )
  expect_identical(out, NaN)

  x <- factor(c("x", "x", "x"), levels = c("x", "y", "z"))

  estimate <- c(
    c(.8, .5, .6),
    c(.1, .1, .1),
    c(.1, .4, .3)
  )

  estimate <- matrix(estimate, ncol = 3)
  colnames(estimate) <- c("x", "y", "z")

  # With three levels -> one
  expect_snapshot(
    out <- roc_auc_vec(x, estimate, estimator = "hand_till")
  )
  expect_identical(out, NaN)
})

# ------------------------------------------------------------------------------

test_that("roc_auc() - `options` is deprecated", {
  skip_if(getRversion() <= "3.5.3", "Base R used a different deprecated warning class.")
  local_lifecycle_warnings()

  expect_snapshot({
    out <- roc_auc(two_class_example, truth, Class1, options = 1)
  })

  expect_identical(
    out,
    roc_auc(two_class_example, truth, Class1)
  )

  expect_snapshot({
    out <- roc_auc_vec(
      truth = two_class_example$truth,
      estimate = two_class_example$Class1,
      options = 1
    )
  })

  expect_identical(
    out,
    roc_auc_vec(
      truth = two_class_example$truth,
      estimate = two_class_example$Class1
    )
  )
})
