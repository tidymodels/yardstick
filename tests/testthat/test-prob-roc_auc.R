test_that("Two class", {
  # roc_curv <- pROC::roc(
  #   two_class_example$truth,
  #   two_class_example$Class1,
  #   levels = rev(levels(two_class_example$truth)),
  #   direction = "<"
  # )
  #
  # lvls <- levels(two_class_example$truth)
  # roc_val <- as.numeric(roc_curv$auc)
  roc_val <- 0.939313857389967

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
  hpc_cv2 <- dplyr::filter(
    hpc_cv,
    Resample %in% c("Fold06", "Fold07", "Fold08", "Fold09", "Fold10")
  )

  expect_equal(
    roc_auc(hpc_cv2, obs, VF:L)[[".estimate"]],
    0.827387699597311
  )
})

test_that("can calculate Hand Till when prob matrix column names are different from level values", {
  # HPC_CV takes too long
  hpc_cv2 <- dplyr::filter(
    hpc_cv,
    Resample %in% c("Fold06", "Fold07", "Fold08", "Fold09", "Fold10")
  )

  # Mimic how parsnip returns names
  colnames(hpc_cv2) <- c(
    "obs",
    "pred",
    ".pred_VF",
    ".pred_F",
    ".pred_M",
    ".pred_L",
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
  expect_snapshot(
    expect_identical(roc_auc_vec(truth, estimate), 0.5)
  )
})

# ------------------------------------------------------------------------------

test_that("binary roc auc uses equivalent of pROC `direction = <`", {
  # In yardstick we do events (or cases) as the first level
  truth <- factor(c("control", "case", "case"), levels = c("case", "control"))

  # Make really bad predictions
  # This would force `direction = "auto"` to choose `>`,
  # which would be incorrect. We are required to force `direction = <` for
  # our purposes of having `estimate` match the event
  estimate <- c(.8, .2, .1)

  # # pROC() expects levels to be in the order of control, then event.
  # auc <- pROC::auc(
  #   truth,
  #   estimate,
  #   levels = c("control", "case"),
  #   direction = "<"
  # )
  # auc <- as.numeric(auc)
  auc <- 0

  expect_identical(roc_auc_vec(truth, estimate), auc)
})

test_that("equivalent of `direction = <` is forced when individual binary AUCs are computed (#123)", {
  truth <- factor(
    c(
      "c",
      "c",
      "c",
      "d",
      "d",
      "a",
      "d",
      "c",
      "a",
      "a",
      "d",
      "b",
      "d",
      "a",
      "d",
      "a",
      "d",
      "d"
    ),
    levels = c("a", "b", "c", "d")
  )

  estimate <- c(
    c(
      c(
        0.5,
        0.4,
        0.8,
        0.5,
        0.8,
        1,
        0.8,
        0,
        0.5,
        2 / 3,
        0,
        1,
        0.4,
        1 / 6,
        0.4,
        0.8,
        0.5,
        0.6
      )
    ),
    c(0, 0.2, 0, 0, 0, 0, 0, 0.6, 0, 1 / 3, 0, 0, 0, 5 / 6, 0.6, 0, 0, 0),
    c(0, 0.2, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    c(
      0.5,
      0.2,
      0.2,
      0.5,
      0.2,
      0,
      0.2,
      0.2,
      0.5,
      0,
      1,
      0,
      0.6,
      0,
      0,
      0.2,
      0.5,
      0.4
    )
  )

  estimate <- matrix(
    estimate,
    ncol = 4,
    dimnames = list(NULL, c("a", "b", "c", "d"))
  )

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
    hpc_fold1_macro_metric(roc_auc_binary)
  )
  expect_equal(
    roc_auc(hpc_f1, obs, VF:L, estimator = "macro_weighted")[[".estimate"]],
    hpc_fold1_macro_weighted_metric(roc_auc_binary)
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
    out <- roc_auc(no_event, truth, Class1, Class2, estimator = "macro")[[
      ".estimate"
    ]]
  )
  expect_identical(out, NA_real_)

  expect_snapshot(
    out <- roc_auc(no_event, truth, Class1, Class2, estimator = "macro")[[
      ".estimate"
    ]]
  )
  expect_identical(out, NA_real_)

  expect_snapshot(
    out <- roc_auc(
      no_event,
      truth,
      Class1,
      Class2,
      estimator = "macro_weighted"
    )[[".estimate"]]
  )
  expect_identical(out, NA_real_)

  expect_snapshot(
    out <- roc_auc(
      no_event,
      truth,
      Class1,
      Class2,
      estimator = "macro_weighted"
    )[[".estimate"]]
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

test_that("df method - presense of case weights affects default multiclass estimator", {
  hpc_cv$weight <- read_weights_hpc_cv()

  hpc_cv_estimate_matrix <- as.matrix(hpc_cv[c("VF", "F", "M", "L")])

  # Generally hand-till
  out <- roc_auc(hpc_cv, obs, VF:L)
  expect_identical(out$.estimator, "hand_till")
  expect_identical(
    out$.estimate,
    roc_auc_vec(hpc_cv$obs, hpc_cv_estimate_matrix, estimator = "hand_till")
  )

  # Unless case weights are supplied
  out <- roc_auc(hpc_cv, obs, VF:L, case_weights = weight)
  expect_identical(out$.estimator, "macro")
  expect_identical(
    out$.estimate,
    roc_auc_vec(
      hpc_cv$obs,
      hpc_cv_estimate_matrix,
      estimator = "macro",
      case_weights = hpc_cv$weight
    )
  )
})

test_that("vec method - presense of case weights affects default multiclass estimator", {
  hpc_cv$weight <- read_weights_hpc_cv()

  hpc_cv_estimate_matrix <- as.matrix(hpc_cv[c("VF", "F", "M", "L")])

  # Generally hand-till
  expect_identical(
    roc_auc_vec(hpc_cv$obs, hpc_cv_estimate_matrix),
    roc_auc_vec(hpc_cv$obs, hpc_cv_estimate_matrix, estimator = "hand_till")
  )

  # Unless case weights are supplied
  expect_identical(
    roc_auc_vec(
      hpc_cv$obs,
      hpc_cv_estimate_matrix,
      case_weights = hpc_cv$weight
    ),
    roc_auc_vec(
      hpc_cv$obs,
      hpc_cv_estimate_matrix,
      estimator = "macro",
      case_weights = hpc_cv$weight
    )
  )
})

test_that("can't use case weights and hand-till method", {
  hpc_cv$weight <- read_weights_hpc_cv()

  expect_snapshot(error = TRUE, {
    roc_auc(hpc_cv, obs, VF:L, estimator = "hand_till", case_weights = weight)
  })
})

# ------------------------------------------------------------------------------

test_that("Two class ROC AUC matches sklearn", {
  sklearn <- read_pydata("py-roc-auc")

  expect_equal(
    roc_auc(two_class_example, truth, Class1)[[".estimate"]],
    sklearn$binary
  )
})

test_that("Two class weighted ROC AUC matches sklearn", {
  sklearn <- read_pydata("py-roc-auc")

  two_class_example$weight <- read_weights_two_class_example()

  expect_equal(
    roc_auc(two_class_example, truth, Class1, case_weights = weight)[[
      ".estimate"
    ]],
    sklearn$case_weight$binary
  )
})

test_that("Multiclass ROC AUC matches sklearn", {
  sklearn <- read_pydata("py-roc-auc")

  expect_equal(
    roc_auc(hpc_cv, obs, VF:L, estimator = "macro")[[".estimate"]],
    sklearn$macro
  )
  expect_equal(
    roc_auc(hpc_cv, obs, VF:L, estimator = "macro_weighted")[[".estimate"]],
    sklearn$macro_weighted
  )
  expect_equal(
    roc_auc(hpc_cv, obs, VF:L, estimator = "hand_till")[[".estimate"]],
    sklearn$hand_till
  )
})

test_that("Multiclass weighted ROC AUC matches sklearn", {
  sklearn <- read_pydata("py-roc-auc")

  hpc_cv$weight <- read_weights_hpc_cv()

  expect_equal(
    roc_auc(hpc_cv, obs, VF:L, estimator = "macro", case_weights = weight)[[
      ".estimate"
    ]],
    sklearn$case_weight$macro
  )
  expect_equal(
    roc_auc(
      hpc_cv,
      obs,
      VF:L,
      estimator = "macro_weighted",
      case_weights = weight
    )[[".estimate"]],
    sklearn$case_weight$macro_weighted
  )

  # No support for hand_till + case weights
})

test_that("grouped multiclass (one-vs-all) weighted example matches expanded equivalent", {
  hpc_cv$weight <- rep(1, times = nrow(hpc_cv))
  hpc_cv$weight[c(100, 200, 150, 2)] <- 5

  hpc_cv <- dplyr::group_by(hpc_cv, Resample)

  hpc_cv_expanded <- hpc_cv[
    vec_rep_each(seq_len(nrow(hpc_cv)), times = hpc_cv$weight),
  ]

  expect_identical(
    roc_auc(hpc_cv, obs, VF:L, case_weights = weight, estimator = "macro"),
    roc_auc(hpc_cv_expanded, obs, VF:L, estimator = "macro")
  )

  expect_identical(
    roc_auc(
      hpc_cv,
      obs,
      VF:L,
      case_weights = weight,
      estimator = "macro_weighted"
    ),
    roc_auc(hpc_cv_expanded, obs, VF:L, estimator = "macro_weighted")
  )
})

# ------------------------------------------------------------------------------

test_that("roc_auc() - `options` is deprecated", {
  skip_if(
    getRversion() <= "3.5.3",
    "Base R used a different deprecated warning class."
  )
  rlang::local_options(lifecycle_verbosity = "warning")

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

test_that("works with hardhat case weights", {
  df <- two_class_example

  imp_wgt <- hardhat::importance_weights(seq_len(nrow(df)))
  freq_wgt <- hardhat::frequency_weights(seq_len(nrow(df)))

  expect_no_error(
    roc_auc_vec(df$truth, df$Class1, case_weights = imp_wgt)
  )

  expect_no_error(
    roc_auc_vec(df$truth, df$Class1, case_weights = freq_wgt)
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
    roc_auc_vec(cp_truth, estimate)
  )
})
