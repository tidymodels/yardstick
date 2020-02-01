context("ROC Curve/AUC")

library(dplyr)

# HPC_CV takes too long
hpc_cv2 <- filter(hpc_cv, Resample %in% c("Fold06", "Fold07", "Fold08", "Fold09", "Fold10"))

# ------------------------------------------------------------------------------

roc_curv <- pROC::roc(two_class_example$truth,
                      two_class_example$Class1,
                      levels = rev(levels(two_class_example$truth)))
lvls <- levels(two_class_example$truth)
roc_val <- as.numeric(roc_curv$auc)
smooth_curv <- pROC::roc(two_class_example$truth,
                         two_class_example$Class1,
                         levels = rev(levels(two_class_example$truth)),
                         smooth = TRUE)

test_that('Two class', {
  expect_equal(
    roc_auc(two_class_example, truth, Class1)[[".estimate"]],
    roc_val
  )
  expect_equal(
    roc_auc(two_class_example, truth = "truth", Class2)[[".estimate"]],
    roc_val
  )
  expect_equal(
    roc_auc(two_class_example, truth, Class1, options = list(smooth = TRUE))[[".estimate"]],
    as.numeric(smooth_curv$auc),
    tol = 0.001
  )
})

test_that('ROC Curve', {
  library(pROC)
  points <- coords(roc_curv, x = unique(c(-Inf, two_class_example$Class1, Inf)), input = "threshold", transpose = FALSE)
  points <- dplyr::as_tibble(points) %>% dplyr::arrange(threshold) %>% dplyr::rename(.threshold = threshold)
  s_points <- coords(smooth_curv, x = unique(c(0, smooth_curv$specificities, 1)), input = "specificity", transpose = FALSE)
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
  res <- roc_curve(hpc_cv2, obs, VF:L)

  # structural tests
  expect_equal(colnames(res), c(".level", ".threshold", "specificity", "sensitivity"))
  expect_equal(unique(res$.level), levels(hpc_cv2$obs))

  res_g <- roc_curve(group_by(hpc_cv2, Resample), obs, VF:L)

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
    levels = c("Class2", "Class1")
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

  curv <- roc(aSAH$outcome, aSAH$s100b)

  proc_auc <- as.numeric(pROC::auc(
    curv,
    partial.auc = c(1, .8),
    partial.auc.focus = "se",
    partial.auc.correct = TRUE)
  )

  ys_auc <- rlang::with_options(
    .expr = {
      roc_auc(
        aSAH,
        outcome,
        s100b,
        options = list(
          partial.auc = c(1, .8),
          partial.auc.focus = "se",
          partial.auc.correct = TRUE
        )
      )
    },
    yardstick.event_first = FALSE
  )

  expect_equal(
    ys_auc[[".estimate"]],
    proc_auc
  )

})

# ------------------------------------------------------------------------------

# HandTill2001::auc(HandTill2001::multcap(hpc_cv2$obs, as.matrix(select(hpc_cv2, VF:L))))
test_that("Hand Till multiclass", {
  expect_equal(
    roc_auc(hpc_cv2, obs, VF:L)[[".estimate"]],
    0.827387699597311
  )
})

test_that("can calculate Hand Till when prob matrix column names are different from level values", {

  hpc_cv_renamed_cols <- hpc_cv2

  # Mimic how parsnip returns names
  colnames(hpc_cv_renamed_cols) <- c(
    "obs", "pred",
    ".pred_VF", ".pred_F", ".pred_M", ".pred_L",
    "Resample"
  )

  expect_equal(
    roc_auc(hpc_cv_renamed_cols, obs, .pred_VF:.pred_L)[[".estimate"]],
    0.827387699597311
  )

})

# ------------------------------------------------------------------------------

hpc_f1 <- data_hpc_fold1()

test_that("Multiclass ROC AUC", {
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

test_that("Missing case or control returns NA", {
  one_class <-
    two_class_example %>%
    dplyr::filter(truth == "Class1")

  expect_true(
    suppressWarnings(
      is.na(roc_auc(one_class, truth, Class1)[[".estimate"]])
    )
  )
   expect_warning(
    roc_auc(one_class, truth, Class1)
  )
  expect_equal(
    suppressWarnings(
      roc_auc_multiclass(one_class[["truth"]], one_class[, 2:3])
    ),
      c(NA_real_, NA_real_)
  )

   expect_warning(
     roc_auc_multiclass(one_class[["truth"]], one_class[, 2:3])
   )
})

