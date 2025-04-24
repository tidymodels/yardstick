# reticulate::install_python("3.10.4")
# reticulate::virtualenv_create("yardstick-environment", version = "3.10.4")
# reticulate::use_virtualenv("yardstick-environment")
# reticulate::py_install("scikit-learn") # 1.2.0 is what was downloaded
# reticulate::py_install("scikit-survival") # 1.2.0 is what was downloaded
# reticulate::py_install("fairlearn") # 0.8.0 is what was downloaded
library(reticulate)

# Inside yardstick
devtools::load_all()

use_virtualenv("yardstick-environment")

skmetrics <- import("sklearn.metrics")
sksurv_metrics <- import("sksurv.metrics")
sksurv_util <- import("sksurv.util", convert = FALSE)
fairlearn <- import("fairlearn.metrics")

data("hpc_cv")
data("two_class_example")
data("solubility_test")

weights_hpc_cv <- read_weights_hpc_cv()
weights_two_class_example <- read_weights_two_class_example()
weights_solubility_test <- read_weights_solubility_test()

save_metric_results <- function(
  nm,
  fn,
  ...,
  average = c("macro", "micro", "weighted")
) {
  # Two class metrics
  res <- list(
    fn(
      two_class_example$truth,
      two_class_example$predicted,
      ...,
      pos_label = "Class1"
    )
  )

  # Multiclass metrics
  res2 <- lapply(
    average,
    function(.x) fn(hpc_cv$obs, hpc_cv$pred, ..., average = .x)
  )

  # Two class weighted metrics
  case_weight <- list(
    fn(
      two_class_example$truth,
      two_class_example$predicted,
      ...,
      pos_label = "Class1",
      sample_weight = weights_two_class_example
    )
  )

  # Multiclass weighted metrics
  case_weight2 <- lapply(
    average,
    function(.x)
      fn(
        hpc_cv$obs,
        hpc_cv$pred,
        ...,
        average = .x,
        sample_weight = weights_hpc_cv
      )
  )

  case_weight <- c(case_weight, case_weight2)
  names(case_weight) <- c("binary", average)

  res <- c(res, res2, list(case_weight))
  names(res) <- c("binary", average, "case_weight")

  saveRDS(res, test_path("py-data", paste0("py-", nm, ".rds")), version = 2)
}

# ------------------------------------------------------------------------------
# Table statistics used in other metrics

# Note: Truth down the left side, Predicted across the top, unlike
# yardstick's table function. But this matches most sklearn references better.
py_binary_weighted_confusion <- skmetrics$confusion_matrix(
  y_true = two_class_example$truth,
  y_pred = two_class_example$predicted,
  sample_weight = weights_two_class_example
)
py_multiclass_weighted_confusion <- skmetrics$confusion_matrix(
  y_true = hpc_cv$obs,
  y_pred = hpc_cv$pred,
  sample_weight = weights_hpc_cv
)

py_binary_weighted_FP <- colSums(py_binary_weighted_confusion) -
  diag(py_binary_weighted_confusion)
py_binary_weighted_FN <- rowSums(py_binary_weighted_confusion) -
  diag(py_binary_weighted_confusion)
py_binary_weighted_TP <- diag(py_binary_weighted_confusion)
py_binary_weighted_TN <- sum(py_binary_weighted_confusion) -
  (py_binary_weighted_FP + py_binary_weighted_FN + py_binary_weighted_TP)

py_multiclass_weighted_FP <- colSums(py_multiclass_weighted_confusion) -
  diag(py_multiclass_weighted_confusion)
py_multiclass_weighted_FN <- rowSums(py_multiclass_weighted_confusion) -
  diag(py_multiclass_weighted_confusion)
py_multiclass_weighted_TP <- diag(py_multiclass_weighted_confusion)
py_multiclass_weighted_TN <- sum(py_multiclass_weighted_confusion) -
  (py_multiclass_weighted_FP +
    py_multiclass_weighted_FN +
    py_multiclass_weighted_TP)

# ------------------------------------------------------------------------------

# Save metrics results
save_metric_results("precision", skmetrics$precision_score)
save_metric_results("recall", skmetrics$recall_score)
save_metric_results("f_meas", skmetrics$f1_score)
save_metric_results("f_meas_beta_.5", skmetrics$fbeta_score, beta = .5)

# MCC
py_mcc <- list(
  binary = skmetrics$matthews_corrcoef(
    two_class_example$truth,
    two_class_example$predicted
  ),
  multiclass = skmetrics$matthews_corrcoef(hpc_cv$obs, hpc_cv$pred),
  case_weight = list(
    binary = skmetrics$matthews_corrcoef(
      two_class_example$truth,
      two_class_example$predicted,
      sample_weight = weights_two_class_example
    ),
    multiclass = skmetrics$matthews_corrcoef(
      hpc_cv$obs,
      hpc_cv$pred,
      sample_weight = weights_hpc_cv
    )
  )
)
saveRDS(py_mcc, test_path("py-data", "py-mcc.rds"), version = 2)

# Accuracy
py_accuracy <- list(
  binary = skmetrics$accuracy_score(
    two_class_example$truth,
    two_class_example$predicted
  ),
  multiclass = skmetrics$accuracy_score(hpc_cv$obs, hpc_cv$pred)
)
saveRDS(py_accuracy, test_path("py-data", "py-accuracy.rds"), version = 2)

# Kappa
py_kap <- list(
  binary = skmetrics$cohen_kappa_score(
    two_class_example$truth,
    two_class_example$predicted,
    labels = levels(two_class_example$truth)
  ),
  multiclass = skmetrics$cohen_kappa_score(
    hpc_cv$obs,
    hpc_cv$pred,
    labels = levels(hpc_cv$obs)
  ),
  linear_binary = skmetrics$cohen_kappa_score(
    two_class_example$truth,
    two_class_example$predicted,
    labels = levels(two_class_example$truth),
    weights = "linear"
  ),
  linear_multiclass = skmetrics$cohen_kappa_score(
    hpc_cv$obs,
    hpc_cv$pred,
    labels = levels(hpc_cv$obs),
    weights = "linear"
  ),
  quadratic_binary = skmetrics$cohen_kappa_score(
    two_class_example$truth,
    two_class_example$predicted,
    labels = levels(two_class_example$truth),
    weights = "quadratic"
  ),
  quadratic_multiclass = skmetrics$cohen_kappa_score(
    hpc_cv$obs,
    hpc_cv$pred,
    labels = levels(hpc_cv$obs),
    weights = "quadratic"
  ),
  case_weight = list(
    binary = skmetrics$cohen_kappa_score(
      two_class_example$truth,
      two_class_example$predicted,
      labels = levels(two_class_example$truth),
      sample_weight = weights_two_class_example
    ),
    multiclass = skmetrics$cohen_kappa_score(
      hpc_cv$obs,
      hpc_cv$pred,
      labels = levels(hpc_cv$obs),
      sample_weight = weights_hpc_cv
    ),
    linear_binary = skmetrics$cohen_kappa_score(
      two_class_example$truth,
      two_class_example$predicted,
      labels = levels(two_class_example$truth),
      weights = "linear",
      sample_weight = weights_two_class_example
    ),
    linear_multiclass = skmetrics$cohen_kappa_score(
      hpc_cv$obs,
      hpc_cv$pred,
      labels = levels(hpc_cv$obs),
      weights = "linear",
      sample_weight = weights_hpc_cv
    ),
    quadratic_binary = skmetrics$cohen_kappa_score(
      two_class_example$truth,
      two_class_example$predicted,
      labels = levels(two_class_example$truth),
      weights = "quadratic",
      sample_weight = weights_two_class_example
    ),
    quadratic_multiclass = skmetrics$cohen_kappa_score(
      hpc_cv$obs,
      hpc_cv$pred,
      labels = levels(hpc_cv$obs),
      weights = "quadratic",
      sample_weight = weights_hpc_cv
    )
  )
)
saveRDS(py_kap, test_path("py-data", "py-kap.rds"), version = 2)

# RMSE
py_rmse <- list(
  case_weight = sqrt(
    skmetrics$mean_squared_error(
      y_true = solubility_test$solubility,
      y_pred = solubility_test$prediction,
      sample_weight = weights_solubility_test
    )
  )
)
saveRDS(py_rmse, test_path("py-data", "py-rmse.rds"), version = 2)

# MAE
py_mae <- list(
  case_weight = skmetrics$mean_absolute_error(
    y_true = solubility_test$solubility,
    y_pred = solubility_test$prediction,
    sample_weight = weights_solubility_test
  )
)
saveRDS(py_mae, test_path("py-data", "py-mae.rds"), version = 2)

# MAPE
# (Zeros purposefully cause infinite results in our R metrics, see #271)
zero_solubility <- solubility_test$solubility == 0
solubility_test_not_zero <- solubility_test[!zero_solubility, ]
py_mape <- list(
  case_weight = skmetrics$mean_absolute_percentage_error(
    y_true = solubility_test_not_zero$solubility,
    y_pred = solubility_test_not_zero$prediction,
    sample_weight = weights_solubility_test[!zero_solubility]
  )
)
saveRDS(py_mape, test_path("py-data", "py-mape.rds"), version = 2)

# R^2 Traditional
py_rsq_trad <- list(
  case_weight = skmetrics$r2_score(
    y_true = solubility_test$solubility,
    y_pred = solubility_test$prediction,
    sample_weight = weights_solubility_test
  )
)
saveRDS(py_rsq_trad, test_path("py-data", "py-rsq-trad.rds"), version = 2)

# Balanced Accuracy
# Not comparing multiclass against sklearn here, because they use a different definition
py_bal_accuracy <- list(
  case_weight = list(
    binary = skmetrics$balanced_accuracy_score(
      y_true = two_class_example$truth,
      y_pred = two_class_example$predicted,
      sample_weight = weights_two_class_example
    )
  )
)
saveRDS(
  py_bal_accuracy,
  test_path("py-data", "py-bal-accuracy.rds"),
  version = 2
)

# NPV
py_binary_weighted_NPV <- py_binary_weighted_TN /
  (py_binary_weighted_TN + py_binary_weighted_FN)
py_multiclass_weighted_NPV <- py_multiclass_weighted_TN /
  (py_multiclass_weighted_TN + py_multiclass_weighted_FN)

py_npv <- list(
  case_weight = list(
    binary = py_binary_weighted_NPV[[1]],
    macro = mean(py_multiclass_weighted_NPV)
  )
)
saveRDS(py_npv, test_path("py-data", "py-npv.rds"), version = 2)

# PPV
py_binary_weighted_PPV <- py_binary_weighted_TP /
  (py_binary_weighted_TP + py_binary_weighted_FP)
py_multiclass_weighted_PPV <- py_multiclass_weighted_TP /
  (py_multiclass_weighted_TP + py_multiclass_weighted_FP)

py_ppv <- list(
  case_weight = list(
    binary = py_binary_weighted_PPV[[1]],
    macro = mean(py_multiclass_weighted_PPV)
  )
)
saveRDS(py_ppv, test_path("py-data", "py-ppv.rds"), version = 2)

# mn_log_loss
# log_loss() requires labels in alphabetical order, because that is what LabelBinarizer() does
log_loss_two_class_example_levels <- c("Class1", "Class2")
log_loss_two_class_example_estimate <- cbind(
  Class1 = two_class_example$Class1,
  Class2 = two_class_example$Class2
)

log_loss_hpc_cv_levels <- c("F", "L", "M", "VF")
log_loss_hpc_cv_estimate <- as.matrix(hpc_cv[log_loss_hpc_cv_levels])

mn_log_loss_eps <- .Machine$double.eps

py_mn_log_loss <- list(
  binary = skmetrics$log_loss(
    two_class_example$truth,
    log_loss_two_class_example_estimate,
    labels = log_loss_two_class_example_levels,
    eps = mn_log_loss_eps
  ),
  multiclass = skmetrics$log_loss(
    hpc_cv$obs,
    log_loss_hpc_cv_estimate,
    labels = log_loss_hpc_cv_levels,
    eps = mn_log_loss_eps
  ),
  binary_sum = skmetrics$log_loss(
    two_class_example$truth,
    log_loss_two_class_example_estimate,
    labels = log_loss_two_class_example_levels,
    normalize = FALSE
  ),
  multiclass_sum = skmetrics$log_loss(
    hpc_cv$obs,
    log_loss_hpc_cv_estimate,
    labels = log_loss_hpc_cv_levels,
    eps = mn_log_loss_eps,
    normalize = FALSE
  ),
  case_weight = list(
    binary = skmetrics$log_loss(
      two_class_example$truth,
      log_loss_two_class_example_estimate,
      labels = log_loss_two_class_example_levels,
      eps = mn_log_loss_eps,
      sample_weight = weights_two_class_example
    ),
    multiclass = skmetrics$log_loss(
      hpc_cv$obs,
      log_loss_hpc_cv_estimate,
      labels = log_loss_hpc_cv_levels,
      eps = mn_log_loss_eps,
      sample_weight = weights_hpc_cv
    ),
    binary_sum = skmetrics$log_loss(
      two_class_example$truth,
      log_loss_two_class_example_estimate,
      labels = log_loss_two_class_example_levels,
      normalize = FALSE,
      sample_weight = weights_two_class_example
    ),
    multiclass_sum = skmetrics$log_loss(
      hpc_cv$obs,
      log_loss_hpc_cv_estimate,
      labels = log_loss_hpc_cv_levels,
      eps = mn_log_loss_eps,
      normalize = FALSE,
      sample_weight = weights_hpc_cv
    )
  )
)
saveRDS(py_mn_log_loss, test_path("py-data", "py-mn_log_loss.rds"), version = 2)

# PR Curve
make_py_two_class_pr_curve <- function(case_weights) {
  # - sklearn puts them in decreasing order of recall
  # - sklearn doesn't add the `Inf` threshold value but does add the `0` recall and `1` precision value
  # - sklearn stops the curve once we hit the first `recall == 1` value, we don't
  # - reticulate brings all inputs in as arrays

  if (!is.logical(case_weights) || length(case_weights) != 1L) {
    stop("`case_weights` must be a single logical.")
  }

  if (case_weights) {
    curve <- skmetrics$precision_recall_curve(
      y_true = two_class_example$truth,
      probas_pred = two_class_example$Class1,
      pos_label = "Class1",
      sample_weight = weights_two_class_example
    )
  } else {
    curve <- skmetrics$precision_recall_curve(
      y_true = two_class_example$truth,
      probas_pred = two_class_example$Class1,
      pos_label = "Class1"
    )
  }

  names(curve) <- c("precision", "recall", ".threshold")

  curve$recall <- as.vector(curve$recall)
  curve$precision <- as.vector(curve$precision)
  curve$.threshold <- as.vector(curve$.threshold)

  curve$.threshold <- c(curve$.threshold, Inf)

  curve <- tibble::tibble(!!!curve)
  curve <- dplyr::select(curve, .threshold, recall, precision)

  # Reverse rows to match yardstick
  curve <- curve[rev(seq_len(nrow(curve))), ]

  class(curve) <- c("pr_df", class(curve))

  curve
}

py_pr_curve <- list(
  binary = make_py_two_class_pr_curve(case_weights = FALSE),
  case_weight = list(
    binary = make_py_two_class_pr_curve(case_weights = TRUE)
  )
)
saveRDS(py_pr_curve, test_path("py-data", "py-pr-curve.rds"), version = 2)

# Average precision
# sklearn doesn't support >2 levels in `truth` directly, but does support
# "multi-label" `truth`, which ends up being the same thing here.
average_precision_hpc_cv_truth <- model.matrix(~ obs - 1, hpc_cv)
colnames(average_precision_hpc_cv_truth) <- levels(hpc_cv$obs)
average_precision_hpc_cv_estimate <- as.matrix(hpc_cv[levels(hpc_cv$obs)])

py_average_precision <- list(
  binary = skmetrics$average_precision_score(
    two_class_example$truth,
    two_class_example$Class1,
    pos_label = "Class1"
  ),
  macro = skmetrics$average_precision_score(
    average_precision_hpc_cv_truth,
    average_precision_hpc_cv_estimate,
    average = "macro"
  ),
  macro_weighted = skmetrics$average_precision_score(
    average_precision_hpc_cv_truth,
    average_precision_hpc_cv_estimate,
    average = "weighted"
  ),
  case_weight = list(
    binary = skmetrics$average_precision_score(
      two_class_example$truth,
      two_class_example$Class1,
      pos_label = "Class1",
      sample_weight = weights_two_class_example
    ),
    macro = skmetrics$average_precision_score(
      average_precision_hpc_cv_truth,
      average_precision_hpc_cv_estimate,
      average = "macro",
      sample_weight = weights_hpc_cv
    ),
    macro_weighted = skmetrics$average_precision_score(
      average_precision_hpc_cv_truth,
      average_precision_hpc_cv_estimate,
      average = "weighted",
      sample_weight = weights_hpc_cv
    )
  )
)
saveRDS(
  py_average_precision,
  test_path("py-data", "py-average-precision.rds"),
  version = 2
)

# ROC Curve
make_py_two_class_roc_curve <- function(case_weights) {
  # - sklearn puts them in decreasing order of specificity
  # - sklearn needs the "first" row and a tweak to the threshold of the "last" row
  # - sklearn has a `drop_intermediate = TRUE` default to trim the curve
  # - reticulate brings all inputs in as arrays

  if (!is.logical(case_weights) || length(case_weights) != 1L) {
    stop("`case_weights` must be a single logical.")
  }

  if (case_weights) {
    curve <- skmetrics$roc_curve(
      y_true = two_class_example$truth,
      y_score = two_class_example$Class1,
      pos_label = "Class1",
      sample_weight = weights_two_class_example,
      drop_intermediate = FALSE
    )
  } else {
    curve <- skmetrics$roc_curve(
      y_true = two_class_example$truth,
      y_score = two_class_example$Class1,
      pos_label = "Class1",
      drop_intermediate = FALSE
    )
  }

  names(curve) <- c("fpr", "tpr", ".threshold")

  curve$fpr <- as.vector(curve$fpr)
  curve$tpr <- as.vector(curve$tpr)
  curve$.threshold <- as.vector(curve$.threshold)

  curve$sensitivity <- curve$tpr
  curve$specificity <- 1 - curve$fpr

  curve$fpr <- NULL
  curve$tpr <- NULL

  # Reverse rows to match yardstick
  curve$.threshold <- rev(curve$.threshold)
  curve$sensitivity <- rev(curve$sensitivity)
  curve$specificity <- rev(curve$specificity)

  # Add "first" row to match yardstick
  curve$.threshold <- c(-Inf, curve$.threshold)
  curve$sensitivity <- c(1, curve$sensitivity)
  curve$specificity <- c(0, curve$specificity)

  # sklearn adds the "last" row, but their threshold value is
  # `last_threshold + 1` rather than `Inf`
  curve$.threshold[length(curve$.threshold)] <- Inf

  curve <- tibble::tibble(!!!curve)
  curve <- dplyr::select(curve, .threshold, specificity, sensitivity)

  class(curve) <- c("roc_df", class(curve))

  curve
}

py_roc_curve <- list(
  binary = make_py_two_class_roc_curve(case_weights = FALSE),
  case_weight = list(
    binary = make_py_two_class_roc_curve(case_weights = TRUE)
  )
)
saveRDS(py_roc_curve, test_path("py-data", "py-roc-curve.rds"), version = 2)

# ROC AUC Score
# - For binary, sklearn expects the probability of the class with the "greater
#   label", so if you sort c("Class1", "Class2") and take the 2nd one, you get
#   Class2, which is what we have to provide.
# - Neither sklearn nor yardstick support case weights in combination with the
#   Hand Till method
roc_auc_hpc_cv_levels <- sort(levels(hpc_cv$obs))
roc_auc_hpc_cv_estimate <- as.matrix(
  dplyr::select(hpc_cv, !!!syms(roc_auc_hpc_cv_levels))
)

py_roc_auc <- list(
  binary = skmetrics$roc_auc_score(
    two_class_example$truth,
    two_class_example$Class2
  ),
  macro = skmetrics$roc_auc_score(
    hpc_cv$obs,
    roc_auc_hpc_cv_estimate,
    average = "macro",
    labels = roc_auc_hpc_cv_levels,
    multi_class = "ovr"
  ),
  macro_weighted = skmetrics$roc_auc_score(
    hpc_cv$obs,
    roc_auc_hpc_cv_estimate,
    average = "weighted",
    labels = roc_auc_hpc_cv_levels,
    multi_class = "ovr"
  ),
  hand_till = skmetrics$roc_auc_score(
    hpc_cv$obs,
    roc_auc_hpc_cv_estimate,
    average = "macro",
    labels = roc_auc_hpc_cv_levels,
    multi_class = "ovo"
  ),
  case_weight = list(
    binary = skmetrics$roc_auc_score(
      two_class_example$truth,
      two_class_example$Class2,
      sample_weight = weights_two_class_example
    ),
    macro = skmetrics$roc_auc_score(
      hpc_cv$obs,
      roc_auc_hpc_cv_estimate,
      average = "macro",
      labels = roc_auc_hpc_cv_levels,
      multi_class = "ovr",
      sample_weight = weights_hpc_cv
    ),
    macro_weighted = skmetrics$roc_auc_score(
      hpc_cv$obs,
      roc_auc_hpc_cv_estimate,
      average = "weighted",
      labels = roc_auc_hpc_cv_levels,
      multi_class = "ovr",
      sample_weight = weights_hpc_cv
    )
  )
)
saveRDS(py_roc_auc, test_path("py-data", "py-roc-auc.rds"), version = 2)

# Brier survival
lung_surv <- data_lung_surv()
lung_surv_unnest <- tidyr::unnest(lung_surv, cols = c(.pred))

sksurv_obj <- sksurv_util$Surv$from_arrays(
  event = lung_surv$surv_obj[, "status"],
  time = lung_surv$surv_obj[, "time"]
)

eval_time_unique <- unique(lung_surv_unnest$.eval_time)

py_brier_survival <- vapply(
  X = eval_time_unique,
  FUN.VALUE = numeric(1),
  FUN = function(x) {
    sksurv_metrics$brier_score(
      sksurv_obj,
      sksurv_obj,
      dplyr::filter(lung_surv_unnest, .eval_time == x)$.pred_survival,
      x
    )[[2]]
  }
)

names(py_brier_survival) <- eval_time_unique
saveRDS(
  py_brier_survival,
  test_path("py-data", "py-brier-survival.rds"),
  version = 2
)

# Fairness metrics via fairlearn
py_demographic_parity <-
  list(
    binary = fairlearn$demographic_parity_difference(
      hpc_cv$obs == "VF",
      hpc_cv$pred == "VF",
      sensitive_features = hpc_cv$Resample
    ),
    weighted = fairlearn$demographic_parity_difference(
      hpc_cv$obs == "VF",
      hpc_cv$pred == "VF",
      sensitive_features = hpc_cv$Resample,
      sample_weight = weights_hpc_cv
    )
  )

saveRDS(
  py_demographic_parity,
  test_path("py-data", "py-demographic_parity.rds"),
  version = 2
)

py_equalized_odds <-
  list(
    binary = fairlearn$equalized_odds_difference(
      hpc_cv$obs == "VF",
      hpc_cv$pred == "VF",
      sensitive_features = hpc_cv$Resample
    ),
    weighted = fairlearn$equalized_odds_difference(
      hpc_cv$obs == "VF",
      hpc_cv$pred == "VF",
      sensitive_features = hpc_cv$Resample,
      sample_weight = weights_hpc_cv
    )
  )

saveRDS(
  py_equalized_odds,
  test_path("py-data", "py-equalized_odds.rds"),
  version = 2
)

py_equal_opportunity <-
  list(
    binary = fairlearn$true_positive_rate_difference(
      hpc_cv$obs == "VF",
      hpc_cv$pred == "VF",
      sensitive_features = hpc_cv$Resample
    ),
    weighted = fairlearn$true_positive_rate_difference(
      hpc_cv$obs == "VF",
      hpc_cv$pred == "VF",
      sensitive_features = hpc_cv$Resample,
      sample_weight = weights_hpc_cv
    )
  )

saveRDS(
  py_equal_opportunity,
  test_path("py-data", "py-equal_opportunity.rds"),
  version = 2
)
