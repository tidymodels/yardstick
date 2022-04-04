# reticulate::install_python("3.10.4")
# reticulate::virtualenv_create("yardstick-environment", version = "3.10.4")
# reticulate::use_virtualenv("yardstick-environment")
# reticulate::py_install("scikit-learn") # 1.0.2 is what was downloaded

library(reticulate)
library(purrr)

# Inside yardstick
devtools::load_all()

use_virtualenv("yardstick-environment")

skmetrics <- import("sklearn.metrics")

data("hpc_cv")
data("two_class_example")
data("solubility_test")

weights_hpc_cv <- read_weights_hpc_cv()
weights_two_class_example <- read_weights_two_class_example()
weights_solubility_test <- read_weights_solubility_test()

save_metric_results <- function(nm, fn, ..., average = c("macro", "micro", "weighted")) {

  # Two class metrics
  res <- list(fn(two_class_example$truth, two_class_example$predicted, ..., pos_label = "Class1"))

  # Multiclass metrics
  res2 <- lapply(average, function(.x) fn(hpc_cv$obs, hpc_cv$pred, ..., average = .x))

  # Two class weighted metrics
  case_weight <- list(fn(
    two_class_example$truth,
    two_class_example$predicted,
    ...,
    pos_label = "Class1",
    sample_weight = weights_two_class_example
  ))

  # Multiclass weighted metrics
  case_weight2 <- lapply(
    average,
    function(.x) fn(hpc_cv$obs, hpc_cv$pred, ..., average = .x, sample_weight = weights_hpc_cv)
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

py_binary_weighted_FP <- colSums(py_binary_weighted_confusion) - diag(py_binary_weighted_confusion)
py_binary_weighted_FN <- rowSums(py_binary_weighted_confusion) - diag(py_binary_weighted_confusion)
py_binary_weighted_TP <- diag(py_binary_weighted_confusion)
py_binary_weighted_TN <- sum(py_binary_weighted_confusion) - (py_binary_weighted_FP + py_binary_weighted_FN + py_binary_weighted_TP)

py_multiclass_weighted_FP <- colSums(py_multiclass_weighted_confusion) - diag(py_multiclass_weighted_confusion)
py_multiclass_weighted_FN <- rowSums(py_multiclass_weighted_confusion) - diag(py_multiclass_weighted_confusion)
py_multiclass_weighted_TP <- diag(py_multiclass_weighted_confusion)
py_multiclass_weighted_TN <- sum(py_multiclass_weighted_confusion) - (py_multiclass_weighted_FP + py_multiclass_weighted_FN + py_multiclass_weighted_TP)

# ------------------------------------------------------------------------------

# Save metrics results
save_metric_results("precision", skmetrics$precision_score)
save_metric_results("recall", skmetrics$recall_score)
save_metric_results("f_meas", skmetrics$f1_score)
save_metric_results("f_meas_beta_.5", skmetrics$fbeta_score, beta = .5)

# MCC
py_mcc <- list(
  binary = skmetrics$matthews_corrcoef(two_class_example$truth, two_class_example$predicted),
  multiclass = skmetrics$matthews_corrcoef(hpc_cv$obs, hpc_cv$pred),
  case_weight = list(
    binary = skmetrics$matthews_corrcoef(two_class_example$truth, two_class_example$predicted, sample_weight = weights_two_class_example),
    multiclass = skmetrics$matthews_corrcoef(hpc_cv$obs, hpc_cv$pred, sample_weight = weights_hpc_cv)
  )
)
saveRDS(py_mcc, test_path("py-data", "py-mcc.rds"), version = 2)

# Accuracy
py_accuracy <- list(
  binary = skmetrics$accuracy_score(two_class_example$truth, two_class_example$predicted),
  multiclass = skmetrics$accuracy_score(hpc_cv$obs, hpc_cv$pred)
)
saveRDS(py_accuracy, test_path("py-data", "py-accuracy.rds"), version = 2)

# Kappa
py_kap <- list(
  binary = skmetrics$cohen_kappa_score(two_class_example$truth, two_class_example$predicted, labels = levels(two_class_example$truth)),
  multiclass = skmetrics$cohen_kappa_score(hpc_cv$obs, hpc_cv$pred, labels = levels(hpc_cv$obs)),
  linear_binary = skmetrics$cohen_kappa_score(two_class_example$truth, two_class_example$predicted, labels = levels(two_class_example$truth), weights = "linear"),
  linear_multiclass = skmetrics$cohen_kappa_score(hpc_cv$obs, hpc_cv$pred, labels = levels(hpc_cv$obs), weights = "linear"),
  quadratic_binary = skmetrics$cohen_kappa_score(two_class_example$truth, two_class_example$predicted, labels = levels(two_class_example$truth), weights = "quadratic"),
  quadratic_multiclass = skmetrics$cohen_kappa_score(hpc_cv$obs, hpc_cv$pred, labels = levels(hpc_cv$obs), weights = "quadratic"),
  case_weight = list(
    binary = skmetrics$cohen_kappa_score(two_class_example$truth, two_class_example$predicted, labels = levels(two_class_example$truth), sample_weight = weights_two_class_example),
    multiclass = skmetrics$cohen_kappa_score(hpc_cv$obs, hpc_cv$pred, labels = levels(hpc_cv$obs), sample_weight = weights_hpc_cv),
    linear_binary = skmetrics$cohen_kappa_score(two_class_example$truth, two_class_example$predicted, labels = levels(two_class_example$truth), weights = "linear", sample_weight = weights_two_class_example),
    linear_multiclass = skmetrics$cohen_kappa_score(hpc_cv$obs, hpc_cv$pred, labels = levels(hpc_cv$obs), weights = "linear", sample_weight = weights_hpc_cv),
    quadratic_binary = skmetrics$cohen_kappa_score(two_class_example$truth, two_class_example$predicted, labels = levels(two_class_example$truth), weights = "quadratic", sample_weight = weights_two_class_example),
    quadratic_multiclass = skmetrics$cohen_kappa_score(hpc_cv$obs, hpc_cv$pred, labels = levels(hpc_cv$obs), weights = "quadratic", sample_weight = weights_hpc_cv)
  )
)
saveRDS(py_kap, test_path("py-data", "py-kap.rds"), version = 2)

# RMSE
py_rmse <- list(
  case_weight = sqrt(skmetrics$mean_squared_error(
    y_true = solubility_test$solubility,
    y_pred = solubility_test$prediction,
    sample_weight = weights_solubility_test
  ))
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
solubility_test_not_zero <- solubility_test[!zero_solubility,]
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
saveRDS(py_bal_accuracy, test_path("py-data", "py-bal-accuracy.rds"), version = 2)

# NPV
py_binary_weighted_NPV <- py_binary_weighted_TN / (py_binary_weighted_TN + py_binary_weighted_FN)
py_multiclass_weighted_NPV <- py_multiclass_weighted_TN / (py_multiclass_weighted_TN + py_multiclass_weighted_FN)

py_npv <- list(
  case_weight = list(
    binary = py_binary_weighted_NPV[[1]],
    macro = mean(py_multiclass_weighted_NPV)
  )
)
saveRDS(py_npv, test_path("py-data", "py-npv.rds"), version = 2)

# PPV
py_binary_weighted_PPV <- py_binary_weighted_TP / (py_binary_weighted_TP + py_binary_weighted_FP)
py_multiclass_weighted_PPV <- py_multiclass_weighted_TP / (py_multiclass_weighted_TP + py_multiclass_weighted_FP)

py_ppv <- list(
  case_weight = list(
    binary = py_binary_weighted_PPV[[1]],
    macro = mean(py_multiclass_weighted_PPV)
  )
)
saveRDS(py_ppv, test_path("py-data", "py-ppv.rds"), version = 2)
