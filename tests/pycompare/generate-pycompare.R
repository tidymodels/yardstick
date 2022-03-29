library(reticulate)
library(purrr)

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

  saveRDS(res, paste0("tests/pycompare/py-", nm), version = 2)
}

# ------------------------------------------------------------------------------

# Save metrics results
save_metric_results("precision", skmetrics$precision_score)
save_metric_results("recall", skmetrics$recall_score)
save_metric_results("f_meas", skmetrics$f1_score)
save_metric_results("f_meas_beta_.5", skmetrics$fbeta_score, beta = .5)

# MCC
py_mcc <- list(
  binary = skmetrics$matthews_corrcoef(two_class_example$truth, two_class_example$predicted),
  multiclass = skmetrics$matthews_corrcoef(hpc_cv$obs, hpc_cv$pred)
)
saveRDS(py_mcc, "tests/pycompare/py-mcc", version = 2)

# Accuracy
py_accuracy <- list(
  binary = skmetrics$accuracy_score(two_class_example$truth, two_class_example$predicted),
  multiclass = skmetrics$accuracy_score(hpc_cv$obs, hpc_cv$pred)
)
saveRDS(py_accuracy, "tests/pycompare/py-accuracy", version = 2)

# Kappa
py_kap <- list(
  binary = skmetrics$cohen_kappa_score(two_class_example$truth, two_class_example$predicted, levels(two_class_example$truth)),
  multiclass = skmetrics$cohen_kappa_score(hpc_cv$obs, hpc_cv$pred, labels = levels(hpc_cv$obs)),
  linear_binary = skmetrics$cohen_kappa_score(two_class_example$truth, two_class_example$predicted, levels(two_class_example$truth), weights = "linear"),
  linear_multiclass = skmetrics$cohen_kappa_score(hpc_cv$obs, hpc_cv$pred, labels = levels(hpc_cv$obs), weights = "linear"),
  quadratic_binary = skmetrics$cohen_kappa_score(two_class_example$truth, two_class_example$predicted, levels(two_class_example$truth), weights = "quadratic"),
  quadratic_multiclass = skmetrics$cohen_kappa_score(hpc_cv$obs, hpc_cv$pred, labels = levels(hpc_cv$obs), weights = "quadratic")
)
saveRDS(py_kap, "tests/pycompare/py-kap", version = 2)

# RMSE
py_rmse <- list(
  case_weight = sqrt(skmetrics$mean_squared_error(
    y_true = solubility_test$solubility,
    y_pred = solubility_test$prediction,
    sample_weight = weights_solubility_test
  ))
)
saveRDS(py_rmse, "tests/pycompare/py-rmse", version = 2)
