library(reticulate)
library(purrr)

skmetrics <- import("sklearn.metrics")
data("hpc_cv")
data("two_class_example")

save_metric_results <- function(nm, fn, ..., average = c("macro", "micro", "weighted")) {

  # Two class metrics
  res <- list(fn(two_class_example$truth, two_class_example$predicted, ..., pos_label = "Class1"))

  # Multiclass metrics
  res2 <- lapply(average, function(.x) fn(hpc_cv$obs, hpc_cv$pred, ..., average = .x))

  res <- c(res, res2)

  names(res) <- c("binary", average)
  saveRDS(res, paste0("tests/pycompare/py-", nm))
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
saveRDS(py_mcc, "tests/pycompare/py-mcc")

# Accuracy
py_accuracy <- list(
  binary = skmetrics$accuracy_score(two_class_example$truth, two_class_example$predicted),
  multiclass = skmetrics$accuracy_score(hpc_cv$obs, hpc_cv$pred)
)
saveRDS(py_accuracy, "tests/pycompare/py-accuracy")

# Kappa
py_kap <- list(
  binary = skmetrics$cohen_kappa_score(two_class_example$truth, two_class_example$predicted),
  multiclass = skmetrics$cohen_kappa_score(hpc_cv$obs, hpc_cv$pred)
)
saveRDS(py_kap, "tests/pycompare/py-kap")

# Kappa weighted
py_kap_weighted_linear <- list(
  binary = skmetrics$cohen_kappa_score(two_class_example$truth, two_class_example$predicted, weights = "linear"),
  multiclass = skmetrics$cohen_kappa_score(hpc_cv$obs, hpc_cv$pred, weights = "linear")
)
saveRDS(py_kap_weighted_linear, "tests/pycompare/py-kap-weighted-linear")

py_kap_weighted_quadratic <- list(
  binary = skmetrics$cohen_kappa_score(two_class_example$truth, two_class_example$predicted, weights = "quadratic"),
  multiclass = skmetrics$cohen_kappa_score(hpc_cv$obs, hpc_cv$pred, weights = "quadratic")
)
saveRDS(py_kap_weighted_quadratic, "tests/pycompare/py-kap-weighted-quadratic")
