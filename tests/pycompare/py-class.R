library(reticulate)
library(purrr)
skmetrics <- import("sklearn.metrics")
data("hpc_cv")
data("two_class_example")

save_metric_results <- function(nm, fn, ..., average = c("macro", "micro", "weighted")) {
  res <- list(fn(two_class_example$truth, two_class_example$predicted, ..., pos_label = "Class1"))
  res2 <- lapply(average, function(.x) fn(hpc_cv$obs, hpc_cv$pred, ..., average = .x))
  res <- c(res, res2)
  names(res) <- c("binary", average)
  saveRDS(res, paste0("tests/pycompare/py-", nm))
}

save_metric_results("precision", skmetrics$precision_score)

save_metric_results("recall", skmetrics$recall_score)

save_metric_results("f_meas", skmetrics$f1_score)
save_metric_results("f_meas_beta_.5", skmetrics$fbeta_score, beta = .5)
