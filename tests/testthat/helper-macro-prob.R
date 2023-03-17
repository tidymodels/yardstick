# These are helpers for class prob macro / macro_weighted tests

data_hpc_fold1 <- function() {
  data("hpc_cv")
  dplyr::filter(hpc_cv, Resample == "Fold01")
}

hpc_fold1_macro_metric <- function(binary_metric, ...) {
  hpc_f1 <- data_hpc_fold1()
  truth <- hpc_f1$obs
  prob_mat <- as.matrix(dplyr::select(hpc_f1, VF:L))
  case_weights <- NULL

  res <- one_vs_all_impl(
    fn = binary_metric,
    truth = truth,
    estimate = prob_mat,
    case_weights = case_weights,
    ...
  )
  res <- vapply(res, FUN.VALUE = numeric(1), function(x) x)

  mean(res)
}

hpc_fold1_macro_weighted_metric <- function(binary_metric, ...) {
  hpc_f1 <- data_hpc_fold1()
  wt <- as.vector(table(hpc_f1$obs))
  macro_wt <- wt / sum(wt)
  truth <- hpc_f1$obs
  prob_mat <- as.matrix(dplyr::select(hpc_f1, VF:L))
  case_weights <- NULL

  res <- one_vs_all_impl(
    fn = binary_metric,
    truth = truth,
    estimate = prob_mat,
    case_weights = case_weights,
    ...
  )

  res <- vapply(res, FUN.VALUE = numeric(1), function(x) x)

  stats::weighted.mean(res, macro_wt)
}
