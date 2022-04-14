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

  res <- rlang::flatten_dbl(one_vs_all_case_weights(
    metric_fn = binary_metric,
    truth = truth,
    estimate = prob_mat,
    case_weights = case_weights,
    ...
  ))

  mean(res)
}

hpc_fold1_macro_weighted_metric <- function(binary_metric, ...) {
  hpc_f1 <- data_hpc_fold1()
  wt <- as.vector(table(hpc_f1$obs))
  macro_wt <- wt / sum(wt)
  truth <- hpc_f1$obs
  prob_mat <- as.matrix(dplyr::select(hpc_f1, VF:L))
  case_weights <- NULL

  res <- rlang::flatten_dbl(one_vs_all_case_weights(
    metric_fn = binary_metric,
    truth = truth,
    estimate = prob_mat,
    case_weights = case_weights,
    ...
  ))

  stats::weighted.mean(res, macro_wt)
}

# TODO: Remove this and all below when all prob metrics support case weights
# These get embedded in the helper functions below
hpc_f1 <- data_hpc_fold1()
wt <- as.vector(table(hpc_f1$obs))
macro_wt <- wt / sum(wt)
truth <- hpc_f1$obs
prob_mat <- as.matrix(dplyr::select(hpc_f1, VF:L))

# Just pass in a binary metric function
prob_macro_metric <- function(binary_metric, ...) {
  res <- rlang::flatten_dbl(one_vs_all_impl(binary_metric, truth, prob_mat, ...))
  mean(res)
}

prob_macro_weighted_metric <- function(binary_metric, ...) {
  res <- rlang::flatten_dbl(one_vs_all_impl(binary_metric, truth, prob_mat, ...))
  stats::weighted.mean(res, macro_wt)
}
