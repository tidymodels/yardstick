#' General Function to Estimate Performance
#'
#' This function estimates one or more common performance estimates depending
#' on the class of `truth` (see **Value** below) and returns them in a three
#' column tibble. If you wish to modify the metrics used or how they are used
#' see [metric_set()].
#'
#' @inheritParams roc_auc
#'
#' @param data A `data.frame` containing the columns specified by `truth`,
#' `estimate`, and `...`.
#'
#' @param truth The column identifier for the true results (that
#' is `numeric` or `factor`). This should be an unquoted column name
#' although this argument is passed by expression and support
#' [quasiquotation][rlang::quasiquotation] (you can unquote column
#' names).
#'
#' @param estimate The column identifier for the predicted results
#' (that is also `numeric` or `factor`). As with `truth` this can be
#' specified different ways but the primary method is to use an
#' unquoted variable name.
#'
#' @return
#'
#' A three column tibble.
#'
#' * When `truth` is a factor, there are rows for [accuracy()] and the
#' Kappa statistic ([kap()]).
#'
#' * When `truth` has two levels and 1 column of class probabilities is
#' passed to `...`, there are rows for the two class versions of
#' [mn_log_loss()] and [roc_auc()].
#'
#' * When `truth` has more than two levels and a full set of class probabilities
#' are passed to `...`, there are rows for the multiclass version of
#' [mn_log_loss()] and the Hand Till generalization of [roc_auc()].
#'
#' * When `truth` is numeric, there are rows for [rmse()], [rsq()],
#' and [mae()].
#'
#' @seealso [metric_set()]
#'
#' @examples
#'
#' # Accuracy and kappa
#' metrics(two_class_example, truth, predicted)
#'
#' # Add on multinomal log loss and ROC AUC by specifying class prob columns
#' metrics(two_class_example, truth, predicted, Class1)
#'
#' # Regression metrics
#' metrics(solubility_test, truth = solubility, estimate = prediction)
#'
#' # Multiclass metrics work, but you cannot specify any averaging
#' # for roc_auc() besides the default, hand_till. Use the specific function
#' # if you need more customization
#' library(dplyr)
#'
#' hpc_cv |>
#'   group_by(Resample) |>
#'   metrics(obs, pred, VF:L) |>
#'   print(n = 40)
#'
#' @export metrics
metrics <- function(data, ...) {
  UseMethod("metrics")
}

#' @export
#' @rdname metrics
metrics.data.frame <- function(
  data,
  truth,
  estimate,
  ...,
  na_rm = TRUE,
  options = list()
) {
  check_roc_options_deprecated("metrics", options)

  names <- names(data)

  truth <- tidyselect::vars_pull(names, {{ truth }})
  estimate <- tidyselect::vars_pull(names, {{ estimate }})
  probs <- names(tidyselect::eval_select(rlang::expr(c(...)), data))

  is_class <- is.factor(data[[truth]]) || is_class_pred(data[[truth]])

  if (is_class) {
    metrics_class <- metric_set(accuracy, kap)
    res <- metrics_class(data, !!truth, estimate = !!estimate, na_rm = na_rm)

    if (length(probs) > 0L) {
      res2 <- mn_log_loss(data, !!truth, !!probs, na_rm = na_rm)
      res3 <- roc_auc(data, !!truth, !!probs, na_rm = na_rm)
      res <- dplyr::bind_rows(res, res2, res3)
    }
  } else {
    # Assume only regression for now
    metrics_regression <- metric_set(rmse, rsq, mae)

    res <- metrics_regression(
      data = data,
      truth = !!truth,
      estimate = !!estimate,
      na_rm = na_rm
    )
  }

  res
}
