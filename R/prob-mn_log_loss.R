#' Mean log loss for multinomial data
#'
#' Compute the logarithmic loss of a classification model.
#'
#' Log loss is a measure of the performance of a classification model. A
#' perfect model has a log loss of `0`.
#'
#' Compared with [accuracy()], log loss
#' takes into account the uncertainty in the prediction and gives a more
#' detailed view into the actual performance. For example, given two input
#' probabilities of `.6` and `.9` where both are classified as predicting
#' a positive value, say, `"Yes"`, the accuracy metric would interpret them
#' as having the same value. If the true output is `"Yes"`, log loss penalizes
#' `.6` because it is "less sure" of its result compared to the probability
#' of `.9`.
#'
#' @family class probability metrics
#' @templateVar fn mn_log_loss
#' @template return
#'
#' @section Multiclass:
#' Log loss has a known multiclass extension, and is simply the sum of the
#' log loss values for each class prediction. Because of this, no averaging
#' types are supported.
#'
#' @inheritParams pr_auc
#'
#' @param sum A `logical`. Should the sum of the likelihood contributions be
#' returned (instead of the mean value)?
#'
#' @author Max Kuhn
#'
#' @examples
#' # Two class
#' data("two_class_example")
#' mn_log_loss(two_class_example, truth, Class1)
#'
#' # Multiclass
#' library(dplyr)
#' data(hpc_cv)
#'
#' # You can use the col1:colN tidyselect syntax
#' hpc_cv |>
#'   filter(Resample == "Fold01") |>
#'   mn_log_loss(obs, VF:L)
#'
#' # Groups are respected
#' hpc_cv |>
#'   group_by(Resample) |>
#'   mn_log_loss(obs, VF:L)
#'
#'
#' # Vector version
#' # Supply a matrix of class probabilities
#' fold1 <- hpc_cv |>
#'   filter(Resample == "Fold01")
#'
#' mn_log_loss_vec(
#'   truth = fold1$obs,
#'   matrix(
#'     c(fold1$VF, fold1$F, fold1$M, fold1$L),
#'     ncol = 4
#'   )
#' )
#'
#' # Supply `...` with quasiquotation
#' prob_cols <- levels(two_class_example$truth)
#' mn_log_loss(two_class_example, truth, Class1)
#' mn_log_loss(two_class_example, truth, !!prob_cols[1])
#'
#' @export
mn_log_loss <- function(data, ...) {
  UseMethod("mn_log_loss")
}
mn_log_loss <- new_prob_metric(
  mn_log_loss,
  direction = "minimize"
)

#' @export
#' @rdname mn_log_loss
mn_log_loss.data.frame <- function(
  data,
  truth,
  ...,
  na_rm = TRUE,
  sum = FALSE,
  event_level = yardstick_event_level(),
  case_weights = NULL
) {
  prob_metric_summarizer(
    name = "mn_log_loss",
    fn = mn_log_loss_vec,
    data = data,
    truth = !!enquo(truth),
    ...,
    na_rm = na_rm,
    event_level = event_level,
    case_weights = !!enquo(case_weights),
    # Extra argument for mn_log_loss_impl()
    fn_options = list(sum = sum)
  )
}

#' @rdname mn_log_loss
#' @export
mn_log_loss_vec <- function(
  truth,
  estimate,
  na_rm = TRUE,
  sum = FALSE,
  event_level = yardstick_event_level(),
  case_weights = NULL,
  ...
) {
  abort_if_class_pred(truth)

  estimator <- finalize_estimator(truth, metric_class = "mn_log_loss")

  check_prob_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  mn_log_loss_estimator_impl(
    truth = truth,
    estimate = estimate,
    estimator = estimator,
    event_level = event_level,
    sum = sum,
    case_weights = case_weights
  )
}

mn_log_loss_estimator_impl <- function(
  truth,
  estimate,
  estimator,
  event_level,
  sum,
  case_weights
) {
  if (is_binary(estimator)) {
    mn_log_loss_binary(
      truth = truth,
      estimate = estimate,
      event_level = event_level,
      sum = sum,
      case_weights = case_weights
    )
  } else {
    mn_log_loss_multiclass(
      truth = truth,
      estimate = estimate,
      sum = sum,
      case_weights = case_weights
    )
  }
}

mn_log_loss_binary <- function(
  truth,
  estimate,
  event_level,
  sum,
  case_weights
) {
  if (!is_event_first(event_level)) {
    lvls <- levels(truth)
    truth <- stats::relevel(truth, lvls[[2]])
  }

  estimate <- matrix(c(estimate, 1 - estimate), ncol = 2)

  mn_log_loss_multiclass(
    truth = truth,
    estimate = estimate,
    sum = sum,
    case_weights = case_weights
  )
}

# We apply the min/max rule to avoid undefined log() values (#103)
# (Standard seems to be to use `eps = 1e-15`, but base R uses
# .Machine$double.eps in many places when they do this,
# and it should be more precise)
# https://github.com/wch/r-source/blob/582d94805aeee0c91f9bd9bdd63e421dd60e441f/src/library/stats/R/family.R#L83

mn_log_loss_multiclass <- function(truth, estimate, sum, case_weights) {
  # Binarize factor
  y <- stats::model.matrix(~ truth - 1)

  eps <- .Machine$double.eps
  estimate <- pmax(pmin(estimate, 1 - eps), eps)

  loss <- y * log(estimate)
  loss <- rowSums(loss)
  loss <- -loss

  if (sum) {
    yardstick_sum(loss, case_weights = case_weights)
  } else {
    yardstick_mean(loss, case_weights = case_weights)
  }
}
