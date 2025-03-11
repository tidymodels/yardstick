#' Positive predictive value
#'
#' These functions calculate the [ppv()] (positive predictive value) of a
#' measurement system compared to a reference result (the "truth" or gold standard).
#' Highly related functions are [spec()], [sens()], and [npv()].
#'
#' The positive predictive value ([ppv()]) is defined as the percent of
#' predicted positives that are actually positive while the
#' negative predictive value ([npv()]) is defined as the percent of negative
#' positives that are actually negative.
#'
#' @family class metrics
#' @family sensitivity metrics
#' @templateVar fn ppv
#' @template event_first
#' @template multiclass
#' @template return
#' @template table-positive
#'
#' @inheritParams sens
#'
#' @param prevalence A numeric value for the rate of the
#'  "positive" class of the data.
#'
#' @author Max Kuhn
#'
#' @references
#'
#' Altman, D.G., Bland, J.M. (1994) ``Diagnostic tests 2:
#' predictive values,'' *British Medical Journal*, vol 309,
#' 102.
#'
#' @template examples-class
#' @examples
#' # But what if we think that Class 1 only occurs 40% of the time?
#' ppv(two_class_example, truth, predicted, prevalence = 0.40)
#'
#' @export
ppv <- function(data, ...) {
  UseMethod("ppv")
}
ppv <- new_class_metric(
  ppv,
  direction = "maximize"
)

#' @rdname ppv
#' @export
ppv.data.frame <- function(
  data,
  truth,
  estimate,
  prevalence = NULL,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  class_metric_summarizer(
    name = "ppv",
    fn = ppv_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    case_weights = !!enquo(case_weights),
    event_level = event_level,
    fn_options = list(prevalence = prevalence)
  )
}

#' @export
ppv.table <- function(
  data,
  prevalence = NULL,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "ppv",
    .estimator = estimator,
    .estimate = ppv_table_impl(
      data,
      estimator = estimator,
      event_level = event_level,
      prevalence = prevalence
    )
  )
}

#' @export
ppv.matrix <- function(
  data,
  prevalence = NULL,
  estimator = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  data <- as.table(data)

  ppv.table(
    data,
    prevalence = prevalence,
    estimator = estimator,
    event_level = event_level
  )
}

#' @export
#' @rdname ppv
ppv_vec <- function(
  truth,
  estimate,
  prevalence = NULL,
  estimator = NULL,
  na_rm = TRUE,
  case_weights = NULL,
  event_level = yardstick_event_level(),
  ...
) {
  abort_if_class_pred(truth)
  estimate <- as_factor_from_class_pred(estimate)

  estimator <- finalize_estimator(truth, estimator)

  check_class_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  data <- yardstick_table(truth, estimate, case_weights = case_weights)
  ppv_table_impl(data, estimator, event_level, prevalence = prevalence)
}

ppv_table_impl <- function(data, estimator, event_level, prevalence = NULL) {
  if (is_binary(estimator)) {
    ppv_binary(data, event_level, prevalence)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- ppv_multiclass(data, estimator, prevalence)
    stats::weighted.mean(out_vec, w)
  }
}

ppv_binary <- function(data, event_level, prevalence = NULL) {
  positive <- pos_val(data, event_level)

  if (is.null(prevalence)) {
    prevalence <- sum(data[, positive]) / sum(data)
  }

  sens <- sens_binary(data, event_level)
  spec <- spec_binary(data, event_level)
  (sens * prevalence) / ((sens * prevalence) + ((1 - spec) * (1 - prevalence)))
}

ppv_multiclass <- function(data, estimator, prevalence = NULL) {
  # ppv should be equal to precision in all cases except when
  # prevalence is explicitely set. In that case, that value
  # is used which alters the result
  if (is.null(prevalence)) {
    tpfn <- colSums(data)
    tptnfpfn <- rep(sum(data), times = nrow(data))

    if (is_micro(estimator)) {
      tpfn <- sum(tpfn)
      tptnfpfn <- sum(tptnfpfn)
    }

    prevalence <- tpfn / tptnfpfn
  }

  .sens_vec <- recall_multiclass(data, estimator)
  .spec_vec <- spec_multiclass(data, estimator)

  numer <- .sens_vec * prevalence
  denom <- .sens_vec * prevalence + (1 - .spec_vec) * (1 - prevalence)

  denom[denom <= 0] <- NA_real_

  numer / denom
}
