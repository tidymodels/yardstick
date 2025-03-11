#' Concordance index for right-censored data
#'
#' Compute the Concordance index for right-censored data
#'
#' @family static survival metrics
#' @templateVar fn concordance_survival
#' @template return
#' @details
#'
#' The concordance index is defined as the proportion of all comparable pairs in
#' which the predictions and outcomes are concordant.
#'
#' Two observations are comparable if:
#'
#' 1. both of the observations experienced an event (at different times), or
#' 2. the observation with the shorter observed survival time experienced an
#'    event, in which case the event-free subject “outlived” the other.
#'
#' A pair is not comparable if they experienced events at the same time.
#'
#' Concordance intuitively means that two samples were ordered correctly by the
#' model. More specifically, two samples are concordant, if the one with a
#' higher estimated risk score has a shorter actual survival time.
#'
#' Larger values of the score are associated with better model performance.
#'
#' @inheritParams brier_survival
#'
#' @param estimate The column identifier for the predicted time, this should be
#' a numeric variables. This should be an unquoted column name although this
#' argument is passed by expression and supports
#' [quasiquotation][rlang::quasiquotation] (you can unquote column names). For
#' `_vec()` functions, a numeric vector.
#'
#' @param ... Currently not used.
#'
#' @author Emil Hvitfeldt
#'
#' @references
#'
#' Harrell, F.E., Califf, R.M., Pryor, D.B., Lee, K.L., Rosati, R.A,
#' “Multivariable prognostic models: issues in developing models, evaluating
#' assumptions and adequacy, and measuring and reducing errors”, Statistics in
#' Medicine, 15(4), 361-87, 1996.
#'
#' @examples
#' concordance_survival(
#'   data = lung_surv,
#'   truth = surv_obj,
#'   estimate = .pred_time
#' )
#' @export
concordance_survival <- function(data, ...) {
  UseMethod("concordance_survival")
}

concordance_survival <- new_static_survival_metric(
  concordance_survival,
  direction = "maximize"
)

#' @rdname concordance_survival
#' @export
concordance_survival.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  static_survival_metric_summarizer(
    name = "concordance_survival",
    fn = concordance_survival_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname concordance_survival
concordance_survival_vec <- function(
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  check_static_survival_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  concordance_survival_impl(truth, estimate, case_weights)
}

concordance_survival_impl <- function(truth, estimate, case_weights) {
  if (is.null(case_weights)) {
    case_weights <- rep(1, length(estimate))
  } else {
    case_weights <- vec_cast(case_weights, to = double())
  }

  survival::concordance(truth ~ estimate, weights = case_weights)$concordance
}
