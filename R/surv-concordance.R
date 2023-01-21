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
#' @inheritParams pr_auc
#'
#' @param data A `data.frame` containing the columns specified by `truth` and
#' `estimate`.
#'
#' @param truth The column identifier for the true class survival result (that
#' is created using [survival::Surv()].). This should be an unquoted column name
#' although this argument is passed by expression and supports
#' [quasiquotation][rlang::quasiquotation] (you can unquote column names). For
#' `_vec()` functions, an [survival::Surv()] object.
#'
#' @param estimate The column identifier for the survival probabilities. This
#' needs to be a numeric vector.This should be an unquoted column name although
#' this argument is passed by expression and supports
#' [quasiquotation][rlang::quasiquotation] (you can unquote column names). For
#' `_vec()` functions, a numeric vector.
#'
#' @param ... Not currently used.
#'
#' @author Emil Hvitfeldt
#'
#' @examples
#' concordance_survival(data = lung_surv, truth = surv_obj, estimate = age)
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
concordance_survival.data.frame <- function(data,
                                            truth,
                                            estimate,
                                            na_rm = TRUE,
                                            case_weights = NULL,
                                            ...) {
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
concordance_survival_vec <- function(truth,
                                     estimate,
                                     na_rm = TRUE,
                                     case_weights = NULL,
                                     ...) {
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
  }

  survival::concordance(truth ~ estimate, weights = case_weights)$concordance
}
