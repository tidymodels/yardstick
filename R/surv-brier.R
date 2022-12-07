#' Brier score for censored data models
#'
#' Compute the Brier score for a censored data model.
#'
#' @family dynamic survival metrics
#' @templateVar fn brier_surv
#' @template return
#' @details
#'
#' Smaller values of the score are associated with better model performance.
#'
#' @section Multiclass:
#' Brier scores can be computed in the same way for any number of classes.
#' Because of this, no averaging types are supported.
#'
#' @inheritParams pr_auc
#' @param .time A vector of time points.
#'
#' @author Emil Hvitfeldt
#'
#' @export
brier_surv <- function(data, ...) {
  UseMethod("brier_surv")
}
brier_surv <- new_surv_metric(
  brier_surv,
  direction = "minimize"
)

#' @rdname brier_surv
#' @export
brier_surv.data.frame <- function(data,
                                  truth,
                                  estimate,
                                  .time,
                                  na_rm = TRUE,
                                  case_weights = NULL,
                                  ...) {
  surv_dynamic_metric_summarizer(
    name = "brier_surv",
    fn = brier_surv_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    .time = .time,
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname brier_surv
brier_surv_vec <- function(truth,
                           estimate,
                           .time,
                           na_rm = TRUE,
                           case_weights = NULL,
                           ...) {
  check_surv_dynamic_metric(truth, estimate, case_weights, .time)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  brier_surv_impl(truth, estimate, case_weights, .time)
}

brier_surv_impl <- function(truth, estimate, case_weights, .time) {
  res <- numeric(length(.time))

  .time
}

