#' Concordance index for right-censored data
#'
#' Compute the Concordance index for right-censored data
#'
#' @family static survival metrics
#' @templateVar fn concordance_survival
#' @template return
#' @details TODO
#'
#' @inheritParams pr_auc
#'
#' @author Emil Hvitfeldt
#'
#' @export
concordance_survival <- function(data, ...) {
  UseMethod("concordance_survival")
}

concordance_survival <- new_dynamic_survival_metric(
  concordance_survival,
  direction = "minimize"
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
  check_survival_static_metric(truth, estimate, case_weights)

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
  survival::concordance(truth ~ estimate)$concordance
}
