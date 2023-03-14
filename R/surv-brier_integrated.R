#' Integrated Brier score for right censored data
#'
#' Compute the integrated Brier score for right censored data, which is an
#' overall calculation of model performance for all values of `eval_time`.
#'
#' @family dynamic survival metrics
#' @templateVar fn brier_survival_integrated
#' @template return-dynamic-survival
#' @details
#'
#' The integrated time-dependent brier score is calculated in an "area under the
#' curve" fashion. The brier score is calculated for each value of `eval_time`.
#' The area is calculated via the trapeziodal rule. The area is divided by the
#' largest value of `eval_time` to bring it into the same scale as the
#' traditional brier score.
#'
#' Smaller values of the score are associated with better model performance.
#'
#' @inheritParams brier_survival
#'
#' @author Emil Hvitfeldt
#'
#' @references E. Graf, C. Schmoor, W. Sauerbrei, and M. Schumacher, “Assessment
#' and comparison of prognostic classification schemes for survival data,”
#' Statistics in Medicine, vol. 18, no. 17-18, pp. 2529–2545, 1999.
#'
#' @examples
#' library(dplyr)
#'
#' lung_surv %>%
#'   brier_survival_integrated(
#'     truth = surv_obj,
#'     estimate = .pred_survival,
#'     censoring_weights = ipcw,
#'     eval_time = .time
#'   )
#' @export
brier_survival_integrated <- function(data, ...) {
  UseMethod("brier_survival_integrated")
}

brier_survival_integrated <- new_dynamic_survival_metric(
  brier_survival_integrated,
  direction = "minimize"
)

#' @rdname brier_survival_integrated
#' @export
brier_survival_integrated.data.frame <- function(data,
                                                 truth,
                                                 estimate,
                                                 censoring_weights,
                                                 eval_time,
                                                 na_rm = TRUE,
                                                 case_weights = NULL,
                                                 ...) {
  dynamic_survival_metric_summarizer(
    name = "brier_survival_integrated",
    fn = brier_survival_integrated_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    censoring_weights = !!enquo(censoring_weights),
    eval_time = !!enquo(eval_time),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname brier_survival_integrated
brier_survival_integrated_vec <- function(truth,
                                          estimate,
                                          censoring_weights,
                                          eval_time,
                                          na_rm = TRUE,
                                          case_weights = NULL,
                                          ...) {
  check_dynamic_survival_metric(
    truth, estimate, censoring_weights, case_weights, eval_time
  )

  if (na_rm) {
    result <- yardstick_remove_missing(
      truth, estimate, case_weights, censoring_weights, eval_time
    )

    truth <- result$truth
    estimate <- result$estimate
    censoring_weights <- result$censoring_weights
    eval_time <- result$eval_time
    case_weights <- result$case_weights
  } else {
    any_missing <- yardstick_any_missing(
      truth, estimate, case_weights, censoring_weights, eval_time
    )
    if (any_missing) {
      return(NA_real_)
    }
  }

  brier_survival_integrated_impl(
    truth,
    estimate,
    censoring_weights,
    case_weights,
    eval_time
  )
}

brier_survival_integrated_impl <- function(truth,
                                           estimate,
                                           censoring_weights,
                                           case_weights,
                                           eval_time) {
  unique_eval_times <- sort(unique(eval_time))

  brier_scores <- numeric(length(unique_eval_times))

  for (i in seq_along(unique_eval_times)) {
    index <- unique_eval_times[i] == eval_time
    brier_scores[i] <- brier_survival_vec(
      truth = truth[index],
      estimate = estimate[index],
      censoring_weights = censoring_weights[index],
      case_weights = case_weights[index],
      eval_time = eval_time[index]
    )
  }

  auc(unique_eval_times, brier_scores) / max(unique_eval_times)
}
