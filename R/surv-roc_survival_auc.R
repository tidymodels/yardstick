#' ROC Survival AUC
#'
#' Compute the area under the ROC survival curve.
#'
#' @family dynamic survival metrics
#' @templateVar fn roc_survival_auc
#' @template return-dynamic-survival
#' @details
#'
#' This method automatically groups by the `eval_time` argument.
#'
#' Smaller values of the score are associated with better model performance.
#'
#' @seealso
#' Compute the ROC survival curve with [roc_survival_curve()].
#'
#' @inheritParams pr_auc
#' @inheritParams brier_survival
#'
#' @param ... Not currently used.
#'
#' @author Emil Hvitfeldt
#'
#' @examples
#' library(dplyr)
#'
#' lung_surv %>%
#'   roc_survival_auc(
#'     truth = surv_obj,
#'     estimate = .pred_survival,
#'     censoring_weights = ipcw,
#'     eval_time = .time
#'   )
#' @export
roc_survival_auc <- function(data, ...) {
  UseMethod("roc_survival_auc")
}

roc_survival_auc <- new_dynamic_survival_metric(
  roc_survival_auc,
  direction = "minimize"
)

#' @rdname roc_survival_auc
#' @export
roc_survival_auc.data.frame <- function(data,
                                      truth,
                                      estimate,
                                      censoring_weights,
                                      eval_time,
                                      na_rm = TRUE,
                                      case_weights = NULL,
                                      ...) {
  data <- dplyr::group_by(data, {{eval_time}})

  dynamic_survival_metric_summarizer(
    name = "roc_survival_auc",
    fn = roc_survival_auc_vec,
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
#' @rdname roc_survival_auc
roc_survival_auc_vec <- function(truth,
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

  roc_survival_auc_impl(truth, estimate, censoring_weights, case_weights, eval_time)
}

roc_survival_auc_vec <- function(truth, estimate, censor_probs, .time) {
  curve <- roc_survival_curve_vec(truth, estimate, censor_probs, .time)
  roc_trap_auc(curve$specificity, curve$sensitivity)
}

roc_trap_auc <- function(specificity, sensitivity) {
  not_na <- !is.na(sensitivity) & !is.na(specificity)
  sensitivity <- sensitivity[not_na]
  specificity <- specificity[not_na]

  trapezoidal_rule( 1 - specificity, sensitivity)
}
