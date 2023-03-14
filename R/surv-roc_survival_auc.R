#' ROC Survival AUC
#'
#' Compute the area under the ROC survival curve.
#'
#' @family dynamic survival metrics
#' @templateVar fn roc_auc_survival
#' @template return-dynamic-survival
#' @details
#'
#' This method automatically groups by the `eval_time` argument.
#'
#' Smaller values of the score are associated with better model performance.
#'
#' @seealso
#' Compute the ROC survival curve with [roc_curve_survival()].
#'
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
#'   roc_auc_survival(
#'     truth = surv_obj,
#'     estimate = .pred_survival,
#'     censoring_weights = ipcw,
#'     eval_time = .time
#'   )
#' @export
roc_auc_survival <- function(data, ...) {
  UseMethod("roc_auc_survival")
}

roc_auc_survival <- new_dynamic_survival_metric(
  roc_auc_survival,
  direction = "minimize"
)

#' @rdname roc_auc_survival
#' @export
roc_auc_survival.data.frame <- function(data,
                                        truth,
                                        estimate,
                                        censoring_weights,
                                        eval_time,
                                        na_rm = TRUE,
                                        case_weights = NULL,
                                        ...) {

  dynamic_survival_metric_summarizer(
    name = "roc_auc_survival",
    fn = roc_auc_survival_vec,
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
#' @rdname roc_auc_survival
roc_auc_survival_vec <- function(truth,
                                 estimate,
                                 censoring_weights,
                                 eval_time,
                                 na_rm = TRUE,
                                 case_weights = NULL,
                                 ...) {
  # No checking since roc_curve_survival_vec() does checking
  curve <- roc_curve_survival_vec(truth, estimate, censoring_weights, eval_time)
  roc_trap_auc(curve$specificity, curve$sensitivity)
}

roc_trap_auc <- function(specificity, sensitivity) {
  not_na <- !is.na(sensitivity) & !is.na(specificity)
  sensitivity <- sensitivity[not_na]
  specificity <- specificity[not_na]

  auc( 1 - specificity, sensitivity)
}
