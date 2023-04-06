#' ROC Survival AUC
#'
#' Compute the area under the ROC survival curve.
#'
#' @family dynamic survival metrics
#' @templateVar fn roc_auc_survival
#' @template return-dynamic-survival
#' @details
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
#'     .pred
#'   )
#' @export
roc_auc_survival <- function(data, ...) {
  UseMethod("roc_auc_survival")
}

roc_auc_survival <- new_dynamic_survival_metric(
  roc_auc_survival,
  direction = "maximize"
)

#' @rdname roc_auc_survival
#' @export
roc_auc_survival.data.frame <- function(data,
                                        truth,
                                        ...,
                                        na_rm = TRUE,
                                        case_weights = NULL) {

  dynamic_survival_metric_summarizer(
    name = "roc_auc_survival",
    fn = roc_auc_survival_vec,
    data = data,
    truth = !!enquo(truth),
    ...,
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname roc_auc_survival
roc_auc_survival_vec <- function(truth,
                                 estimate,
                                 na_rm = TRUE,
                                 case_weights = NULL,
                                 ...) {
  # No checking since roc_curve_survival_vec() does checking
  curve <- roc_curve_survival_vec(truth, estimate)
  roc_trap_auc(curve$specificity, curve$sensitivity)
}

roc_trap_auc <- function(specificity, sensitivity) {
  not_na <- !is.na(sensitivity) & !is.na(specificity)
  sensitivity <- sensitivity[not_na]
  specificity <- specificity[not_na]

  auc(1 - specificity, sensitivity)
}
