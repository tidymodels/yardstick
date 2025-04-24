#' Time-Dependent ROC AUC for Censored Data
#'
#' Compute the area under the ROC survival curve using predicted survival
#' probabilities that corresponds to different time points.
#'
#' @family dynamic survival metrics
#' @templateVar fn roc_auc_survival
#' @template return-dynamic-survival
#' @details
#'
#' This formulation takes survival probability predictions at one or more
#' specific _evaluation times_ and, for each time, computes the area under the
#' ROC curve. To account for censoring, inverse probability of censoring weights
#' (IPCW) are used in the calculations. See equation 7 of section 4.3 in
#' Blanche _at al_ (2013) for the details.
#'
#' The column passed to `...` should be a list column with one element per
#' independent experiential unit (e.g. patient). The list column should contain
#' data frames with several columns:
#'
#'  - `.eval_time`: The time that the prediction is made.
#'  - `.pred_survival`: The predicted probability of survival up to `.eval_time`
#'  - `.weight_censored`: The case weight for the inverse probability of censoring.
#'
#' The last column can be produced using [parsnip::.censoring_weights_graf()].
#' This corresponds to the weighting scheme of  Graf _et al_ (1999). The
#' internal data set `lung_surv` shows an example of the format.
#'
#' This method automatically groups by the `.eval_time` argument.
#'
#' Larger values of the score are associated with better model performance.
#'
#' @seealso
#' Compute the ROC survival curve with [roc_curve_survival()].
#'
#' @inheritParams brier_survival
#'
#' @author Emil Hvitfeldt
#'
#' @references
#'
#' Blanche, P., Dartigues, J.-F. and Jacqmin-Gadda, H. (2013), Review and
#' comparison of ROC curve estimators for a time-dependent outcome with
#' marker-dependent censoring. _Biom. J._, 55: 687-704.
#'
#' Graf, E., Schmoor, C., Sauerbrei, W. and Schumacher, M. (1999), Assessment
#' and comparison of prognostic classification schemes for survival data.
#' _Statist. Med._, 18: 2529-2545.
#'
#' @examplesIf rlang::is_installed(c("tidyr"))
#' library(dplyr)
#'
#' lung_surv |>
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
roc_auc_survival.data.frame <- function(
  data,
  truth,
  ...,
  na_rm = TRUE,
  case_weights = NULL
) {
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
roc_auc_survival_vec <- function(
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  # No checking since roc_curve_survival_vec() does checking
  curve <- roc_curve_survival_vec(
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    case_weights = case_weights
  )

  curve |>
    dplyr::group_by(.eval_time) |>
    dplyr::summarize(.estimate = roc_trap_auc(specificity, sensitivity))
}

roc_trap_auc <- function(specificity, sensitivity) {
  not_na <- !is.na(sensitivity) & !is.na(specificity)
  sensitivity <- sensitivity[not_na]
  specificity <- specificity[not_na]

  auc(1 - specificity, sensitivity)
}
