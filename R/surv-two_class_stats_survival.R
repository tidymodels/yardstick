#' Time-Dependent Top-Class Statistics for Censored Data
#'
#' Compute various statistics associated with a classification problem with two
#' classes using predicted survival probabilities that correspond to different
#' time points. The statistics include sensitivity, specificity, positive
#' predictive value, negative predictive value, and prevalence.
#'
#' @family dynamic survival metrics
#' @templateVar fn two_class_stats_survival
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
#'   two_class_stats_survival(
#'     truth = surv_obj,
#'     .pred
#'   )
#' @export
two_class_stats_survival <- function(data, ...) {
  UseMethod("two_class_stats_survival")
}

two_class_stats_survival <- new_dynamic_survival_metric(
  two_class_stats_survival,
  direction = "maximize"
)

#' @rdname two_class_stats_survival
#' @export
two_class_stats_survival.data.frame <- function(
    data,
    truth,
    ...,
    threshold = 1/2,
    na_rm = TRUE,
    case_weights = NULL
) {
  dynamic_survival_metric_summarizer(
    name = "two_class_stats_survival",
    fn = two_class_stats_survival_vec,
    data = data,
    truth = !!enquo(truth),
    ...,
    fn_options = list(threshold = threshold),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname two_class_stats_survival
two_class_stats_survival_vec <- function(
    truth,
    estimate,
    threshold = threshold,
    na_rm = TRUE,
    case_weights = NULL,
    ...
) {
  # No checking since roc_curve_survival_vec() does checking
  curve <- roc_curve_survival_vec(
    truth = truth,
    estimate = estimate,
    thresholds = threshold,
    na_rm = na_rm,
    case_weights = case_weights
  )
  browser()
  curve |>
    dplyr::group_by(.eval_time) |>
    dplyr::summarize(.estimate = roc_trap_auc(specificity, sensitivity))
}

