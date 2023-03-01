#' ROC Survival Curve
#'
#' @family survival curve metrics
#' @templateVar fn roc_survival_curve
#'
#' @inheritParams brier_survival
#'
#' @return
#' A tibble with class `roc_survival_df`, `grouped_roc_survival_df` having
#' columns `.threshold`, `recall`, and `precision`.
#'
#' @seealso
#' Compute the area under the ROC survival curve with (TODO link to
#' roc_survival_auc()).
#'
#' @author Emil Hvitfeldt
#' @examples
#' roc_survival_curve(
#'   lung_surv,
#'   truth = surv_obj,
#'   estimate = .pred_survival,
#'   censoring_weights = ipcw,
#'   eval_time = .time
#' )
#' @export
roc_survival_curve <- function(data, ...) {
  UseMethod("roc_survival_curve")
}

#' @export
#' @rdname roc_survival_curve
roc_survival_curve.data.frame <- function(data,
                                          truth,
                                          estimate,
                                          censoring_weights,
                                          eval_time,
                                          na_rm = TRUE,
                                          case_weights = NULL,
                                          ...) {

  result <- curve_survival_metric_summarizer(
    name = "roc_survival_curve",
    fn = roc_survival_curve_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    censoring_weights = !!enquo(censoring_weights),
    eval_time = !!enquo(eval_time),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )

  curve_finalize(result, data, "roc_survival_df", "grouped_roc_survival_df")
}

roc_survival_curve_vec <- function(truth,
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

  roc_survival_curve_impl(
    truth = truth,
    estimate = estimate,
    censoring_weights = censoring_weights,
    eval_time = eval_time
  )
}

roc_survival_curve_impl <- function(truth,
                                    estimate,
                                    censoring_weights,
                                    eval_time) {
  res <- dplyr::tibble(.threshold = sort(unique(c(0, 1, estimate))))
  res$sensitivity <- vapply(
    res$.threshold,
    sensitivity_uno_2007,
    FUN.VALUE = numeric(1),
    eval_time, truth, estimate, censoring_weights
  )
  res$specificity <- vapply(
    res$.threshold,
    specificity_naive,
    FUN.VALUE = numeric(1),
    eval_time, truth, estimate
  )
  res
}

sensitivity_uno_2007 <- function(threshold,
                                 eval_time,
                                 surv_obj,
                                 prob_surv,
                                 prob_cens) {
  n <- length(prob_surv)
  event_time <- .extract_surv_time(surv_obj)
  delta <- .extract_surv_status(surv_obj)
  obs_time_le_time <- ifelse(event_time <= eval_time, 1, 0)
  # Since the "marker" X is the survival prob, X <= C means an event
  prob_le_thresh <- ifelse(prob_surv <= threshold, 1, 0)
  multiplier <- delta / (n * prob_cens)
  numer <- sum(obs_time_le_time * prob_le_thresh * multiplier, na.rm = TRUE)
  denom <- sum(obs_time_le_time * multiplier, na.rm = TRUE)
  numer / denom
}

specificity_naive <- function(threshold, eval_time, surv_obj, prob_surv) {
  event_time <- .extract_surv_time(surv_obj)
  delta <- .extract_surv_status(surv_obj)
  obs_time_gt_time <- ifelse(event_time > eval_time, 1, 0)
  # Since the "marker" X is the survival prob, X > C means no event
  prob_gt_thresh <- ifelse(prob_surv > threshold, 1, 0)
  numer <- sum(obs_time_gt_time * prob_gt_thresh, na.rm = TRUE)
  denom <- sum(obs_time_gt_time, na.rm = TRUE)
  numer / denom
}

# Wait for https://github.com/tidymodels/parsnip/pull/893 to be merged
.extract_surv_time <- function(surv) {
  .is_surv(surv)
  keepers <- c("time", "start", "stop", "time1", "time2")
  res <- surv[, colnames(surv) %in% keepers]
  if (NCOL(res) > 1) {
    res <- dplyr::tibble(as.data.frame(res))
  }
  res
}

.extract_surv_status <- function(surv) {
  .is_surv(surv)
  res <-   surv[, "status"]
  un_vals <- sort(unique(res))
  if (identical(un_vals, 1:2) | identical(un_vals, c(1.0, 2.0))) {
    res <- res - 1
  }
  res
}

.is_surv <- function(surv, fail = TRUE) {
  is_surv <- inherits(surv, "Surv")
  if (fail && !is_surv) {
    rlang::abort("The object does not have class `Surv`.", call = NULL)
  }
  is_surv
}
