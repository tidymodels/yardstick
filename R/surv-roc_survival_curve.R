#' ROC Survival Curve
#'
#' @family survival curve metrics
#' @templateVar fn roc_curve_survival
#'
#' @inheritParams brier_survival
#'
#' @return
#' A tibble with class `roc_survival_df`, `grouped_roc_survival_df` having
#' columns `.threshold`, `recall`, and `precision`.
#'
#' @seealso
#' Compute the area under the ROC survival curve with [roc_auc_survival()].
#'
#' @author Emil Hvitfeldt
#' @examples
#' result <- roc_curve_survival(
#'   lung_surv,
#'   truth = surv_obj,
#'   estimate = .pred_survival,
#'   censoring_weights = ipcw,
#'   eval_time = .time
#' )
#' result
#'
#' #' # ---------------------------------------------------------------------------
#' # `autoplot()`
#'
#' # Visualize the curve using ggplot2 manually
#' library(ggplot2)
#' library(dplyr)
#' result %>%
#'   ggplot(aes(x = 1 - specificity, y = sensitivity)) +
#'   geom_path() +
#'   geom_abline(lty = 3) +
#'   coord_equal() +
#'   theme_bw()
#'
#' # Or use autoplot
#' autoplot(result)
#' @export
roc_curve_survival <- function(data, ...) {
  UseMethod("roc_curve_survival")
}

#' @export
#' @rdname roc_curve_survival
roc_curve_survival.data.frame <- function(data,
                                          truth,
                                          estimate,
                                          censoring_weights,
                                          eval_time,
                                          na_rm = TRUE,
                                          case_weights = NULL,
                                          ...) {

  result <- curve_survival_metric_summarizer(
    name = "roc_curve_survival",
    fn = roc_curve_survival_vec,
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

roc_curve_survival_vec <- function(truth,
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

  roc_curve_survival_impl(
    truth = truth,
    estimate = estimate,
    censoring_weights = censoring_weights,
    eval_time = eval_time
  )
}

roc_curve_survival_impl <- function(truth,
                                    estimate,
                                    censoring_weights,
                                    eval_time) {
  event_time <- .extract_surv_time(truth)
  delta <- .extract_surv_status(truth)

  res <- dplyr::tibble(.threshold = sort(unique(c(0, estimate, 1))))

  obs_time_le_time <- event_time <= eval_time
  obs_time_gt_time <- event_time > eval_time
  n <- length(estimate)
  multiplier <- delta / (n * censoring_weights)

  sensitivity_denom <- sum(obs_time_le_time * multiplier, na.rm = TRUE)
  specificity_denom <- sum(obs_time_gt_time, na.rm = TRUE)

  data_df <- data.frame(
    le_time = obs_time_le_time,
    ge_time = obs_time_gt_time,
    multiplier = multiplier
  )
  data_split <- split(data_df, estimate)

  sensitivity <- vapply(
    data_split,
    function(x) sum(x$le_time * x$multiplier, na.rm = TRUE),
    FUN.VALUE = numeric(1)
  )
  sensitivity <- cumsum(sensitivity)
  sensitivity <- sensitivity / sensitivity_denom
  sensitivity <- c(0, sensitivity, 1)
  res$sensitivity <- sensitivity

  specificity <- vapply(
    data_split,
    function(x) sum(x$gt_time, na.rm = TRUE),
    FUN.VALUE = numeric(1)
  )
  specificity <- cumsum(specificity)
  specificity <- specificity / specificity_denom
  specificity <- c(0, specificity, 1)
  specificity <- 1 - specificity
  res$specificity <- specificity

  res
}

# Dynamically exported
autoplot.roc_survival_df <- autoplot.roc_df
