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

  new_order <- vctrs::vec_order(estimate)
  event_time <- event_time[new_order]
  delta <- delta[new_order]
  estimate <- estimate[new_order]
  censoring_weights <- censoring_weights[new_order]
  eval_time <- eval_time[new_order]

  res <- dplyr::tibble(.threshold = unique(c(0, estimate, 1)))
  obs_time_le_time <- event_time <= eval_time
  obs_time_gt_time <- event_time > eval_time
  n <- length(estimate)
  multiplier <- delta / (n * censoring_weights)

  sensitivity_denom <- sum(obs_time_le_time * multiplier, na.rm = TRUE)
  specificity_denom <- sum(obs_time_gt_time, na.rm = TRUE)

  new_sensitivity <- data.frame(x = obs_time_le_time, y = multiplier)
  new_sensitivity <- split(new_sensitivity, estimate)
  new_sensitivity <- vapply(
    new_sensitivity,
    function(x) sum(x$x * x$y, na.rm = TRUE),
    FUN.VALUE = numeric(1)
  )
  new_sensitivity <- cumsum(new_sensitivity)
  new_sensitivity <- new_sensitivity / sensitivity_denom
  new_sensitivity <- c(0, new_sensitivity, 1)
  res$sensitivity <- new_sensitivity

  new_specificity <- split(obs_time_gt_time, estimate)
  new_specificity <- vapply(
    new_specificity,
    sum,
    na.rm = TRUE,
    FUN.VALUE = numeric(1)
  )
  new_specificity <- cumsum(new_specificity)
  new_specificity <- new_specificity / specificity_denom
  new_specificity <- c(0, new_specificity, 1)
  new_specificity <- 1 - new_specificity
  res$specificity <- new_specificity

  res
}

# Dynamically exported
autoplot.roc_survival_df <- autoplot.roc_df
