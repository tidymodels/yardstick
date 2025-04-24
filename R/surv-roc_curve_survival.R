#' Time-Dependent ROC surve for Censored Data
#'
#' Compute the ROC survival curve using predicted survival probabilities that
#' corresponds to different time points.
#'
#' @family survival curve metrics
#' @templateVar fn roc_curve_survival
#'
#' @inheritParams brier_survival
#'
#' @details
#'
#' This formulation takes survival probability predictions at one or more
#' specific _evaluation times_ and, for each time, computes the ROC curve. To
#' account for censoring, inverse probability of censoring weights (IPCW) are
#' used in the calculations. See equation 7 of section 4.3 in Blanche _at al_
#' (2013) for the details.
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
#' @return
#' A tibble with class `roc_survival_df`, `grouped_roc_survival_df` having
#' columns `.threshold`, `sensitivity`, `specificity`, and `.eval_time`.
#'
#' @seealso
#' Compute the area under the ROC survival curve with [roc_auc_survival()].
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
#' @examplesIf rlang::is_installed(c("ggplot2"))
#' result <- roc_curve_survival(
#'   lung_surv,
#'   truth = surv_obj,
#'   .pred
#' )
#' result
#'
#' # ---------------------------------------------------------------------------
#' # `autoplot()`
#'
#' # Visualize the curve using ggplot2 manually
#' library(ggplot2)
#' library(dplyr)
#' result |>
#'   mutate(.eval_time = format(.eval_time)) |>
#'   ggplot(aes(
#'     x = 1 - specificity, y = sensitivity,
#'     group = .eval_time, col = .eval_time
#'   )) +
#'   geom_step(direction = "hv") +
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
roc_curve_survival.data.frame <- function(
  data,
  truth,
  ...,
  na_rm = TRUE,
  case_weights = NULL
) {
  result <- curve_survival_metric_summarizer(
    name = "roc_curve_survival",
    fn = roc_curve_survival_vec,
    data = data,
    truth = !!enquo(truth),
    ...,
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )

  curve_finalize(result, data, "roc_survival_df", "grouped_roc_survival_df")
}

roc_curve_survival_vec <- function(
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  check_dynamic_survival_metric(
    truth,
    estimate,
    case_weights
  )

  if (na_rm) {
    result <- yardstick_remove_missing(
      truth,
      seq_along(estimate),
      case_weights
    )

    truth <- result$truth
    estimate <- estimate[result$estimate]
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    cli::cli_abort(
      c(
        x = "Missing values were detected and {.code na_ra = FALSE}.",
        i = "Not able to perform calculations."
      )
    )
  }

  roc_curve_survival_impl(
    truth = truth,
    estimate = estimate,
    case_weights = case_weights
  )
}

roc_curve_survival_impl <- function(truth, estimate, case_weights) {
  event_time <- .extract_surv_time(truth)
  delta <- .extract_surv_status(truth)
  case_weights <- vec_cast(case_weights, double())
  if (is.null(case_weights)) {
    case_weights <- rep(1, length(delta))
  }

  # Drop any `0` weights.
  # These shouldn't affect the result, but can result in wrong thresholds
  detect_zero_weight <- case_weights == 0
  if (any(detect_zero_weight)) {
    detect_non_zero_weight <- !detect_zero_weight
    event_time <- event_time[detect_non_zero_weight]
    delta <- delta[detect_non_zero_weight]
    case_weights <- case_weights[detect_non_zero_weight]
    estimate <- estimate[detect_non_zero_weight]
  }

  data <- dplyr::tibble(event_time, delta, case_weights, estimate)
  data <- tidyr::unnest(data, cols = estimate)

  .eval_times <- unique(data$.eval_time)

  not_missing_pred_survival <- !is.na(data$.pred_survival)

  out <- list()
  for (i in seq_along(.eval_times)) {
    .eval_time_ind <- .eval_times[[i]] == data$.eval_time &
      not_missing_pred_survival

    res <- roc_curve_survival_impl_one(
      data$event_time[.eval_time_ind],
      data$delta[.eval_time_ind],
      data[.eval_time_ind, ],
      data$case_weights[.eval_time_ind]
    )

    res$.eval_time <- .eval_times[[i]]
    out[[i]] <- res
  }

  dplyr::bind_rows(out)
}

roc_curve_survival_impl_one <- function(event_time, delta, data, case_weights) {
  res <- dplyr::tibble(
    .threshold = sort(
      unique(c(-Inf, data$.pred_survival, Inf)),
      decreasing = TRUE
    )
  )

  obs_time_le_time <- event_time <= data$.eval_time
  obs_time_gt_time <- event_time > data$.eval_time
  n <- nrow(data)

  sensitivity_denom <- sum(
    obs_time_le_time * delta * data$.weight_censored * case_weights,
    na.rm = TRUE
  )
  specificity_denom <- sum(
    obs_time_gt_time * data$.weight_censored * case_weights,
    na.rm = TRUE
  )

  data_df <- data.frame(
    le_time = obs_time_le_time,
    ge_time = obs_time_gt_time,
    delta = delta,
    weight_censored = data$.weight_censored,
    case_weights = case_weights
  )

  data_split <- vec_split(data_df, data$.pred_survival)
  data_split <- data_split$val[order(data_split$key)]

  specificity <- vapply(
    data_split,
    function(x)
      sum(x$ge_time * x$weight_censored * x$case_weights, na.rm = TRUE),
    FUN.VALUE = numeric(1)
  )
  specificity <- cumsum(specificity)
  specificity <- specificity / specificity_denom
  specificity <- dplyr::if_else(specificity > 1, 1, specificity)
  specificity <- dplyr::if_else(specificity < 0, 0, specificity)
  specificity <- c(0, specificity, 1)
  specificity <- 1 - specificity
  res$specificity <- specificity

  sensitivity <- vapply(
    data_split,
    function(x)
      sum(
        x$le_time * x$delta * x$weight_censored * x$case_weights,
        na.rm = TRUE
      ),
    FUN.VALUE = numeric(1)
  )

  sensitivity <- cumsum(sensitivity)
  sensitivity <- sensitivity / sensitivity_denom
  sensitivity <- dplyr::if_else(sensitivity > 1, 1, sensitivity)
  sensitivity <- dplyr::if_else(sensitivity < 0, 0, sensitivity)
  sensitivity <- c(0, sensitivity, 1)
  res$sensitivity <- sensitivity

  res
}

# Dynamically exported
autoplot.roc_survival_df <- function(object, ...) {
  `%+%` <- ggplot2::`%+%`
  object$.eval_time <- format(object$.eval_time)

  # Base chart
  roc_chart <- ggplot2::ggplot(data = object)

  # create aesthetic
  roc_aes <- ggplot2::aes(
    x = 1 - specificity,
    y = sensitivity,
    color = .eval_time,
    group = .eval_time
  )

  # build the graph
  roc_chart <- roc_chart %+%
    ggplot2::geom_step(mapping = roc_aes, direction = "hv") %+%
    ggplot2::geom_abline(lty = 3) %+%
    ggplot2::coord_equal() %+%
    ggplot2::theme_bw()

  roc_chart
}
