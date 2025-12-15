#' Royston-Sauerbei D statistic
#'
#' Compute the Royston-Sauerbei D statistic
#'
#' @family linear pred survival metrics
#' @templateVar fn royston_survival
#' @template return
#' @details
#'
#' Royston and Sauerbrei proposed $R^2_D$ as a measure of explained variation
#' on the log relative hazard scale based on the authors’ D statistic.
#' D measures prognostic separation of survival curves, and is closely related
#' to the standard deviation of the prognostic index.
#'
#' Larger values of the score are associated with better model performance.
#'
#' @inheritParams brier_survival
#'
#' @param estimate The column identifier for the predicted linear predictor,
#' this should be a numeric variable. This should be an unquoted column name
#' although this argument is passed by expression and supports
#' [quasiquotation][rlang::quasiquotation] (you can unquote column names). For
#' `_vec()` functions, a numeric vector.
#'
#' @param ... Currently not used.
#'
#' @author Hannah Frick
#'
#' @references
#'
#' Royston, P., Sauerbrei, W., "A new measure of prognostic separation in
#' survival data", Statistics in Medicine, 23, 723-748, 2004.
#'
#' @examples
#' royston_survival(
#'   data = lung_surv,
#'   truth = surv_obj,
#'   estimate = .pred_linear_pred
#' )
#' @export
royston_survival <- function(data, ...) {
  UseMethod("royston_survival")
}
royston_survival <- new_linear_pred_survival_metric(
  royston_survival,
  direction = "maximize"
)

#' @export
royston_survival.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  linear_pred_survival_metric_summarizer(
    name = "royston_survival",
    fn = royston_survival_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
royston_survival_vec <- function(
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  check_linear_pred_survival_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  royston_survival_impl(truth, estimate, case_weights)
}

royston_survival_impl <- function(truth, estimate, case_weights) {
  if (is.null(case_weights)) {
    case_weights <- rep(1, length(estimate))
  } else {
    case_weights <- vec_cast(case_weights, to = double())
  }

  bns <- normal_score_blom(estimate, case_weights)

  fit <- survival::coxph(truth ~ bns, weights = case_weights)
  est <- unname(coef(fit))
  est^2 / (est^2 + pi^2 / 6)
}

normal_score_blom <- function(x, case_weights) {
  tibble::tibble(
    .row = rep(seq_along(x), times = case_weights),
    x = rep(x, times = case_weights),
  ) |>
    dplyr::mutate(
      x_first = rank(.data$x, ties.method = "first"),
      # does not need kappa (from Royston & Sauerbrei 2004) because it'll
      # "disappear" into the baseline hazard of the Cox model in
      # `royston_survival_impl()`
      z = qnorm((.data$x_first - 3 / 8) / (dplyr::n() + 0.25))
    ) |>
    # average over ties
    dplyr::mutate(s = mean(.data$z), .by = "x") |>
    dplyr::slice(1, .by = .row) |>
    dplyr::pull("s")
}
