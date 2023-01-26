#' integrated Brier score for right censored data
#'
#' Compute the integrated Brier score for right censored data.
#'
#' @family dynamic survival metrics
#' @templateVar fn integrated_brier_survival
#' @template return-dynamic-survival
#' @details
#'
#' Smaller values of the score are associated with better model performance.
#'
#' @inheritParams pr_auc
#'
#' @param data A `data.frame` containing the columns specified by `truth` and
#' `estimate`.
#'
#' @param truth The column identifier for the true class survival result (that
#' is created using [survival::Surv()].). This should be an unquoted column name
#' although this argument is passed by expression and supports
#' [quasiquotation][rlang::quasiquotation] (you can unquote column names). For
#' `_vec()` functions, an [survival::Surv()] object.
#'
#' @param estimate The column identifier for the survival probabilities. This
#' should be a numeric vector. This should be an unquoted column name although
#' this argument is passed by expression and supports
#'  [quasiquotation][rlang::quasiquotation] (you can unquote column names). For
#' `_vec()` functions, a numeric vector.
#'
#' @param censoring_weights The column identifier for censoring weights. This is
#' expected to a numeric vector. This should be an unquoted column name although
#' this argument is passed by expression and supports
#' [quasiquotation][rlang::quasiquotation] (you can unquote column names). For
#' `_vec()` functions, a numeric vector.
#'
#' @param .time The column identifier for the time point. This
#' should be a numeric vector. This should be an unquoted column name although
#' this argument is passed by expression and supports
#' [quasiquotation][rlang::quasiquotation] (you can unquote column names). For
#' `_vec()` functions, a numeric vector.
#'
#' @param ... Not currently used.
#'
#' @author Emil Hvitfeldt
#'
#' @references
#'   E. Graf, C. Schmoor, W. Sauerbrei, and M. Schumacher, “Assessment and
#'   comparison of prognostic classification schemes for survival data,”
#'   Statistics in Medicine, vol. 18, no. 17-18, pp. 2529–2545, 1999.
#'
#' @examples
#' library(dplyr)
#'
#' lung_surv %>%
#'   integrated_brier_survival(
#'     truth = surv_obj,
#'     estimate = .pred_survival,
#'     censoring_weights = prob_censored,
#'     .time = .time
#'   )
#' @export
integrated_brier_survival <- function(data, ...) {
  UseMethod("integrated_brier_survival")
}

integrated_brier_survival <- new_dynamic_survival_metric(
  integrated_brier_survival,
  direction = "minimize"
)

#' @rdname integrated_brier_survival
#' @export
integrated_brier_survival.data.frame <- function(data,
                                      truth,
                                      estimate,
                                      censoring_weights,
                                      .time,
                                      na_rm = TRUE,
                                      case_weights = NULL,
                                      ...) {
  dynamic_survival_metric_summarizer(
    name = "integrated_brier_survival",
    fn = integrated_brier_survival_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    censoring_weights = !!enquo(censoring_weights),
    .time = !!enquo(.time),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname integrated_brier_survival
integrated_brier_survival_vec <- function(truth,
                               estimate,
                               censoring_weights,
                               .time,
                               na_rm = TRUE,
                               case_weights = NULL,
                               ...) {
  check_dynamic_survival_metric(
    truth, estimate, censoring_weights, case_weights, .time
  )

  if (na_rm) {
    result <- yardstick_remove_missing(
      truth, estimate, case_weights, censoring_weights, .time
    )

    truth <- result$truth
    estimate <- result$estimate
    censoring_weights <- result$censoring_weights
    .time <- result$.time
    case_weights <- result$case_weights
  } else {
    any_missing <- yardstick_any_missing(
      truth, estimate, case_weights, censoring_weights, .time
    )
    if (any_missing) {
      return(NA_real_)
    }
  }

  integrated_brier_survival_impl(truth, estimate, censoring_weights, case_weights, .time)
}

integrated_brier_survival_impl <- function(truth,
                                estimate,
                                censoring_weights,
                                case_weights,
                                .time) {

  if (is.null(case_weights)) {
    case_weights <- rep(1, length(estimate))
  }

  res <- dplyr::tibble(
    truth, estimate, censoring_weights, case_weights, .time
  ) %>%
    dplyr::group_by(.time) %>%
    dplyr::summarise(
      bs = brier_survival_impl(
        truth = truth,
        estimate = estimate,
        censoring_weights = censoring_weights,
        case_weights = case_weights,
        .time = .time
      )
    ) %>%
    dplyr::summarise(bs = trapezoidal_rule(bs, .time))

  res[["bs"]]
}

trapezoidal_rule <- function(x, t) {
  mean(x)
}
