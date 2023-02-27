#' Brier score for right censored data
#'
#' Compute the time-dependent Brier score for right censored data. Which is the
#' mean squared error at time point `eval_time`.
#'
#' @family dynamic survival metrics
#' @templateVar fn brier_survival
#' @template return-dynamic-survival
#' @details
#'
#' This method will automatically group by the `eval_time` argument.
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
#' @param eval_time The column identifier for the time point. This
#' should be a numeric vector, with 1 unique value for each group. This should
#' be an unquoted column name although this argument is passed by expression and
#' supports [quasiquotation][rlang::quasiquotation] (you can unquote column
#' names). For `_vec()` functions, a numeric vector.
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
#'   brier_survival(
#'     truth = surv_obj,
#'     estimate = .pred_survival,
#'     censoring_weights = ipcw,
#'     eval_time = .time
#'   )
#' @export
brier_survival <- function(data, ...) {
  UseMethod("brier_survival")
}

brier_survival <- new_dynamic_survival_metric(
  brier_survival,
  direction = "minimize"
)

#' @rdname brier_survival
#' @export
brier_survival.data.frame <- function(data,
                                      truth,
                                      estimate,
                                      censoring_weights,
                                      eval_time,
                                      na_rm = TRUE,
                                      case_weights = NULL,
                                      ...) {
  data <- dplyr::group_by(data, {{eval_time}})

  dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
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
#' @rdname brier_survival
brier_survival_vec <- function(truth,
                               estimate,
                               censoring_weights,
                               eval_time,
                               na_rm = TRUE,
                               case_weights = NULL,
                               ...) {
  check_dynamic_survival_metric(
    truth, estimate, censoring_weights, case_weights, eval_time
  )

  n_distinct_time <- dplyr::n_distinct(eval_time)
  if (n_distinct_time != 1) {
    abort(paste0(
      "`eval_time` should have at most 1 unique value. But ", n_distinct_time,
      " was detected."
    ))
  }

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

  brier_survival_impl(truth, estimate, censoring_weights, case_weights, eval_time)
}

brier_survival_impl <- function(truth,
                                estimate,
                                censoring_weights,
                                case_weights,
                                eval_time) {
  surv_time <- truth[, "time"]
  surv_status <- truth[, "status"]

  if (!is.null(case_weights)) {
    norm_const <- sum(case_weights)
    # censoring_weights <- censoring_weights / case_weights
  } else {
    case_weights <- rep(1, length(estimate))
    norm_const <- sum(!survival::is.na.Surv(truth))
  }

  category_1 <- surv_time < eval_time & surv_status == 1
  category_2 <- surv_time >= eval_time

  # (0 - estimate) ^ 2 == estimate ^ 2
  res <- (category_1 * estimate ^ 2 * censoring_weights) +
    (category_2 * (1 - estimate) ^ 2 * censoring_weights)

  res <- res * case_weights
  res <- sum(res, na.rm = TRUE)
  res / norm_const
}
