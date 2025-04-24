#' Time-Dependent Brier score for right censored data
#'
#' Compute the time-dependent Brier score for right censored data, which is the
#' mean squared error at time point `.eval_time`.
#'
#' @family dynamic survival metrics
#' @templateVar fn brier_survival
#' @template return-dynamic-survival
#' @inheritParams pr_auc
#'
#' @param data A `data.frame` containing the columns specified by `truth` and
#' `...`.
#'
#' @param truth The column identifier for the true survival result (that
#' is created using [survival::Surv()].). This should be an unquoted column name
#' although this argument is passed by expression and supports
#' [quasiquotation][rlang::quasiquotation] (you can unquote column names). For
#' `_vec()` functions, an [survival::Surv()] object.
#'
#' @param ... The column identifier for the survival probabilities this
#' should be a list column of data.frames corresponding to the output given when
#' predicting with [censored](https://censored.tidymodels.org/) model. This
#' should be an unquoted column name although this argument is passed by
#' expression and supports [quasiquotation][rlang::quasiquotation] (you can
#' unquote column names). For `_vec()` functions, the dots are not used.
#'
#' @param estimate A list column of data.frames corresponding to the output
#' given when predicting with [censored](https://censored.tidymodels.org/)
#' model. See the details for more information regarding format.
#'
#' @details
#'
#' This formulation takes survival probability predictions at one or more
#' specific _evaluation times_ and, for each time, computes the Brier score. To
#' account for censoring, inverse probability of censoring weights (IPCW) are
#' used in the calculations.
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
#' Smaller values of the score are associated with better model performance.
#'
#' @author Emil Hvitfeldt
#'
#' @references
#'
#' E. Graf, C. Schmoor, W. Sauerbrei, and M. Schumacher, “Assessment and
#' comparison of prognostic classification schemes for survival data,”
#' _Statistics in Medicine_, vol. 18, no. 17-18, pp. 2529–2545, 1999.
#'
#' @examplesIf rlang::is_installed(c("tidyr"))
#' # example code
#'
#' library(dplyr)
#'
#' lung_surv |>
#'   brier_survival(
#'     truth = surv_obj,
#'     .pred
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
brier_survival.data.frame <- function(
  data,
  truth,
  ...,
  na_rm = TRUE,
  case_weights = NULL
) {
  dynamic_survival_metric_summarizer(
    name = "brier_survival",
    fn = brier_survival_vec,
    data = data,
    truth = !!enquo(truth),
    ...,
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname brier_survival
brier_survival_vec <- function(
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
  } else {
    any_missing <- yardstick_any_missing(
      truth,
      estimate,
      case_weights
    )
    if (any_missing) {
      return(NA_real_)
    }
  }

  dplyr::tibble(estimate) |>
    tidyr::unnest(estimate) |>
    dplyr::group_by(.eval_time) |>
    dplyr::summarize(
      .estimate = brier_survival_impl(
        truth,
        .pred_survival,
        .weight_censored,
        case_weights,
        .eval_time
      )
    )
}

brier_survival_impl <- function(
  truth,
  estimate,
  censoring_weights,
  case_weights,
  eval_time
) {
  surv_time <- .extract_surv_time(truth)
  surv_status <- .extract_surv_status(truth)

  if (!is.null(case_weights)) {
    case_weights <- vec_cast(case_weights, to = double())
    norm_const <- sum(case_weights)
  } else {
    case_weights <- rep(1, length(estimate))
    norm_const <- sum(!survival::is.na.Surv(truth))
  }

  category_1 <- surv_time <= eval_time & surv_status == 1
  category_2 <- surv_time > eval_time

  # (0 - estimate) ^ 2 == estimate ^ 2
  res <- (category_1 * estimate^2 * censoring_weights) +
    (category_2 * (1 - estimate)^2 * censoring_weights)

  res <- res * case_weights
  res <- sum(res, na.rm = TRUE)
  res / norm_const
}
