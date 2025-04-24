#' Integrated Brier score for right censored data
#'
#' Compute the integrated Brier score for right censored data, which is an
#' overall calculation of model performance for all values of `.eval_time`.
#'
#' @family dynamic survival metrics
#' @templateVar fn brier_survival_integrated
#' @template return-dynamic-survival
#' @details
#'
#' The integrated time-dependent brier score is calculated in an "area under the
#' curve" fashion. The brier score is calculated for each value of `.eval_time`.
#' The area is calculated via the trapezoidal rule. The area is divided by the
#' largest value of `.eval_time` to bring it into the same scale as the
#' traditional brier score.
#'
#' Smaller values of the score are associated with better model performance.
#'
#' This formulation takes survival probability predictions at one or more
#' specific _evaluation times_ and, for each time, computes the Brier score.
#' To account for censoring, inverse probability of censoring weights
#' (IPCW) are used in the calculations.
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
#' @inheritParams brier_survival
#'
#' @author Emil Hvitfeldt
#'
#' @references E. Graf, C. Schmoor, W. Sauerbrei, and M. Schumacher, “Assessment
#' and comparison of prognostic classification schemes for survival data,”
#' Statistics in Medicine, vol. 18, no. 17-18, pp. 2529–2545, 1999.
#'
#' @examplesIf rlang::is_installed(c("tidyr"))
#' library(dplyr)
#'
#' lung_surv |>
#'   brier_survival_integrated(
#'     truth = surv_obj,
#'     .pred
#'   )
#' @export
brier_survival_integrated <- function(data, ...) {
  UseMethod("brier_survival_integrated")
}

brier_survival_integrated <- new_integrated_survival_metric(
  brier_survival_integrated,
  direction = "minimize"
)

#' @rdname brier_survival_integrated
#' @export
brier_survival_integrated.data.frame <- function(
  data,
  truth,
  ...,
  na_rm = TRUE,
  case_weights = NULL
) {
  dynamic_survival_metric_summarizer(
    name = "brier_survival_integrated",
    fn = brier_survival_integrated_vec,
    data = data,
    truth = !!enquo(truth),
    ...,
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname brier_survival_integrated
brier_survival_integrated_vec <- function(
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

  num_eval_times <- get_unique_eval_times(estimate)
  if (num_eval_times < 2) {
    cli::cli_abort(
      "At least 2 evaluation times are required.
      Only {num_eval_times} unique time was given."
    )
  }

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

  brier_survival_integrated_impl(truth, estimate, case_weights)
}

get_unique_eval_times <- function(x) {
  # Since validate_surv_truth_list_estimate() makes sure they are all the same
  length(x[[1]]$.eval_time)
}

brier_survival_integrated_impl <- function(truth, estimate, case_weights) {
  res <- brier_survival_vec(
    truth = truth,
    estimate = estimate,
    na_rm = FALSE,
    case_weights = case_weights
  )

  auc(res$.eval_time, res$.estimate) / max(res$.eval_time)
}
