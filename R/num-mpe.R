#' Mean percentage error
#'
#' @description
#' Calculate the mean percentage error. This metric is in _relative
#' units_. It can be used as a measure of the `estimate`'s bias.
#'
#' Note that if _any_ `truth` values are `0`, a value of:
#' `-Inf` (`estimate > 0`), `Inf` (`estimate < 0`), or `NaN` (`estimate == 0`)
#' is returned for `mpe()`.
#'
#' @family numeric metrics
#' @family accuracy metrics
#' @seealso [All numeric metrics][numeric-metrics]
#' @templateVar fn mpe
#' @template return
#'
#' @inheritParams rmse
#'
#' @details
#' MPE is a metric where the optimal value is `r metric_optimal(mpe)`. The
#' output ranges from `r metric_range(mpe)[1]` to `r metric_range(mpe)[2]`, with
#' `r metric_optimal(mpe)` indicating predictions are unbiased.
#'
#' The formula for MPE is:
#'
#' \deqn{\text{MPE} = \frac{100}{n} \sum_{i=1}^{n} \frac{\text{truth}_i - \text{estimate}_i}{\text{truth}_i}}
#'
#' Using this convention, a _positive_ MPE indicates
#' under-prediction (on average, `truth > estimate`) and a _negative_ MPE
#' indicates over-prediction (on average, `estimate > truth`).
#'
#' @author Thomas Bierhance
#'
#' @export
#' @examples
#' # `solubility_test$solubility` has zero values with corresponding
#' # `$prediction` values that are negative. By definition, this causes `Inf`
#' # to be returned from `mpe()`.
#' solubility_test[solubility_test$solubility == 0, ]
#'
#' mpe(solubility_test, solubility, prediction)
#'
#' # We'll remove the zero values for demonstration
#' solubility_test <- solubility_test[solubility_test$solubility != 0, ]
#'
#' # Supply truth and predictions as bare column names
#' mpe(solubility_test, solubility, prediction)
#'
#' library(dplyr)
#'
#' set.seed(1234)
#' size <- 100
#' times <- 10
#'
#' # create 10 resamples
#' solubility_resampled <- bind_rows(
#'   replicate(
#'     n = times,
#'     expr = sample_n(solubility_test, size, replace = TRUE),
#'     simplify = FALSE
#'   ),
#'   .id = "resample"
#' )
#'
#' # Compute the metric by group
#' metric_results <- solubility_resampled |>
#'   group_by(resample) |>
#'   mpe(solubility, prediction)
#'
#' metric_results
#'
#' # Resampled mean estimate
#' metric_results |>
#'   summarise(avg_estimate = mean(.estimate))
mpe <- function(data, ...) {
  UseMethod("mpe")
}
mpe <- new_numeric_metric(
  mpe,
  direction = "zero",
  range = c(-Inf, Inf)
)

#' @rdname mpe
#' @export
mpe.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  numeric_metric_summarizer(
    name = "mpe",
    fn = mpe_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname mpe
mpe_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_bool(na_rm)
  check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  mpe_impl(truth, estimate, case_weights)
}

mpe_impl <- function(truth, estimate, case_weights) {
  error <- (truth - estimate) / truth

  out <- yardstick_mean(error, case_weights = case_weights)
  out <- out * 100
  out
}
