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
#' @templateVar fn mpe
#' @template return
#'
#' @inheritParams rmse
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
  direction = "zero"
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
