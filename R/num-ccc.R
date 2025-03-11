#' Concordance correlation coefficient
#'
#' Calculate the concordance correlation coefficient.
#'
#' [ccc()] is a metric of both consistency/correlation and accuracy,
#' while metrics such as [rmse()] are strictly for accuracy and metrics
#' such as [rsq()] are strictly for consistency/correlation
#'
#' @family numeric metrics
#' @family consistency metrics
#' @family accuracy metrics
#' @templateVar fn ccc
#' @template return
#'
#' @inheritParams rmse
#'
#' @param bias A `logical`; should the biased estimate of variance
#' be used (as is Lin (1989))?
#'
#' @author Max Kuhn
#'
#' @references
#'
#' Lin, L. (1989). A concordance correlation
#'  coefficient to evaluate reproducibility. _Biometrics_, 45 (1),
#'  255-268.
#'
#' Nickerson, C. (1997). A note on "A concordance correlation
#'  coefficient to evaluate reproducibility". _Biometrics_, 53(4),
#'  1503-1507.
#'
#'
#' @template examples-numeric
#'
#' @export
#'
ccc <- function(data, ...) {
  UseMethod("ccc")
}
ccc <- new_numeric_metric(
  ccc,
  direction = "maximize"
)

#' @rdname ccc
#' @export
ccc.data.frame <- function(
  data,
  truth,
  estimate,
  bias = FALSE,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  numeric_metric_summarizer(
    name = "ccc",
    fn = ccc_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights),
    # Extra argument for ccc_impl()
    fn_options = list(bias = bias)
  )
}

#' @export
#' @rdname ccc
ccc_vec <- function(
  truth,
  estimate,
  bias = FALSE,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  ccc_impl(truth, estimate, bias, case_weights)
}

ccc_impl <- function(truth, estimate, bias, case_weights) {
  case_weights <- vec_cast(case_weights, to = double())

  truth_mean <- yardstick_mean(truth, case_weights = case_weights)
  estimate_mean <- yardstick_mean(estimate, case_weights = case_weights)

  truth_variance <- yardstick_var(truth, case_weights = case_weights)
  estimate_variance <- yardstick_var(estimate, case_weights = case_weights)

  covariance <- yardstick_cov(truth, estimate, case_weights = case_weights)

  if (bias) {
    # Assumes `case_weights` are frequency weights so we can generate an
    # appropriate `n`
    if (is.null(case_weights)) {
      n <- length(estimate)
    } else {
      n <- sum(case_weights)
    }

    estimate_variance <- estimate_variance * (n - 1) / n
    truth_variance <- truth_variance * (n - 1) / n
    covariance <- covariance * (n - 1) / n
  }

  numerator <- 2 * covariance
  denominator <- truth_variance +
    estimate_variance +
    (truth_mean - estimate_mean)^2

  numerator / denominator
}
