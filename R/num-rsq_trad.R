#' R squared - traditional
#'
#' Calculate the coefficient of determination using the traditional definition
#' of R squared using sum of squares. For a measure of R squared that is
#' strictly between (0, 1), see [rsq()].
#'
#' The two estimates for the
#' coefficient of determination, [rsq()] and [rsq_trad()], differ by
#' their formula. The former guarantees a value on (0, 1) while the
#' latter can generate inaccurate values when the model is
#' non-informative (see the examples). Both are measures of
#' consistency/correlation and not of accuracy.
#'
#'
#' @family numeric metrics
#' @family consistency metrics
#' @templateVar fn rsq_trad
#' @template return
#'
#' @inheritParams rmse
#'
#' @author Max Kuhn
#'
#' @references
#'
#' Kvalseth. Cautionary note about \eqn{R^2}.
#' American Statistician (1985) vol. 39 (4) pp. 279-285.
#'
#' @template examples-numeric
#' @examples
#' # With uninformitive data, the traditional version of R^2 can return
#' # negative values.
#' set.seed(2291)
#' solubility_test$randomized <- sample(solubility_test$prediction)
#' rsq(solubility_test, solubility, randomized)
#' rsq_trad(solubility_test, solubility, randomized)
#'
#' @export
#'
rsq_trad <- function(data, ...) {
  UseMethod("rsq_trad")
}
rsq_trad <- new_numeric_metric(
  rsq_trad,
  direction = "maximize"
)

#' @rdname rsq_trad
#' @export
rsq_trad.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  numeric_metric_summarizer(
    name = "rsq_trad",
    fn = rsq_trad_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname rsq_trad
rsq_trad_vec <- function(
  truth,
  estimate,
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

  rsq_trad_impl(truth, estimate, case_weights)
}

rsq_trad_impl <- function(truth, estimate, case_weights) {
  # Weighted calculation from the following, basically computing `y_bar`,
  # `SS_res`, and `SS_tot` in weighted manners:
  # https://stats.stackexchange.com/questions/83826/is-a-weighted-r2-in-robust-linear-model-meaningful-for-goodness-of-fit-analys/375752#375752
  # https://github.com/scikit-learn/scikit-learn/blob/582fa30a31ffd1d2afc6325ec3506418e35b88c2/sklearn/metrics/_regression.py#L805
  truth_mean <- yardstick_mean(truth, case_weights = case_weights)

  SS_residuals <- yardstick_sum(
    (truth - estimate)^2,
    case_weights = case_weights
  )
  SS_total <- yardstick_sum((truth - truth_mean)^2, case_weights = case_weights)

  1 - (SS_residuals / SS_total)
}
