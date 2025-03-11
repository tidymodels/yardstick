#' R squared
#'
#' Calculate the coefficient of determination using correlation. For the
#' traditional measure of R squared, see [rsq_trad()].
#'
#' The two estimates for the
#' coefficient of determination, [rsq()] and [rsq_trad()], differ by
#' their formula. The former guarantees a value on (0, 1) while the
#' latter can generate inaccurate values when the model is
#' non-informative (see the examples). Both are measures of
#' consistency/correlation and not of accuracy.
#'
#' `rsq()` is simply the squared correlation between `truth` and `estimate`.
#'
#' Because `rsq()` internally computes a correlation, if either `truth` or
#' `estimate` are constant it can result in a divide by zero error. In these
#' cases, a warning is thrown and `NA` is returned. This can occur when a model
#' predicts a single value for all samples. For example, a regularized model
#' that eliminates all predictors except for the intercept would do this.
#' Another example would be a CART model that contains no splits.
#'
#' @family numeric metrics
#' @family consistency metrics
#' @templateVar fn rsq
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
#' # A constant `truth` or `estimate` vector results in a warning from
#' # a divide by zero error in the correlation calculation.
#' # `NA` will be returned in these cases.
#' truth <- c(1, 2)
#' estimate <- c(1, 1)
#' rsq_vec(truth, estimate)
#' @export
rsq <- function(data, ...) {
  UseMethod("rsq")
}
rsq <- new_numeric_metric(
  rsq,
  direction = "maximize"
)

#' @rdname rsq
#' @export
rsq.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  numeric_metric_summarizer(
    name = "rsq",
    fn = rsq_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
#' @rdname rsq
rsq_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  rsq_impl(truth, estimate, case_weights)
}

rsq_impl <- function(truth, estimate, case_weights) {
  yardstick_cor(truth, estimate, case_weights = case_weights)^2
}
