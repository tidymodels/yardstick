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
#' @family numeric metrics
#' @family consistency metrics
#' @templateVar metric_fn rsq
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
rsq <- function(data, ...) {
  UseMethod("rsq")
}
rsq <- new_numeric_metric(
  rsq,
  direction = "maximize"
)

#' @rdname rsq
#' @export
rsq.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "rsq",
    metric_fn = rsq_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ... = ...
  )

}

#' @export
#' @rdname rsq
#' @importFrom stats cor
rsq_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  rsq_impl <- function(truth, estimate) {
    try_cor(truth, estimate) ^ 2
  }

  metric_vec_template(
    metric_impl = rsq_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )

}
