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
#' @templateVar metric_fn rsq_trad
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

class(rsq_trad) <- c("numeric_metric", "function")

#' @rdname rsq_trad
#' @export
rsq_trad.data.frame <- function(data, truth, estimate, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "rsq_trad",
    metric_fn = rsq_trad_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
#' @rdname rsq_trad
#' @importFrom stats var
rsq_trad_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  rsq_trad_impl <- function(truth, estimate) {
    n <- length(truth)
    ss <- sum( (estimate - truth) ^ 2)
    1 - (ss / ((n - 1) * var(truth)))
  }

  metric_vec_template(
    metric_impl = rsq_trad_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "numeric",
    ...
  )

}
