#' Calculate Metrics for Numeric Outcomes
#'
#' These functions are appropriate for cases where the model
#'  outcome is a number. The root mean squared error (`rmse()`) and
#'  mean absolute error (`mae()`) are error measures that are in the
#'  same units as the original data. The two estimates for the
#'  coefficient of determination, `rsq()` and `rsq_trad()`, differ by
#'  their formula. The former guarantees a value on (0, 1) while the
#'  latter can generate inaccurate values when the model is
#'  non-informative (see the examples). Both are measures of
#'  consistency/correlation and not of accuracy. The concordance
#'  correlation coefficient (`ccc()`) is a measure of both. The mean absolute
#'  percent error (`mape()`) is in relative units.
#'
#' @param data A `data.frame` containing the `truth` and `estimate`
#' columns.
#' @param truth The column identifier for the true results
#'  (that is `numeric`). This should be an unquoted column name although
#'  this argument is passed by expression and supports
#'  [quasiquotation][rlang::quasiquotation] (you can unquote column
#'  names). For `_vec()` functions, a `numeric` vector.
#' @param estimate The column identifier for the predicted
#'  results (that is also `numeric`). As with `truth` this can be
#'  specified different ways but the primary method is to use an
#'  unquoted variable name. For `_vec()` functions, a `numeric` vector.
#' @param bias A `logical`; should the biased estimate of variance
#'  be used for the concordance correlation coefficient (as is
#'  Lin (1989))?
#' @param na.rm A `logical` value indicating whether `NA`
#'  values should be stripped before the computation proceeds.
#' @param ... Not currently used.
#'
#' @inherit sens return
#'
#' @details Note that a value of `Inf` is returned for `mape()` when the
#' observed value is negative.
#'
#' @author Max Kuhn
#'
#' @references Kvalseth. Cautionary note about \eqn{R^2}.
#' American Statistician (1985) vol. 39 (4) pp. 279-285.
#'
#' Lin, L. (1989). A concordance correlation
#'  coefficient to evaluate reproducibility. _Biometrics_, 45 (1),
#'  255–268.
#'
#' Nickerson, C. (1997). A note on "A concordance correlation
#'  coefficient to evaluate reproducibility". _Biometrics_, 53(4),
#'  1503-1507.
#'
#' @keywords manip
#'
#' @examples
#'
#' rmse(solubility_test, truth = solubility, estimate = prediction)
#' mae(solubility_test, truth = solubility, estimate = prediction)
#'
#' rsq(solubility_test, solubility, prediction)
#'
#' set.seed(2291)
#' solubility_test$randomized <- sample(solubility_test$prediction)
#' rsq(solubility_test, solubility, randomized)
#' rsq_trad(solubility_test, solubility, randomized)
#'
#' ccc(solubility_test, solubility, prediction)
#'
#' @export
#' @rdname rmse
rmse <- function(data, ...) {
  UseMethod("rmse")
}

#' @rdname rmse
#' @export
rmse.data.frame <- function(data, truth, estimate, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "rmse",
    metric_fn = rmse_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
#' @rdname rmse
rmse_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  rmse_impl <- function(truth, estimate) {
    sqrt( mean( (truth - estimate) ^ 2) )
  }

  metric_vec_template(
    metric_impl = rmse_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "numeric",
    ...
  )

}

#' @export
#' @rdname rmse
rsq <- function(data, ...) {
  UseMethod("rsq")
}

#' @rdname rmse
#' @export
rsq.data.frame <- function(data, truth, estimate, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "rsq",
    metric_fn = rsq_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
#' @rdname rmse
#' @importFrom stats cor
rsq_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  rsq_impl <- function(truth, estimate) {
    cor(truth, estimate)^2
  }

  metric_vec_template(
    metric_impl = rsq_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "numeric",
    ...
  )

}

#' @export
#' @rdname rmse
rsq_trad <- function(data, ...) {
  UseMethod("rsq_trad")
}

#' @rdname rmse
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
#' @rdname rmse
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


#' @export
#' @rdname rmse
mae <- function(data, ...) {
  UseMethod("mae")
}

#' @rdname rmse
#' @export
mae.data.frame <- function(data, truth, estimate, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "mae",
    metric_fn = mae_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
#' @rdname rmse
mae_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  mae_impl <- function(truth, estimate) {
    mean( abs(truth - estimate) )
  }

  metric_vec_template(
    metric_impl = mae_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "numeric",
    ...
  )

}


#' @export
#' @rdname rmse
mape <- function(data, ...) {
  UseMethod("mape")
}

#' @rdname rmse
#' @export
mape.data.frame <- function(data, truth, estimate, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "mape",
    metric_fn = mape_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
#' @rdname rmse
mape_vec <- function(truth, estimate, na.rm = TRUE, ...) {

  mape_impl <- function(truth, estimate) {
    mean( abs( (truth - estimate) / truth ) ) * 100
  }

  metric_vec_template(
    metric_impl = mape_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "numeric",
    ...
  )

}


#' @export
#' @rdname rmse
ccc <- function(data, ...) {
  UseMethod("ccc")
}

#' @rdname rmse
#' @export
ccc.data.frame <- function(data, truth, estimate, bias = FALSE, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "ccc",
    metric_fn = ccc_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na.rm = na.rm,
    ... = ...,
    # Extra argument for ccc_impl()
    metric_fn_options = list(bias = bias)
  )

}

#' @export
#' @rdname rmse
#' @importFrom stats var
ccc_vec <- function(truth, estimate, bias = FALSE, na.rm = TRUE, ...) {

  ccc_impl <- function(truth, estimate, bias) {

    m_e <- mean(estimate)
    m_t <- mean(truth)
    v_e <- var(estimate)
    v_t <- var(truth)
    cross <- scale(truth, scale = FALSE) *
      scale(estimate, scale = FALSE)
    cross <- mean(cross)

    if (bias) {
      n <- length(estimate)
      v_e <- v_e * (n - 1) / n
      v_t <- v_t * (n - 1) / n
    }

    2 * cross / (v_e + v_t + (m_e - m_t) ^ 2)

  }

  metric_vec_template(
    metric_impl = ccc_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    cls = "numeric",
    ...,
    bias = bias
  )

}
