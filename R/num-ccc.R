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
#' @templateVar metric_fn ccc
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
#'  255–268.
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

class(ccc) <- c("numeric_metric", "function")

#' @rdname ccc
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
#' @rdname ccc
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
