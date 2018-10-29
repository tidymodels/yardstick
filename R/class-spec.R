#' Specificity
#'
#' These functions calculate the [spec()] (specificity) of a measurement system
#' compared to a reference result (the "truth" or gold standard).
#' Highly related functions are [sens()], [ppv()], and [npv()].
#'
#' The specificity measures the proportion of negatives that are correctly
#' identified as negatives. When there are no negative results, specificity
#' is not defined and a value of `NA` is returned.
#'
#' @family class metrics
#' @family sensitivity metrics
#' @templateVar metric_fn spec
#' @template event_first
#' @template multiclass
#' @template return
#' @template table-positive
#'
#' @inheritParams sens
#'
#'
#' @author Max Kuhn
#'
#' @references
#'
#' Altman, D.G., Bland, J.M. (1994) ``Diagnostic tests 1:
#' sensitivity and specificity,'' *British Medical Journal*,
#' vol 308, 1552.
#'
#' @template examples-class
#'
#' @export
spec <-  function(data, ...) {
  UseMethod("spec")
}

class(spec) <- c("class_metric", "function")

#' @export
#' @rdname spec
spec.data.frame <- function(data, truth, estimate,
                            estimator = NULL, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "spec",
    metric_fn = spec_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na.rm = na.rm,
    ... = ...
  )

}

#' @export
spec.table <- function(data, estimator = NULL, ...) {

  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "spec",
    .estimator = estimator,
    .estimate = spec_table_impl(data, estimator)
  )

}

#' @export
spec.matrix <- function(data, estimator = NULL, ...) {

  data <- as.table(data)
  spec.table(data, estimator)

}

#' @export
#' @rdname spec
spec_vec <- function(truth, estimate, estimator = NULL, na.rm = TRUE,...) {

  estimator <- finalize_estimator(truth, estimator)

  spec_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = FALSE
    )

    spec_table_impl(xtab, estimator)

  }

  metric_vec_template(
    metric_impl = spec_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    estimator = estimator,
    cls = "factor",
    ...
  )

}

#' @importFrom stats weighted.mean
spec_table_impl <- function(data, estimator) {

  if(is_binary(estimator)) {
    spec_binary(data)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- spec_multiclass(data, estimator)
    weighted.mean(out_vec, w)
  }

}

spec_binary <- function(data) {

  negative <- neg_val(data)

  numer <- sum(data[negative, negative])
  denom <- sum(data[, negative])
  spec <- ifelse(denom > 0, numer / denom, NA_real_)
  spec

}

spec_multiclass <- function(data, estimator) {

  n <- sum(data)

  tp   <- diag(data)
  tpfp <- rowSums(data)
  tpfn <- colSums(data)
  tn   <- n - (tpfp + tpfn - tp)
  fp   <- tpfp - tp

  numer <- tn
  denom <- tn + fp

  if(any(denom <= 0)) {
    res <- rep(NA_real_, times = nrow(data))
    return(res)
  }

  if(is_micro(estimator)) {
    numer <- sum(numer)
    denom <- sum(denom)
  }

  numer / denom
}
