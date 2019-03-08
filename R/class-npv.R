#' Negative predictive value
#'
#' These functions calculate the [npv()] (negative predictive value) of a
#' measurement system compared to a reference result (the "truth" or gold standard).
#' Highly related functions are [spec()], [sens()], and [ppv()].
#'
#' The positive predictive value ([ppv()]) is defined as the percent of
#' predicted positives that are actually positive while the
#' negative predictive value ([npv()]) is defined as the percent of negative
#' positives that are actually negative.
#'
#' @family class metrics
#' @family sensitivity metrics
#' @templateVar metric_fn npv
#' @template event_first
#' @template multiclass
#' @template return
#' @template table-positive
#'
#' @inheritParams ppv
#'
#' @author Max Kuhn
#'
#' @references
#'
#' Altman, D.G., Bland, J.M. (1994) ``Diagnostic tests 2:
#' predictive values,'' *British Medical Journal*, vol 309,
#' 102.
#'
#' @template examples-class
#'
#' @export
npv <- function(data, ...) {
  UseMethod("npv")
}

class(npv) <- c("class_metric", "function")

#' @rdname npv
#' @export
npv.data.frame <- function(data, truth, estimate,
                           prevalence = NULL,
                           estimator = NULL, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "npv",
    metric_fn = npv_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    ... = ...,
    metric_fn_options = list(prevalence = prevalence)
  )

}

#' @export
npv.table <- function(data, prevalence = NULL, estimator = NULL, ...) {

  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "npv",
    .estimator = estimator,
    .estimate = npv_table_impl(data, estimator, prevalence = prevalence)
  )

}

#' @export
npv.matrix <- function(data, prevalence = NULL, estimator = NULL, ...) {

  data <- as.table(data)
  npv.table(data, prevalence = prevalence, estimator = estimator)

}

#' @export
#' @rdname npv
npv_vec <- function(truth, estimate,
                    prevalence = NULL,
                    estimator = NULL, na_rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, estimator)

  npv_impl <- function(truth, estimate, prevalence) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate
    )

    npv_table_impl(xtab, estimator, prevalence = prevalence)

  }

  metric_vec_template(
    metric_impl = npv_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    cls = "factor",
    ...,
    prevalence = prevalence
  )

}

npv_table_impl <- function(data, estimator, prevalence = NULL) {

  if(is_binary(estimator)) {
    npv_binary(data, prevalence)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- npv_multiclass(data, estimator, prevalence)
    weighted.mean(out_vec, w)
  }
}

npv_binary <- function(data, prevalence = NULL) {

  positive <- pos_val(data)
  negative <- neg_val(data)

  if (is.null(prevalence))
    prevalence <- sum(data[, positive]) / sum(data)

  # recall = sens
  sens <- recall_binary(data)
  spec <- spec_binary(data)
  (spec * (1 - prevalence)) / (((1 - sens) * prevalence) + ((spec) * (1 - prevalence)))

}

npv_multiclass <- function(data, estimator, prevalence = NULL) {

  if (is.null(prevalence)) {
    tpfn     <- colSums(data)
    tptnfpfn <- rep(sum(data), times = nrow(data))

    if (is_micro(estimator)) {
      tpfn       <- sum(tpfn)
      tptnfpfn   <- sum(tptnfpfn)
    }

    prevalence <- tpfn / tptnfpfn
  }

  .sens_vec <- recall_multiclass(data, estimator)
  .spec_vec <- spec_multiclass(data, estimator)

  numer <- .spec_vec * (1 - prevalence)
  denom <- (1 - .sens_vec) * prevalence + .spec_vec * (1 - prevalence)

  denom[denom <= 0] <- NA_real_

  numer / denom
}
