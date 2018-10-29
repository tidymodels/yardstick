#' Positive predictive value
#'
#' These functions calculate the [ppv()] (positive predictive value) of a
#' measurement system compared to a reference result (the "truth" or gold standard).
#' Highly related functions are [spec()], [sens()], and [npv()].
#'
#' The positive predictive value ([ppv()]) is defined as the percent of
#' predicted positives that are actually positive while the
#' negative predictive value ([npv()]) is defined as the percent of negative
#' positives that are actually negative.
#'
#' @family class metrics
#' @family sensitivity metrics
#' @templateVar metric_fn ppv
#' @template event_first
#' @template multiclass
#' @template return
#' @template table-positive
#'
#' @inheritParams sens
#'
#' @param prevalence A numeric value for the rate of the
#'  "positive" class of the data.
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
#' @examples
#' # But what if we think that Class 1 only occurs 40% of the time?
#' ppv(two_class_example, truth, predicted, prevalence = 0.40)
#'
#' @export
ppv <- function(data, ...) {
  UseMethod("ppv")
}

class(ppv) <- c("class_metric", "function")

#' @rdname ppv
#' @export
ppv.data.frame <- function(data, truth, estimate,
                           prevalence = NULL,
                           estimator = NULL, na.rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "ppv",
    metric_fn = ppv_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na.rm = na.rm,
    ... = ...,
    metric_fn_options = list(prevalence = prevalence)
  )

}

#' @export
ppv.table <- function(data, prevalence = NULL, estimator = NULL, ...) {

  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "ppv",
    .estimator = estimator,
    .estimate = ppv_table_impl(
      data,
      estimator = estimator,
      prevalence = prevalence
    )
  )

}

#' @export
ppv.matrix <- function(data, prevalence = NULL, estimator = NULL, ...) {

  data <- as.table(data)
  ppv.table(data, prevalence = prevalence, estimator = estimator)

}

#' @export
#' @rdname ppv
ppv_vec <- function(truth, estimate, prevalence = NULL,
                    estimator = NULL, na.rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, estimator)

  ppv_impl <- function(truth, estimate, prevalence) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate,
      na.rm = FALSE,
      ...
    )

    ppv_table_impl(xtab, estimator, prevalence = prevalence)

  }

  metric_vec_template(
    metric_impl = ppv_impl,
    truth = truth,
    estimate = estimate,
    na.rm = na.rm,
    estimator = estimator,
    cls = "factor",
    ...,
    prevalence = prevalence
  )

}

ppv_table_impl <- function(data, estimator, prevalence = NULL) {

  if(is_binary(estimator)) {
    ppv_binary(data, prevalence)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- ppv_multiclass(data, estimator, prevalence)
    weighted.mean(out_vec, w)
  }

}

ppv_binary <- function(data, prevalence = NULL) {

  positive <- pos_val(data)

  if (is.null(prevalence))
    prevalence <- sum(data[, positive]) / sum(data)

  # sens = recall
  sens <- recall_binary(data)
  spec <- spec_binary(data)
  (sens * prevalence) / ((sens * prevalence) + ((1 - spec) * (1 - prevalence)))

}

ppv_multiclass <- function(data, estimator, prevalence = NULL) {

  # ppv should be equal to precision in all cases except when
  # prevalence is explicitely set. In that case, that value
  # is used which alters the result
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

  numer <- .sens_vec * prevalence
  denom <- .sens_vec * prevalence + (1 - .spec_vec) * (1 - prevalence)

  if(any(denom <= 0)) {
    res <- rep(NA_real_, times = nrow(data))
    return(res)
  }

  numer / denom
}
