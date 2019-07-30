#' Matthews correlation coefficient
#'
#' @family class metrics
#' @templateVar metric_fn mcc
#' @template event_first
#' @template return
#'
#' @section Multiclass:
#'
#' `mcc()` has a known multiclass generalization and that is computed
#' automatically if a factor with more than 2 levels is provided. Because
#' of this, no averaging methods are provided.
#'
#' @inheritParams sens
#'
#' @author Max Kuhn
#'
#' @references Giuseppe, J. (2012). "A Comparison of MCC and CEN Error
#' Measures in Multi-Class Prediction". _PLOS ONE_. Vol 7, Iss 8, e41882.
#'
#' @examples
#' # Two class
#' data("two_class_example")
#' mcc(two_class_example, truth, predicted)
#'
#' # Multiclass
#' # mcc() has a natural multiclass extension
#' library(dplyr)
#' data(hpc_cv)
#' hpc_cv %>%
#'   group_by(Resample) %>%
#'   mcc(obs, pred)
#'
#' @export
#'
mcc <- function(data, ...) {
  UseMethod("mcc")
}

class(mcc) <- c("class_metric", "function")

#' @export
#' @rdname mcc
mcc.data.frame <- function(data, truth, estimate,
                           na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "mcc",
    metric_fn = mcc_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    # do not pass dots through
    # (could allow estimator to be set. unwanted!)
  )

}

#' @export
mcc.table <- function(data, ...) {
  check_table(data)
  estimator <- finalize_estimator(data, metric_class = "mcc")

  metric_tibbler(
    .metric = "mcc",
    .estimator = estimator,
    .estimate = mcc_table_impl(data, estimator)
  )

}

#' @export
mcc.matrix <- function(data, ...) {
  data <- as.table(data)
  mcc.table(data)
}

#' @export
#' @rdname mcc
mcc_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, metric_class = "mcc")

  mcc_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate
    )

    mcc_table_impl(xtab, estimator)

  }

  metric_vec_template(
    metric_impl = mcc_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    cls = "factor",
    ...
  )

}

mcc_table_impl <- function(data, estimator) {

  if(is_binary(estimator)) {
    mcc_binary(data)
  } else {
    mcc_multiclass(data)
  }

}

mcc_binary <- function(data) {

  positive <- pos_val(data)
  negative <- neg_val(data)

  data <- as.matrix(data)

  # Convert the matrix to double to avoid integer overflow
  # using `storage.mode()<-` keeps dimensions (as opposed to as.double())
  if (!is.double(data)) {
    storage.mode(data) <- "double"
  }

  tp <- data[positive, positive]
  tn <- data[negative, negative]
  fp <- data[positive, negative]
  fn <- data[negative, positive]
  d1 <- tp + fp
  d2 <- tp + fn
  d3 <- tn + fp
  d4 <- tn + fn

  if (d1 == 0 | d2 == 0 | d3 == 0 | d4 == 0) {
    return(NA)
  }

  ((tp * tn) - (fp * fn)) / sqrt(prod(d1, d2, d3, d4))
}

mcc_multiclass <- function(data) {
  stopifnot(is.table(data))
  mcc_multiclass_cpp(data)
}
