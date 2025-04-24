#' Matthews correlation coefficient
#'
#' @family class metrics
#' @templateVar fn mcc
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
#' @export
#' @examples
#' library(dplyr)
#' data("two_class_example")
#' data("hpc_cv")
#'
#' # Two class
#' mcc(two_class_example, truth, predicted)
#'
#' # Multiclass
#' # mcc() has a natural multiclass extension
#' hpc_cv |>
#'   filter(Resample == "Fold01") |>
#'   mcc(obs, pred)
#'
#' # Groups are respected
#' hpc_cv |>
#'   group_by(Resample) |>
#'   mcc(obs, pred)
mcc <- function(data, ...) {
  UseMethod("mcc")
}
mcc <- new_class_metric(
  mcc,
  direction = "maximize"
)

#' @export
#' @rdname mcc
mcc.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  class_metric_summarizer(
    name = "mcc",
    fn = mcc_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

#' @export
mcc.table <- function(data, ...) {
  check_table(data)
  estimator <- finalize_estimator(data, metric_class = "mcc")

  # Convert to a double matrix to avoid integer overflow in the binary case
  # and to pass to the C code in the multiclass case.
  # Using `storage.mode()<-` keeps dimensions (as opposed to as.double()).
  data <- as.matrix(data)
  if (!is.double(data)) {
    storage.mode(data) <- "double"
  }

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
mcc_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  abort_if_class_pred(truth)
  estimate <- as_factor_from_class_pred(estimate)

  estimator <- finalize_estimator(truth, metric_class = "mcc")

  check_class_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  data <- yardstick_table(truth, estimate, case_weights = case_weights)
  mcc_table_impl(data, estimator)
}

mcc_table_impl <- function(data, estimator) {
  if (is_binary(estimator)) {
    mcc_binary(data)
  } else {
    mcc_multiclass(data)
  }
}

mcc_binary <- function(data) {
  check_mcc_data(data)

  # mcc() produces identical results regardless of which level is
  # considered the "event", so hardcode to first here
  positive <- pos_val(data, event_level = "first")
  negative <- neg_val(data, event_level = "first")

  tp <- data[positive, positive]
  tn <- data[negative, negative]
  fp <- data[positive, negative]
  fn <- data[negative, positive]
  d1 <- tp + fp
  d2 <- tp + fn
  d3 <- tn + fp
  d4 <- tn + fn

  if (d1 == 0 || d2 == 0 || d3 == 0 || d4 == 0) {
    return(NA)
  }

  ((tp * tn) - (fp * fn)) / sqrt(prod(d1, d2, d3, d4))
}

mcc_multiclass <- function(data) {
  check_mcc_data(data)
  mcc_multiclass_impl(data)
}

mcc_multiclass_impl <- function(C) {
  .Call(yardstick_mcc_multiclass_impl, C)
}

check_mcc_data <- function(data) {
  if (!is.double(data) && !is.matrix(data)) {
    # should not be reachable
    cli::cli_abort(
      "{.arg data} should be a double matrix at this point.",
      .internal = TRUE
    )
  }
  invisible()
}
