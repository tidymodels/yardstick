#' Kappa
#'
#' Kappa is a similar measure to [accuracy()], but is normalized by
#' the accuracy that would be expected by chance alone and is very useful
#' when one or more classes have large frequency distributions.
#'
#' @family class metrics
#' @templateVar fn kap
#' @template return
#'
#' @section Multiclass:
#'
#' Kappa extends naturally to multiclass scenarios. Because
#' of this, macro and micro averaging are not implemented.
#'
#' @inheritParams sens
#'
#' @param weighting A weighting to apply when computing the scores. One of:
#'   `"none"`, `"linear"`, or `"quadratic"`. Linear and quadratic weighting
#'   penalizes mis-predictions that are "far away" from the true value. Note
#'   that distance is judged based on the ordering of the levels in `truth` and
#'   `estimate`. It is recommended to provide ordered factors for `truth` and
#'   `estimate` to explicitly code the ordering, but this is not required.
#'
#'   In the binary case, all 3 weightings produce the same value, since it is
#'   only ever possible to be 1 unit away from the true value.
#'
#' @author Max Kuhn
#' @author Jon Harmon
#'
#' @references
#'   Cohen, J. (1960). "A coefficient of agreement for nominal
#'   scales". _Educational and Psychological Measurement_. 20 (1): 37-46.
#'
#'   Cohen, J. (1968). "Weighted kappa: Nominal scale agreement provision for
#'   scaled disagreement or partial credit". _Psychological
#'   Bulletin_. 70 (4): 213-220.
#'
#' @export
#' @examples
#' library(dplyr)
#' data("two_class_example")
#' data("hpc_cv")
#'
#' # Two class
#' kap(two_class_example, truth, predicted)
#'
#' # Multiclass
#' # kap() has a natural multiclass extension
#' hpc_cv |>
#'   filter(Resample == "Fold01") |>
#'   kap(obs, pred)
#'
#' # Groups are respected
#' hpc_cv |>
#'   group_by(Resample) |>
#'   kap(obs, pred)
kap <- function(data, ...) {
  UseMethod("kap")
}
kap <- new_class_metric(
  kap,
  direction = "maximize"
)

#' @export
#' @rdname kap
kap.data.frame <- function(
  data,
  truth,
  estimate,
  weighting = "none",
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  class_metric_summarizer(
    name = "kap",
    fn = kap_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights),
    fn_options = list(weighting = weighting)
  )
}

#' @export
kap.table <- function(data, weighting = "none", ...) {
  check_table(data)
  metric_tibbler(
    .metric = "kap",
    .estimator = finalize_estimator(data, metric_class = "kap"),
    .estimate = kap_table_impl(data, weighting = weighting)
  )
}

#' @export
kap.matrix <- function(data, weighting = "none", ...) {
  data <- as.table(data)
  kap.table(data, weighting = weighting)
}

#' @export
#' @rdname kap
kap_vec <- function(
  truth,
  estimate,
  weighting = "none",
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  abort_if_class_pred(truth)
  estimate <- as_factor_from_class_pred(estimate)

  estimator <- finalize_estimator(truth, metric_class = "kap")

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
  kap_table_impl(data, weighting = weighting)
}

kap_table_impl <- function(data, weighting) {
  full_sum <- sum(data)
  row_sum <- rowSums(data)
  col_sum <- colSums(data)
  expected <- outer(row_sum, col_sum) / full_sum

  n_levels <- nrow(data)
  w <- make_weighting_matrix(weighting, n_levels)

  n_disagree <- sum(w * data)
  n_chance <- sum(w * expected)

  1 - n_disagree / n_chance
}

make_weighting_matrix <- function(weighting, n_levels, call = caller_env()) {
  validate_weighting(weighting, call = call)

  if (is_no_weighting(weighting)) {
    # [n_levels x n_levels], 0 on diagonal, 1 on off-diagonal
    w <- matrix(1L, nrow = n_levels, ncol = n_levels)
    diag(w) <- 0L
    return(w)
  }

  if (is_linear_weighting(weighting)) {
    power <- 1L
  } else {
    # quadratic
    power <- 2L
  }

  # [n_levels x n_levels], 0 on diagonal, increasing weighting on off-diagonal
  w <- seq2(0L, n_levels - 1L)
  w <- matrix(w, nrow = n_levels, ncol = n_levels)
  w <- abs(w - t(w))^power

  w
}

# ------------------------------------------------------------------------------

validate_weighting <- function(x, call = caller_env()) {
  check_string(x, arg = "weighting", call = call)

  ok <- is_no_weighting(x) ||
    is_linear_weighting(x) ||
    is_quadratic_weighting(x)

  if (!ok) {
    cli::cli_abort(
      "{.arg weighting} must be {.val none}, {.val linear}, or
       {.val quadratic}, not {.val {x}}.",
      call = call
    )
  }

  invisible(x)
}
is_no_weighting <- function(x) {
  identical(x, "none")
}
is_linear_weighting <- function(x) {
  identical(x, "linear")
}
is_quadratic_weighting <- function(x) {
  identical(x, "quadratic")
}
