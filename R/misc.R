# ------------------------------------------------------------------------------

# Column name extractors

pos_val <- function(xtab, event_level) {
  if (is_event_first(event_level)) {
    colnames(xtab)[[1]]
  } else {
    colnames(xtab)[[2]]
  }
}

neg_val <- function(xtab, event_level) {
  if (is_event_first(event_level)) {
    colnames(xtab)[[2]]
  } else {
    colnames(xtab)[[1]]
  }
}

# ------------------------------------------------------------------------------

check_table <- function(x, call = caller_env()) {
  n_col <- ncol(x)
  n_row <- nrow(x)
  if (n_row != n_col) {
    cli::cli_abort(
      "{.arg x} must have equal dimensions.
      {.arg x} has {n_col} columns and {n_row} rows.",
      call = call
    )
  }
  if (!isTRUE(all.equal(rownames(x), colnames(x)))) {
    cli::cli_abort(
      "The table must the same groups in the same order.",
      call = call
    )
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------

is_binary <- function(x) {
  identical(x, "binary")
}

is_micro <- function(x) {
  identical(x, "micro")
}

# ------------------------------------------------------------------------------

quote_and_collapse <- function(x) {
  x <- encodeString(x, quote = "'", na.encode = FALSE)
  paste0(x, collapse = ", ")
}

# ------------------------------------------------------------------------------

is_class_pred <- function(x) {
  inherits(x, "class_pred")
}

as_factor_from_class_pred <- function(x, call) {
  if (!is_class_pred(x)) {
    return(x)
  }

  if (!is_installed("probably")) {
    cli::cli_abort(
      "A {.cls class_pred} input was detected, but the {.pkg probably}
      package isn't installed. Install {.pkg probably} to be able to convert
      {.cls class_pred} to {.cls factor}.",
      call = call
    )
  }
  probably::as.factor(x)
}

abort_if_class_pred <- function(x, call = caller_env()) {
  if (is_class_pred(x)) {
    cli::cli_abort(
      "{.arg truth} should not a {.cls class_pred} object.",
      call = call
    )
  }
  return(invisible(x))
}
# ------------------------------------------------------------------------------

curve_finalize <- function(result, data, class, grouped_class) {
  # Packed `.estimate` curve data frame
  out <- dplyr::pull(result, ".estimate")

  if (!dplyr::is_grouped_df(data)) {
    class(out) <- c(class, class(out))
    return(out)
  }

  group_syms <- dplyr::groups(data)

  # Poor-man's `tidyr::unpack()`
  groups <- dplyr::select(result, !!!group_syms)
  out <- dplyr::bind_cols(groups, out)

  # Curve functions always return a result grouped by original groups
  out <- dplyr::group_by(out, !!!group_syms)

  class(out) <- c(grouped_class, class, class(out))

  out
}

# ------------------------------------------------------------------------------

yardstick_mean <- function(x, ..., case_weights = NULL, na_remove = FALSE) {
  check_dots_empty()

  if (is.null(case_weights)) {
    mean(x, na.rm = na_remove)
  } else {
    case_weights <- vec_cast(case_weights, to = double())
    stats::weighted.mean(x, w = case_weights, na.rm = na_remove)
  }
}

yardstick_sum <- function(x, ..., case_weights = NULL, na_remove = FALSE) {
  check_dots_empty()

  if (is.null(case_weights)) {
    sum(x, na.rm = na_remove)
  } else {
    case_weights <- vec_cast(case_weights, to = double())

    if (na_remove) {
      # Only remove `NA`s found in `x`, copies `stats::weighted.mean()`
      keep <- !is.na(x)
      x <- x[keep]
      case_weights <- case_weights[keep]
    }

    sum(x * case_weights)
  }
}

# ------------------------------------------------------------------------------

yardstick_sd <- function(x, ..., case_weights = NULL) {
  check_dots_empty()

  variance <- yardstick_var(
    x = x,
    case_weights = case_weights
  )

  sqrt(variance)
}

yardstick_var <- function(x, ..., case_weights = NULL) {
  check_dots_empty()

  yardstick_cov(
    truth = x,
    estimate = x,
    case_weights = case_weights
  )
}

yardstick_cov <- function(truth, estimate, ..., case_weights = NULL) {
  check_dots_empty()

  if (is.null(case_weights)) {
    # To always go through `stats::cov.wt()` for consistency
    case_weights <- rep(1, times = length(truth))
  }

  truth <- vec_cast(truth, to = double())
  estimate <- vec_cast(estimate, to = double())
  case_weights <- vec_cast(case_weights, to = double())

  size <- vec_size(truth)
  if (size != vec_size(estimate)) {
    # should be unreachable
    cli::cli_abort(
      "{.arg truth} ({vec_size(truth)}) and
      {.arg estimate} ({vec_size(estimate)}) must be the same size.",
      .internal = TRUE
    )
  }
  if (size != vec_size(case_weights)) {
    # should be unreachable
    cli::cli_abort(
      "{.arg truth} ({vec_size(truth)}) and
      {.arg case_weights} ({vec_size(case_weights)}) must be the same size.",
      .internal = TRUE
    )
  }

  if (size == 0L || size == 1L) {
    # Like `cov(double(), double())` and `cov(0, 0)`,
    # Otherwise `cov.wt()` returns `NaN` or an error.
    return(NA_real_)
  }

  input <- cbind(truth = truth, estimate = estimate)

  cov <- stats::cov.wt(
    x = input,
    wt = case_weights,
    cor = FALSE,
    center = TRUE,
    method = "unbiased"
  )

  cov <- cov$cov

  # 2-column matrix generates 2x2 covariance matrix.
  # All values represent the variance.
  cov[[1, 2]]
}

yardstick_cor <- function(truth, estimate, ..., case_weights = NULL) {
  check_dots_empty()

  if (is.null(case_weights)) {
    # To always go through `stats::cov.wt()` for consistency
    case_weights <- rep(1, times = length(truth))
  }

  truth <- vec_cast(truth, to = double())
  estimate <- vec_cast(estimate, to = double())
  case_weights <- vec_cast(case_weights, to = double())

  size <- vec_size(truth)
  if (size != vec_size(estimate)) {
    # should be unreachable
    cli::cli_abort(
      "{.arg truth} ({vec_size(truth)}) and
      {.arg estimate} ({vec_size(estimate)}) must be the same size.",
      .internal = TRUE
    )
  }
  if (size != vec_size(case_weights)) {
    # should be unreachable
    cli::cli_abort(
      "{.arg truth} ({vec_size(truth)}) and
      {.arg case_weights} ({vec_size(case_weights)}) must be the same size.",
      .internal = TRUE
    )
  }

  if (size == 0L || size == 1L) {
    warn_correlation_undefined_size_zero_or_one()
    return(NA_real_)
  }
  if (vec_unique_count(truth) == 1L) {
    warn_correlation_undefined_constant_truth(truth)
    return(NA_real_)
  }
  if (vec_unique_count(estimate) == 1L) {
    warn_correlation_undefined_constant_estimate(estimate)
    return(NA_real_)
  }

  input <- cbind(truth = truth, estimate = estimate)

  cov <- stats::cov.wt(
    x = input,
    wt = case_weights,
    cor = TRUE,
    center = TRUE,
    method = "unbiased"
  )

  cor <- cov$cor

  # 2-column matrix generates 2x2 correlation matrix.
  # Diagonals are 1s. Off-diagonals are correlations.
  cor[[1, 2]]
}

warn_correlation_undefined_size_zero_or_one <- function() {
  message <- paste0(
    "A correlation computation is required, but the inputs are size zero or ",
    "one and the standard deviation cannot be computed. ",
    "`NA` will be returned."
  )

  warn_correlation_undefined(
    message = message,
    class = "yardstick_warning_correlation_undefined_size_zero_or_one"
  )
}

warn_correlation_undefined_constant_truth <- function(truth) {
  message <- make_correlation_undefined_constant_message(what = "truth")

  warn_correlation_undefined(
    message = message,
    truth = truth,
    class = "yardstick_warning_correlation_undefined_constant_truth"
  )
}

warn_correlation_undefined_constant_estimate <- function(estimate) {
  message <- make_correlation_undefined_constant_message(what = "estimate")

  warn_correlation_undefined(
    message = message,
    estimate = estimate,
    class = "yardstick_warning_correlation_undefined_constant_estimate"
  )
}

make_correlation_undefined_constant_message <- function(what) {
  paste0(
    "A correlation computation is required, but `",
    what,
    "` is constant ",
    "and has 0 standard deviation, resulting in a divide by 0 error. ",
    "`NA` will be returned."
  )
}

warn_correlation_undefined <- function(message, ..., class = character()) {
  cli::cli_warn(
    message = message,
    class = c(class, "yardstick_warning_correlation_undefined"),
    ...
  )
}

# ------------------------------------------------------------------------------

yardstick_quantile <- function(x, probabilities, ..., case_weights = NULL) {
  # When this goes through `quantile()`, that uses `type = 7` by default,
  # which does linear interpolation of modes. `weighted_quantile()` uses a
  # weighted version of what `type = 4` does, which is a linear interpolation
  # of the empirical CDF, so even if you supply `case_weights = 1`, the values
  # will likely differ.

  check_dots_empty()

  if (is.null(case_weights)) {
    stats::quantile(x, probs = probabilities, names = FALSE)
  } else {
    weighted_quantile(x, weights = case_weights, probabilities = probabilities)
  }
}

weighted_quantile <- function(x, weights, probabilities) {
  # For possible use in hardhat. A weighted variant of `quantile(type = 4)`,
  # which does linear interpolation of the empirical CDF.

  x <- vec_cast(x, to = double())
  weights <- vec_cast(weights, to = double())
  probabilities <- vec_cast(probabilities, to = double())

  size <- vec_size(x)
  if (size != vec_size(weights)) {
    cli::cli_abort(
      "{.arg x} ({vec_size(x)}) and {.arg weights} ({vec_size(weights)})
      must have the same size."
    )
  }

  if (any(is.na(probabilities))) {
    cli::cli_abort("{.arg probabilities} can't have missing values.")
  }
  if (any(probabilities > 1 | probabilities < 0)) {
    cli::cli_abort("{.arg probabilities} must be within `[0, 1]`.")
  }

  if (size == 0L) {
    # For compatibility with `quantile()`, since `approx()` requires >=2 points
    out <- rep(NA_real_, times = length(probabilities))
    return(out)
  }
  if (size == 1L) {
    # For compatibility with `quantile()`, since `approx()` requires >=2 points
    out <- rep(x, times = length(probabilities))
    return(out)
  }

  o <- vec_order(x)
  x <- vec_slice(x, o)
  weights <- vec_slice(weights, o)

  weighted_quantiles <- cumsum(weights) / sum(weights)

  interpolation <- stats::approx(
    x = weighted_quantiles,
    y = x,
    xout = probabilities,
    method = "linear",
    rule = 2L
  )

  out <- interpolation$y

  out
}

# ------------------------------------------------------------------------------

yardstick_table <- function(truth, estimate, ..., case_weights = NULL) {
  check_dots_empty()

  abort_if_class_pred(truth)

  if (is_class_pred(estimate)) {
    estimate <- as_factor_from_class_pred(estimate)
  }

  if (!is.factor(truth)) {
    cli::cli_abort(
      "{.arg truth} must be a factor, not {.obj_type_friendly {truth}}.",
      .internal = TRUE
    )
  }
  if (!is.factor(estimate)) {
    cli::cli_abort(
      "{.arg estimate} must be a factor, not {.obj_type_friendly {estimate}}.",
      .internal = TRUE
    )
  }

  levels <- levels(truth)
  n_levels <- length(levels)

  if (!identical(levels, levels(estimate))) {
    cli::cli_abort(
      "{.arg truth} and {.arg estimate} must have the same levels in the same
      order.",
      .internal = TRUE
    )
  }
  if (n_levels < 2) {
    cli::cli_abort(
      "{.arg truth} must have at least 2 factor levels.",
      .internal = TRUE
    )
  }

  # Supply `estimate` first to get it to correspond to the row names.
  # Always return a double matrix for type stability (in particular, we know
  # `mcc()` relies on this for overflow and C code purposes).
  if (is.null(case_weights)) {
    out <- table(Prediction = estimate, Truth = truth)
    out <- unclass(out)
    storage.mode(out) <- "double"
  } else {
    out <- hardhat::weighted_table(
      Prediction = estimate,
      Truth = truth,
      weights = case_weights
    )
  }

  out
}

yardstick_truth_table <- function(truth, ..., case_weights = NULL) {
  # For usage in many of the prob-metric functions.
  # A `truth` table is required for `"macro_weighted"` estimators.
  # Case weights must be passed through to generate correct `"macro_weighted"`
  # results. `"macro"` and `"micro"` don't require case weights for this
  # particular part of the calculation.

  # Modeled after the treatment of `average = "weighted"` in sklearn, which
  # works the same as `"macro_weighted"` here.
  # https://github.com/scikit-learn/scikit-learn/blob/baf828ca126bcb2c0ad813226963621cafe38adb/sklearn/metrics/_base.py#L23

  check_dots_empty()

  abort_if_class_pred(truth)

  if (!is.factor(truth)) {
    # should be unreachable
    cli::cli_abort("{.arg truth} must be a factor.", .internal = TRUE)
  }

  levels <- levels(truth)
  n_levels <- length(levels)

  if (n_levels < 2) {
    # should be unreachable
    cli::cli_abort(
      "{.arg truth} must have at least 2 factor levels.",
      .internal = TRUE
    )
  }

  # Always return a double matrix for type stability
  if (is.null(case_weights)) {
    out <- table(truth, dnn = NULL)
    out <- unclass(out)
    storage.mode(out) <- "double"
  } else {
    out <- hardhat::weighted_table(
      truth,
      weights = case_weights
    )
  }

  # Required to be a 1 row matrix for `get_weights()`
  out <- matrix(out, nrow = 1L)

  out
}
