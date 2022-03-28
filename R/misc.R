# ------------------------------------------------------------------------------

# Column name extractors

pos_val <- function(xtab, event_level) {
  if (!all(dim(xtab) == 2)) {
    rlang::abort("Only relevant for 2x2 tables")
  }

  if (is_event_first(event_level)) {
    colnames(xtab)[[1]]
  } else {
    colnames(xtab)[[2]]
  }
}

neg_val <- function(xtab, event_level) {
  if (!all(dim(xtab) == 2)) {
    rlang::abort("Only relevant for 2x2 tables")
  }

  if (is_event_first(event_level)) {
    colnames(xtab)[[2]]
  } else {
    colnames(xtab)[[1]]
  }
}

# ------------------------------------------------------------------------------

check_table <- function(x) {
  if (!identical(nrow(x), ncol(x)))
    stop("the table must have nrow = ncol", call. = FALSE)
  if (!isTRUE(all.equal(rownames(x), colnames(x))))
    stop("the table must the same groups in the same order", call. = FALSE)
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

as_factor_from_class_pred <- function(x) {
  if (!rlang::is_installed("probably")) {
    rlang::abort(paste0(
      "A <class_pred> input was detected, but the probably package ",
      "isn't installed. Install probably to be able to convert <class_pred> ",
      "to <factor>."
    ))
  }
  probably::as.factor(x)
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

yardstick_table <- function(truth, estimate, ..., case_weights = NULL) {
  check_dots_empty()

  if (is_class_pred(truth)) {
    truth <- as_factor_from_class_pred(truth)
  }
  if (is_class_pred(estimate)) {
    estimate <- as_factor_from_class_pred(estimate)
  }

  if (!is.factor(truth)) {
    abort("Internal error: `truth` must be a factor.")
  }
  if (!is.factor(estimate)) {
    abort("Internal error: `estimate` must be a factor.")
  }

  levels <- levels(truth)
  n_levels <- length(levels)

  if (!identical(levels, levels(estimate))) {
    abort("Internal error: `truth` and `estimate` must have the same levels in the same order.")
  }
  if (n_levels < 2) {
    abort("Internal error: `truth` must have at least 2 factor levels.")
  }

  if (is.null(case_weights)) {
    # Typical `table()` case.
    # Supply `estimate` first to get it to correspond to the row names.
    out <- table(estimate, truth, dnn = c("Prediction", "Truth"))
    return(out)
  }

  if (!is.integer(case_weights) && !is.double(case_weights)) {
    abort("Internal error: `case_weights` must be a numeric vector.")
  }

  x <- dplyr::tibble(
    truth = truth,
    estimate = estimate,
    case_weights = case_weights
  )

  # For each unique truth/estimate combination, sum up the case weights.
  # These correspond to the weighted cells of the table.
  # Important to set `.drop = FALSE` to retain empty levels in the table.
  x <- dplyr::group_by(x, truth, estimate, .drop = FALSE)
  x <- dplyr::summarise(x, cells = sum(case_weights), .groups = "drop")
  x <- dplyr::arrange(x, truth, estimate)

  cells <- x[["cells"]]

  out <- matrix(
    cells,
    nrow = n_levels,
    ncol = n_levels,
    dimnames = list(Prediction = levels, Truth = levels)
  )

  class(out) <- "table"

  out
}
