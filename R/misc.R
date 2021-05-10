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
