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
