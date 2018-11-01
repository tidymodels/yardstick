# ------------------------------------------------------------------------------

# Column name extractors

pos_val <- function(xtab, check = TRUE) {
  if (check)
    if (!all(dim(xtab) == 2))
      stop("Only relevant for 2x2 tables", call. = TRUE)

  if (getOption("yardstick.event_first"))
    colnames(xtab)[1]
  else
    colnames(xtab)[2]
}

neg_val <- function(xtab, check = TRUE) {
  if (check)
    if (!all(dim(xtab) == 2))
      stop("Only relevant for 2x2 tables", call. = TRUE)

  if (getOption("yardstick.event_first"))
    colnames(xtab)[2]
  else
    colnames(xtab)[1]
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
  x == "binary"
}

is_micro <- function(x) {
  x == "micro"
}
