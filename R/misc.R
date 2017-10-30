get_col <- function(x, val) {
  call <- match.call()
  if(!any(colnames(x) == val))
    stop("Column `", call[["val"]], "`` is not in `",
         call[["x"]], "`", call. = FALSE)
  getElement(x, val)
}


pos_val <- function(xtab, check = TRUE) {
  if (check)
    if (!all(dim(xtab) == 2))
      stop("Only relavant for 2x2 tables", call. = TRUE)
  
  if (getOption("yardstick.event_first"))
    colnames(xtab)[1]
  else
    colnames(xtab)[2]
}

neg_val <- function(xtab, check = TRUE) {
  if (check)
    if (!all(dim(xtab) == 2))
      stop("Only relavant for 2x2 tables", call. = TRUE)
  
  if (getOption("yardstick.event_first"))
    colnames(xtab)[2]
  else
    colnames(xtab)[2]
}

check_call_vars <- function(x) {
  truth <- x$truth
  estimate <- x$estimate
  if (is.null(truth) || length(truth) > 1)
    stop(
      "Please specifiy a single variable name for the ",
      "column containing the true class in `truth`.",
      call. = FALSE
    )
  if (is.null(estimate) || length(estimate) > 1)
    stop(
      "Please specifiy a single variable name for the ",
      "column containing the predicted class in `estimate`.",
      call. = FALSE
    )
  invisible(NULL)
}

check_table <- function(x) {
  if (!all.equal(nrow(x), ncol(x)))
    stop("the table must have nrow = ncol", call. = FALSE)
  if (!isTRUE(all.equal(rownames(x), colnames(x))))
    stop("the table must the same groups in the same order", call. = FALSE)
  invisible(NULL)
}

# tmp <- as.table(matrix(c(11, 12, 13, 21, 22, 23, 31, 32, 33), byrow = FALSE, ncol = 3))
# use in test
flatten <- function(xtab) {
  p <- ncol(xtab)
  if(nrow(xtab) != p)
    stop("table must have equal dimensions")
  flat <- as.vector(xtab)
  names(flat) <- paste("cell", rep(1:3, each = 3), rep(1:3, 3), sep = "_")
  flat
}
