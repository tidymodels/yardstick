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
    colnames(xtab)[1]
}

check_call_vars <- function(x) {
  truth <- x$truth
  estimate <- x$estimate
  if (is.null(truth) || length(eval(truth)) > 1)
    stop(
      "Please specifiy a single variable name for the ",
      "column containing the true class in `truth`.",
      call. = FALSE
    )
  if (is.null(estimate) || length(eval(estimate)) > 1)
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

check_factor <- function(x, two = FALSE) {
  if (!is.factor(x))
    stop("A factor is required", call. = FALSE)
  if (two && length(levels(x)) != 2)
    stop("A factor with two levels is required", call. = FALSE)
  if (!two && length(levels(x)) == 1)
    stop("A factor with at least two levels is required", call. = FALSE)
  x
}

check_probs <- function(data, estimate) {
  if (length(setdiff(estimate, names(data))) > 0)
    stop("Some names in `estimate` are not in `data", call. = FALSE)
  num_check <-
    vapply(data[, estimate, drop = FALSE], is.numeric, logical(1))
  if (length(num_check) == 1)
    stop("`estimate` should be at least two columns",
         call. = FALSE)
  if (!all(num_check))
    stop(
      "In this function, `estimate` should be columns for numeric ",
      "scores (such as a class probability).",
      call. = FALSE
    )
  invisible(NULL)
}

# tmp <- as.table(matrix(c(11, 12, 13, 21, 22, 23, 31, 32, 33), byrow = FALSE, ncol = 3))
# use in test
flatten <- function(xtab) {
  p <- ncol(xtab)
  if(nrow(xtab) != p)
    stop("table must have equal dimensions")
  flat <- as.vector(xtab)
  names(flat) <- paste("cell", rep(1:p, p), rep(1:p, each = p), sep = "_")
  flat
}
